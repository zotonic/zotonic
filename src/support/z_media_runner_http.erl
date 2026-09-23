%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Transfer media runner requests and files over HTTPS using OTP httpc.
%% @end

%% Copyright 2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_media_runner_http).

-export([request/3, request/4, download/5]).

-define(RESPONSE_LIMIT, 65536).

%% @doc Send a streaming upload and preserve the HTTP status code.
%% Stream successful control responses with a bounded accumulator. OTP httpc
%% buffers non-streamed status/error bodies; these are checked on receipt.
-spec request(atom(), tuple(), pos_integer()) -> {ok, integer(), binary()} | {error, term()}.
request(Method, Request, Timeout) -> request(Method, Request, Timeout, ?RESPONSE_LIMIT).

request(Method, Request, Timeout, Limit) ->
    with_request(Method, Request, [{stream, {self, once}}], Timeout, fun(Alias, Id, Deadline) ->
        receive
            {Alias, {Id, stream_start, Headers, Handler}} ->
                undefined = proplists:get_value("content-range", Headers),
                control_stream(Alias, Id, Handler, Deadline, Limit, 0, []);
            {Alias, {Id, {{_, Status, _}, _, Body}}} when byte_size(Body) =< Limit ->
                {ok, Status, Body};
            {Alias, {Id, {{_, _, _}, _, _}}} ->
                {error, response_too_large};
            {'DOWN', _, process, _, _} ->
                {error, cancelled};
            {Alias, {Id, {error, Reason}}} ->
                {error, Reason}
        after remaining(Deadline) ->
            {error, timeout}
        end
    end).

control_stream(Tag, Id, Handler, Deadline, Limit, Size, Acc) ->
    httpc:stream_next(Handler),
    receive
        {Tag, {Id, stream, Data}} when Size + byte_size(Data) =< Limit ->
            control_stream(Tag, Id, Handler, Deadline, Limit, Size + byte_size(Data), [Data | Acc]);
        {Tag, {Id, stream, _}} -> {error, response_too_large};
        {Tag, {Id, stream_end, _}} -> {ok, 200, iolist_to_binary(lists:reverse(Acc))};
        {Tag, {Id, {error, Reason}}} -> {error, Reason};
        {'DOWN', _, process, _, _} -> {error, cancelled}
    after remaining(Deadline) -> {error, timeout}
    end.

%% @doc Stream an exact-size result to a private file while calculating its hash.
-spec download(binary(), list(), file:filename_all(), non_neg_integer(), pos_integer()) ->
    {ok, non_neg_integer(), binary()} | {error, term()}.
download(Url, Headers, Temp, Size, Timeout) ->
    Request = {binary_to_list(Url), Headers},
    with_request(get, Request, [{stream, {self, once}}], Timeout,
        fun(Alias, Id, Deadline) ->
            receive
                {Alias, {Id, stream_start, ResponseHeaders, Handler}} ->
                    %% httpc streams 200 and 206. A partial response is invalid,
                    %% as are transformed or chunked representations of a result.
                    Size = list_to_integer(proplists:get_value("content-length", ResponseHeaders)),
                    undefined = proplists:get_value("content-range", ResponseHeaders),
                    undefined = proplists:get_value("content-encoding", ResponseHeaders),
                    undefined = proplists:get_value("transfer-encoding", ResponseHeaders),
                    {ok, Fd} = file:open(Temp, [write, exclusive, raw, binary]),
                    try
                        ok = file:change_mode(Temp, 8#600),
                        stream(Alias, Id, Handler, Deadline, Fd, Size, 0, crypto:hash_init(sha256))
                    after
                        file:close(Fd)
                    end;
                {Alias, {Id, {{_, Status, _}, _, _}}} ->
                    {error, {http_status, Status}};
                {'DOWN', _, process, _, _} ->
                    {error, cancelled};
                {Alias, {Id, {error, Reason}}} ->
                    {error, Reason}
            after remaining(Deadline) ->
                {error, timeout}
            end
        end).

stream(Alias, Id, Handler, Deadline, Fd, Size, Received, Hash) ->
    httpc:stream_next(Handler),
    receive
        {Alias, {Id, stream, Data}} when Received + byte_size(Data) =< Size ->
            ok = file:write(Fd, Data),
            stream(Alias, Id, Handler, Deadline, Fd, Size,
                Received + byte_size(Data), crypto:hash_update(Hash, Data));
        {Alias, {Id, stream, _}} ->
            {error, response_too_large};
        {Alias, {Id, stream_end, _}} when Received =:= Size ->
            ok = file:sync(Fd),
            {ok, Size, z_media_runner_protocol:hex(crypto:hash_final(Hash))};
        {Alias, {Id, stream_end, _}} ->
            {error, invalid_response};
        {'DOWN', _, process, _, _} ->
            {error, cancelled};
        {Alias, {Id, {error, Reason}}} ->
            {error, Reason}
    after remaining(Deadline) ->
        {error, timeout}
    end.

%% Each request owns a short-lived receiver. Replies sent after cancellation
%% disappear with that process; OTP 22 has no process aliases.
with_request(Method, Request, Options, Timeout, Receive) ->
    Parent = self(),
    Ref = make_ref(),
    {Pid, Monitor} = spawn_monitor(fun() ->
        monitor(process, Parent),
        Reply = request_worker(Method, Request, Options, Timeout, Receive),
        Parent ! {Ref, Reply}
    end),
    receive
        {Ref, Reply} -> demonitor(Monitor, [flush]), Reply;
        {'DOWN', Monitor, process, Pid, _} -> {error, request_failed}
    after Timeout + 1000 ->
        exit(Pid, kill),
        receive {'DOWN', Monitor, process, Pid, _} -> ok end,
        receive {Ref, _} -> ok after 0 -> ok end,
        {error, timeout}
    end.

request_worker(Method, Request, Options, Timeout, Receive) ->
    Receiver = self(),
    Tag = make_ref(),
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    RequestOptions = [{sync, false}, {body_format, binary},
        {receiver, fun(Reply) -> Receiver ! {Tag, Reply} end},
        {socket_opts, [{send_timeout, Timeout}, {send_timeout_close, true}]} | Options],
    try httpc:request(Method, Request, z_media_runner_protocol:http_options(Timeout), RequestOptions) of
        {ok, Id} ->
            try Receive(Tag, Id, Deadline) after httpc:cancel_request(Id) end;
        Error -> Error
    catch _:_ -> {error, invalid_response}
    end.

remaining(Deadline) -> max(0, Deadline - erlang:monotonic_time(millisecond)).
