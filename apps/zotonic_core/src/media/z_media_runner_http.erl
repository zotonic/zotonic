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

-export([request/3, download/5]).

-define(RESPONSE_LIMIT, 65536).

%% @doc Send a streaming upload and preserve the HTTP status code.
%% Trusted peers send small control/error bodies; httpc buffers these in memory.
-spec request(atom(), tuple(), pos_integer()) -> {ok, integer(), binary()} | {error, term()}.
request(Method, Request, Timeout) ->
    with_request(Method, Request, [], Timeout, fun(Alias, Id, Deadline) ->
        receive
            {Alias, {Id, {{_, Status, _}, _, Body}}} when byte_size(Body) =< ?RESPONSE_LIMIT ->
                {ok, Status, Body};
            {Alias, {Id, {{_, _, _}, _, _}}} ->
                {error, response_too_large};
            {Alias, {Id, {error, Reason}}} ->
                {error, Reason}
        after remaining(Deadline) ->
            {error, timeout}
        end
    end).

%% @doc Stream an exact-size result to a private file while calculating its hash.
-spec download(binary(), list(), file:filename_all(), non_neg_integer(), pos_integer()) ->
    {ok, non_neg_integer(), binary()} | {error, term()}.
download(Url, Headers, Temp, Size, Timeout) ->
    Request = {z_convert:to_list(Url), Headers},
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
            {ok, Size, binary:encode_hex(crypto:hash_final(Hash), lowercase)};
        {Alias, {Id, stream_end, _}} ->
            {error, invalid_response};
        {Alias, {Id, {error, Reason}}} ->
            {error, Reason}
    after remaining(Deadline) ->
        {error, timeout}
    end.

%% Per-request socket options make httpc open a dedicated connection. This avoids
%% its default two-session pool and keep-alive queues: large transfers must not
%% delay submissions or callbacks. Do not change the shared default profile.
%% The receiver alias drops late replies after cancellation; an absolute deadline
%% also bounds streams whose peer keeps sending small amounts of data.
with_request(Method, Request, Options, Timeout, Receive) ->
    Alias = alias(),
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    HttpOptions = z_media_runner_protocol:http_options(Timeout),
    RequestOptions = [
        {sync, false},
        {receiver, fun(Reply) -> Alias ! {Alias, Reply} end},
        {socket_opts, [{send_timeout, Timeout}, {send_timeout_close, true}]}
        | Options
    ],
    try
        case httpc:request(Method, Request, HttpOptions, RequestOptions) of
            {ok, Id} ->
                try
                    Receive(Alias, Id, Deadline)
                after
                    httpc:cancel_request(Id)
                end;
            {error, _} = Error ->
                Error
        end
    catch
        _:_ ->
            {error, invalid_response}
    after
        unalias(Alias),
        flush(Alias)
    end.

remaining(Deadline) ->
    max(0, Deadline - erlang:monotonic_time(millisecond)).

flush(Alias) ->
    receive
        {Alias, _} ->
            flush(Alias)
    after 0 ->
        ok
    end.
