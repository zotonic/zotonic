%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Bound media runner HTTP responses and transfer lifetimes for every status code.
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
-define(HEADER_LIMIT, 65536).

%% @doc Read protocol responses with a hard body limit for every HTTP status.
%% Dedicated HTTPS connections avoid both redirects and connection-pool blocking.
-spec request(atom(), tuple(), pos_integer()) -> {ok, integer(), binary()} | {error, term()}.
request(Method, Request, Timeout) ->
    bounded(fun() ->
        with_response(Method, Request, Timeout, fun(Status, Headers, Socket, Buffer) ->
            Parts = read_body(Socket, Status, Headers, Buffer, ?RESPONSE_LIMIT,
                fun(Data, Acc) -> [Data | Acc] end, []),
            {ok, Status, iolist_to_binary(lists:reverse(Parts))}
        end)
    end, Timeout).

%% @doc Stream known-size outputs; reject HTTP errors before reading their bodies.
-spec download(binary(), list(), file:filename_all(), non_neg_integer(), pos_integer()) ->
    {ok, non_neg_integer(), binary()} | {error, term()}.
download(Url, Headers, Temp, Size, Timeout) ->
    bounded(fun() ->
        with_response(get, {Url, Headers, <<>>, <<>>}, Timeout, fun
            (200, ResponseHeaders, Socket, Buffer) ->
                Size = binary_to_integer(header(<<"content-length">>, ResponseHeaders)),
                undefined = header(<<"content-range">>, ResponseHeaders),
                undefined = header(<<"content-encoding">>, ResponseHeaders),
                undefined = header(<<"transfer-encoding">>, ResponseHeaders),
                {ok, Fd} = file:open(Temp, [write, exclusive, raw, binary]),
                try
                    ok = file:change_mode(Temp, 8#600),
                    Write = fun(Data, Hash) ->
                        ok = file:write(Fd, Data),
                        crypto:hash_update(Hash, Data)
                    end,
                    Hash = read_body(Socket, 200, ResponseHeaders, Buffer, Size,
                        Write, crypto:hash_init(sha256)),
                    ok = file:sync(Fd),
                    {ok, Size, binary:encode_hex(crypto:hash_final(Hash), lowercase)}
                after file:close(Fd) end;
            (Status, _, _, _) -> {error, {http_status, Status}}
        end)
    end, Timeout).

with_response(Method, {Url, Headers, Type, Body}, Timeout, Receive) ->
    #{scheme := <<"https">>, host := Host} = Uri = uri_string:parse(z_convert:to_binary(Url)),
    Port = maps:get(port, Uri, 443),
    Path = case maps:get(path, Uri, <<>>) of <<>> -> <<"/">>; P -> P end,
    Target = case maps:find(query, Uri) of
        {ok, Query} -> <<Path/binary, "?", Query/binary>>;
        error -> Path
    end,
    nomatch = re:run(Target, <<"[\\x00-\\x20\\x7f]">>),
    HostName = case binary:match(Host, <<":">>) of
        nomatch -> Host;
        _ -> <<"[", Host/binary, "]">>
    end,
    HostHeader = case Port of
        443 -> HostName;
        _ -> <<HostName/binary, ":", (integer_to_binary(Port))/binary>>
    end,
    H0 = maps:from_list([{string:lowercase(z_convert:to_binary(K)), z_convert:to_binary(V)}
        || {K, V} <- Headers]),
    Length = case maps:find(<<"content-length">>, H0) of
        {ok, L} -> L;
        error -> integer_to_binary(iolist_size(Body))
    end,
    H = H0#{<<"host">> => HostHeader, <<"connection">> => <<"close">>,
        <<"content-type">> => z_convert:to_binary(Type), <<"content-length">> => Length},
    lists:foreach(fun({K, V}) ->
        nomatch = binary:match(K, [<<"\r">>, <<"\n">>]),
        nomatch = binary:match(V, [<<"\r">>, <<"\n">>])
    end, maps:to_list(H)),
    Ssl = proplists:get_value(ssl, z_media_runner_protocol:http_options(Timeout)),
    case ssl:connect(binary_to_list(Host), Port,
            [binary, {active, false}, {packet, raw}, {send_timeout, Timeout},
                {send_timeout_close, true} | Ssl], min(5000, Timeout)) of
        {ok, Socket} ->
            try
                ok = ssl:send(Socket, cow_http:request(string:uppercase(atom_to_binary(Method)),
                    Target, 'HTTP/1.1', maps:to_list(H))),
                ok = send_body(Socket, Body),
                {Status, ResponseHeaders, Buffer} = headers(Socket, <<>>, 0),
                Receive(Status, ResponseHeaders, Socket, Buffer)
            after ssl:close(Socket) end;
        {error, _} = Error -> Error
    end.

send_body(Socket, {Next, State}) when is_function(Next, 1) ->
    case Next(State) of
        eof -> ok;
        {ok, Data, NewState} ->
            ok = ssl:send(Socket, Data),
            send_body(Socket, {Next, NewState})
    end;
send_body(Socket, Data) -> ssl:send(Socket, Data).

headers(Socket, Buffer, Informational) when Informational < 8 ->
    case binary:match(Buffer, <<"\r\n\r\n">>) of
        {End, 4} when End + 4 =< ?HEADER_LIMIT ->
            {_, Status, _, Rest} = cow_http:parse_status_line(Buffer),
            {Headers, Body} = cow_http:parse_headers(Rest),
            case Status of
                N when N >= 100, N < 200 -> headers(Socket, Body, Informational + 1);
                _ -> {Status, Headers, Body}
            end;
        nomatch when byte_size(Buffer) < ?HEADER_LIMIT ->
            headers(Socket, <<Buffer/binary, (recv(Socket))/binary>>, Informational);
        _ -> throw(response_headers_too_large)
    end;
headers(_, _, _) -> throw(invalid_response).

%% Read raw TLS records, never a peer-specified number of bytes. Cowlib yields
%% partial chunks immediately, even when the declared chunk is several GB.
read_body(Socket, Status, Headers, Buffer, Limit, Consume, Acc) ->
    Mode = case {Status, header(<<"transfer-encoding">>, Headers), header(<<"content-length">>, Headers)} of
        {204, _, _} -> {identity, {0, 0}};
        {304, _, _} -> {identity, {0, 0}};
        {_, undefined, undefined} -> close;
        {_, undefined, L} ->
            Size = binary_to_integer(L),
            case Size >= 0 andalso Size =< Limit of
                true -> {identity, {0, Size}};
                false -> throw(response_too_large)
            end;
        {_, TE, undefined} ->
            <<"chunked">> = string:lowercase(TE),
            {chunked, {0, 0}}
    end,
    body(Socket, Mode, Buffer, Limit, Consume, Acc).

body(Socket, close, Buffer, Left, Consume, Acc) ->
    Next = consume(Buffer, Left, Consume, Acc),
    case ssl:recv(Socket, 0) of
        {ok, Data} -> body(Socket, close, Data, Left - byte_size(Buffer), Consume, Next);
        {error, closed} -> Next;
        {error, Reason} -> throw(Reason)
    end;
body(Socket, {Kind, State} = Mode, Buffer, Left, Consume, Acc) ->
    Decoded = case Kind of
        identity -> cow_http_te:stream_identity(Buffer, State);
        chunked -> cow_http_te:stream_chunked(Buffer, State)
    end,
    case Decoded of
        {done, _, _} -> Acc;
        {done, Data, _, _} -> consume(Data, Left, Consume, Acc);
        more when byte_size(Buffer) < ?HEADER_LIMIT ->
            body(Socket, Mode, <<Buffer/binary, (recv(Socket))/binary>>, Left, Consume, Acc);
        more -> throw(response_headers_too_large);
        {more, Data, NewState} ->
            continue_body(Socket, Kind, NewState, Data, <<>>, Left, Consume, Acc);
        {more, Data, Rest, NewState} when is_binary(Rest), byte_size(Rest) =< ?HEADER_LIMIT ->
            continue_body(Socket, Kind, NewState, Data, Rest, Left, Consume, Acc);
        {more, Data, Remaining, NewState} when is_integer(Remaining) ->
            continue_body(Socket, Kind, NewState, Data, <<>>, Left, Consume, Acc);
        _ -> throw(invalid_response)
    end.

continue_body(Socket, Kind, State, Data, Rest, Left, Consume, Acc) ->
    Next = consume(Data, Left, Consume, Acc),
    body(Socket, {Kind, State}, <<Rest/binary, (recv(Socket))/binary>>,
        Left - byte_size(Data), Consume, Next).

consume(Data, Left, Consume, Acc) when byte_size(Data) =< Left -> Consume(Data, Acc);
consume(_, _, _, _) -> throw(response_too_large).

recv(Socket) ->
    case ssl:recv(Socket, 0) of
        {ok, Data} -> Data;
        {error, Reason} -> throw(Reason)
    end.

header(Name, Headers) ->
    case [V || {K, V} <- Headers, K =:= Name] of
        [] -> undefined;
        [Value] -> Value
    end.

%% A dedicated socket owner gives every transfer an overall deadline, including
%% uploads and slow trickles. Process exit closes the socket; aliases drop late replies.
bounded(Fun, Timeout) ->
    Alias = alias([reply]),
    {Pid, Monitor} = spawn_monitor(fun() ->
        Result = try Fun() catch
            throw:Reason -> {error, Reason};
            _:_ -> {error, invalid_response}
        end,
        Alias ! {Alias, Result}
    end),
    try
        receive
            {Alias, Result} ->
                demonitor(Monitor, [flush]),
                Result;
            {'DOWN', Monitor, process, Pid, _} -> {error, connection_closed}
        after Timeout ->
            exit(Pid, kill),
            receive {'DOWN', Monitor, process, Pid, _} -> ok end,
            {error, timeout}
        end
    after unalias(Alias) end.
