%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Communicate with clamav server, scan file or binary data.

%% Copyright 2019 Marc Worrell
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

-module(z_clamav).

-export([
    ping/0,
    scan_file/1,
    scan/1,
    ip_port/0,
    max_size/0
    ]).

-define(CLAMAV_IP, "127.0.0.1").
-define(CLAMAV_PORT, 3310).
-define(CLAMAV_MAX_SIZE, 25*1024*1024).  % Default for StreamMaxLength clamav config
-define(CLAMAV_CHUNK_SIZE, 65536).
-define(CLAMAV_RECV_TIMEOUT, 30000).


-spec ip_port() -> {string(), integer()}.
ip_port() ->
    ClamIP = z_config:get(clamav_ip, ?CLAMAV_IP),
    ClamPort = z_config:get(clamav_port, ?CLAMAV_PORT),
    {ClamIP, ClamPort}.

-spec max_size() -> integer().
max_size() ->
    z_config:get(clamav_max_size, ?CLAMAV_MAX_SIZE).

-spec ping() -> pong | pang.
ping() ->
    case do_clam(<<"PING\n">>, fun(_) -> ok end) of
        {ok, <<"PONG\n">>} -> pong;
        _ -> pang
    end.

-spec scan_file( file:filename() ) -> ok | {error, noclamav | infected | av_sizelimit | term() }.
scan_file(Filename) ->
    MaxSize = max_size(),
    case filelib:file_size(Filename) of
        Size when Size < MaxSize ->
            case file:open(Filename, [ read, raw, binary ]) of
                {ok, Fd} ->
                    try
                        F = fun(Socket) -> send_file(Socket, Fd) end,
                        handle_result(do_clam(<<"zINSTREAM",0>>, F))
                    after
                        file:close(Fd)
                    end;
                {error, _} = Error ->
                    Error
            end;
        _TooBig ->
            {error, av_sizelimit}
    end.

-spec scan( binary() ) -> ok | {error, noclamav | infected}.
scan( Data ) when is_binary(Data) ->
    MaxSize = max_size(),
    case size(Data) of
        Size when Size < MaxSize ->
            F = fun(Socket) -> send_chunks(Socket, chop(Data, [])) end,
            handle_result(do_clam(<<"zINSTREAM",0>>, F));
        _Size ->
            {error, av_sizelimit}
    end.

%% @doc Send a command to clamd, return the reply.
-spec do_clam( binary(), function() ) -> {ok, binary()} | {error, term()}.
do_clam(Command, DataFun) ->
    {ClamIP, ClamPort} = ip_port(),
    ConnectOptions = [
        binary,
        {packet, 0},
        {active, true}
    ],
    case gen_tcp:connect(ClamIP, ClamPort, ConnectOptions) of
        {ok, Socket} ->
            ok = gen_tcp:send(Socket, Command),
            _ = DataFun(Socket),
            Result = recv(Socket, <<>>),
            _ = gen_tcp:close(Socket),
            Result;
        {error, _} = Error ->
            lager:info("ClamAV: could not connect on ~p:~p ~p", [ ClamIP, ClamPort, Error ]),
            Error
    end.

%% @doc Check on the result of clamav
handle_result({ok, <<"INSTREAM size limit exceeded", _/binary>>}) ->
    {error, sizelimit};
handle_result({ok, <<"stream: OK", _/binary>>}) ->
    ok;
handle_result({ok, <<"stream: ", Msg/binary>>}) ->
    case binary:match(Msg, <<" FOUND">>) of
        nomatch ->
            lager:info("ClamAV: daemon returned unknown message: ~p", [ Msg ]),
            {error, unknown};
        {_, _} ->
            {error, infected}
    end;
handle_result({error, _} = Error) ->
    Error.

recv(Socket, Acc) ->
    receive
        {tcp, Socket, Data} ->
            recv(Socket, <<Acc/binary, Data/binary>>);
        {tcp_closed, Socket} when Acc =:= <<>> ->
            {error, closed};
        {tcp_closed, Socket} ->
            {ok, Acc}
    after ?CLAMAV_RECV_TIMEOUT ->
        {error, timeout}
    end.

send_file(Socket, Fd) ->
    case file:read(Fd, ?CLAMAV_CHUNK_SIZE) of
        {ok, Data} ->
            Size = size(Data),
            case gen_tcp:send(Socket, <<Size:32/big, Data/binary>>) of
                ok ->
                    case Size of
                        ?CLAMAV_CHUNK_SIZE ->
                            send_file(Socket, Fd);
                        _ ->
                            gen_tcp:send(Socket, <<0:32/big>>)
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

send_chunks(Socket, []) ->
    gen_tcp:send(Socket, <<0:32/big>>);
send_chunks(Socket, [ Data | Rest ]) ->
    Size = size(Data),
    case gen_tcp:send(Socket, <<Size:32/big, Data/binary>>) of
        ok -> send_chunks(Socket, Rest);
        {error, _} = Error -> Error
    end.

chop(<<>>, Acc) ->
    lists:reverse(Acc);
chop(<<Bin:?CLAMAV_CHUNK_SIZE/binary, Rest/binary>>, Acc) ->
    chop(Rest, [Bin | Acc]);
chop(Data, Acc) ->
    lists:reverse([ Data | Acc ]).

