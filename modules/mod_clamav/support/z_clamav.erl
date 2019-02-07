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


%% TODO: check for clamav sending 'INSTREAM size limit exceeded' and closing the connection

-module(z_clamav).

-export([
    ping/0,
    scan_file/1,
    scan/1
    ]).

-define(CLAMAV_IP, "192.168.1.124").
-define(CLAMAV_PORT, 3310).
-define(CLAMAV_CHUNK_SIZE, 65536).

-spec ping() -> pong | pang.
ping() ->
    case do_clam(<<"PING\n">>, fun(_) -> ok end) of
        {ok, <<"PONG\n">>} -> pong;
        _ -> pang
    end.

-spec scan_file( filename:filename() ) -> ok | {error, noclamav | infected | term() }.
scan_file(Filename) ->
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
    end.

-spec scan( binary() ) -> ok | {error, noclamav | infected}.
scan( Data ) ->
    F = fun(Socket) -> send_chunks(Socket, chop(Data, [])) end,
    handle_result(do_clam(<<"zINSTREAM",0>>, F)).

%% @doc Send a command to clamd, return the reply.
-spec do_clam( binary(), function() ) -> {ok, binary()} | {error, term()}.
do_clam(Command, DataFun) ->
    ClamIP = z_config:get(clamav_ip, ?CLAMAV_IP),
    ClamPort = z_config:get(clamav_port, ?CLAMAV_PORT),
    case gen_tcp:connect(ClamIP, ClamPort, [binary, {packet, 0}, {active, false}]) of
        {ok, Socket} ->
            ok = gen_tcp:send(Socket, Command),
            DataFun(Socket),
            Result = recv(Socket, <<>>),
            _ = gen_tcp:close(Socket),
            Result;
        {error, _} = Error ->
            lager:info("ClamAV: could not connect on ~p ~p", [ ClamIP, ClamPort, Error ]),
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
            lager:info("ClamAV returned unknown message: ~p", [ Msg ]),
            {error, unknown};
        {_, _} ->
            {error, infected}
    end;
handle_result({error, _} = Error) ->
    Error.

recv(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, B} ->
            recv(Socket, <<Acc/binary, B/binary>>);
        {error, closed} ->
            {ok, Acc};
        {error, _} = Error ->
            Error
    end.

send_file(Socket, Fd) ->
    case file:read(Fd, ?CLAMAV_CHUNK_SIZE) of
        {ok, Data} ->
            Size = size(Data),
            ok = gen_tcp:send(Socket, <<Size:32/big, Data/binary>>),
            case Size of
                ?CLAMAV_CHUNK_SIZE ->
                    send_file(Socket, Fd);
                _ ->
                    ok = gen_tcp:send(Socket, <<0:32/big>>)
            end;
        {error, _} = Error ->
            Error
    end.

send_chunks(Socket, []) ->
    ok = gen_tcp:send(Socket, <<0:32/big>>);
send_chunks(Socket, [ Data | Rest ]) ->
    Size = size(Data),
    ok = gen_tcp:send(Socket, <<Size:32/big, Data/binary>>),
    send_chunks(Socket, Rest).

chop(<<>>, Acc) ->
    lists:reverse(Acc);
chop(<<Bin:?CLAMAV_CHUNK_SIZE/binary, Rest/binary>>, Acc) ->
    chop(Rest, [Bin | Acc]);
chop(Data, Acc) ->
    lists:reverse([ Data | Acc ]).

