%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell, 2011, Loïc Hoguin <essen@dev-extend.eu>
%% @doc WebSocket draft-ietf-hybi-thewebsocketprotocol-17 (Chrome 16)

%% Major parts of the protocol handling are copied from Cowboy.
%% https://github.com/extend/cowboy/blob/master/src/cowboy_http_websocket.erl
%% Cowboy is Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
%% And released under the MIT license.

%% Copyright 2012 Marc Worrell
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

-module(z_websocket_hybi17).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    start/2,
    
    handle_data/3,
    receive_loop/3,
    send_loop/2
]).

-include_lib("zotonic.hrl").

% Send a ping every 30 seconds.
-define(PING_TIMEOUT, 30000).


start(ReqData, Context) ->
    Socket = webmachine_request:socket(ReqData),
    WsKey = z_string:trim(z_context:get_req_header("sec-websocket-key", Context)),
    Accept = base64:encode(crypto:sha(WsKey++"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")),

    %% Send the handshake
    Data = ["HTTP/1.1 101 Switching Protocols", 13, 10,
            "Upgrade: websocket", 13, 10,
            "Connection: Upgrade", 13, 10,
            "Sec-WebSocket-Accept: ", Accept, 13, 10,
            13, 10
            ],
    ok = send(Socket, Data),
    spawn_link(fun() -> start_send_loop(Socket, Context) end),
    receive_loop(<<>>, Socket, Context).


%% ============================== RECEIVE DATA =====================================

%% @doc Start receiving messages from the websocket
receive_loop(Buff, Socket, Context) ->
    case mochiweb_socket:recv(Socket, 0, infinity) of
        {ok, Received} ->
            z_websocket_hybi17:handle_data(<<Buff/binary, Received/binary>>, Socket, Context);
        {error, Reason} ->
            {error, Reason}
    end.


% Check if we received a full frame
handle_data(Data, Socket, Context) when byte_size(Data) =< 1 ->
    receive_loop(Data, Socket, Context);
handle_data(Data, Socket, Context) ->
    << 1:1, 0:3, Opcode:4, Mask:1, PayloadLen:7, Rest/bits >> = Data,
    case {PayloadLen, Rest} of
        {126, _} when Opcode >= 8 -> 
            close({error, protocol}, Socket, Context);
        {127, _} when Opcode >= 8 ->
            close({error, protocol}, Socket, Context);
        {126, << L:16, R/bits >>}  -> 
            unmask(Data, R, Opcode, Mask, L, Socket, Context);
        {126, Rest} -> 
            unmask(Data, Rest, Opcode, Mask, undefined, Socket, Context);
        {127, << 0:1, L:63, R/bits >>} -> 
            unmask(Data, R, Opcode, Mask, L, Socket, Context);
        {127, Rest} -> 
            unmask(Data, Rest, Opcode, Mask, undefined, Socket, Context);
        {PayloadLen, Rest} -> 
            unmask(Data, Rest, Opcode, Mask, PayloadLen, Socket, Context)
    end.

% Check if we have received a complete message.
unmask(_Data, Rest, Opcode, 0, 0, Socket, Context) ->
    handle_frame(Rest, Opcode, <<>>, Socket, Context);
unmask(Data, _Rest, _Opcode, 1, undefined, Socket, Context) ->
    z_websocket_hybi17:receive_loop(Data, Socket, Context);
unmask(Data, Rest, _Opcode, 1, PayloadLen, Socket, Context) when PayloadLen + 4 > byte_size(Rest) ->
    z_websocket_hybi17:receive_loop(Data, Socket, Context);
unmask(_Data, Rest, Opcode, 1, PayloadLen, Socket, Context) ->
    <<MaskKey:32, Payload:PayloadLen/binary, Rest2/bits>> = Rest,
    unmask_data(Opcode, Payload, MaskKey, Rest2, Socket, Context, <<>>).


% unmask the received data
unmask_data(Opcode, <<>>, _MaskKey, RemainingData, Socket, Context, Acc) ->
    handle_frame(RemainingData, Opcode, Acc, Socket, Context);
unmask_data(Opcode, <<O:32, Rest/bits>>, MaskKey, RemainingData, Socket, Context, Acc) ->
    T = O bxor MaskKey,
    unmask_data(Opcode, Rest, MaskKey, RemainingData, Socket, Context, <<Acc/binary, T:32>>);
unmask_data(Opcode, <<O:24>>, MaskKey, RemainingData, Socket, Context, Acc) ->
    << MaskKey2:24, _:8 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    handle_frame(RemainingData, Opcode, <<Acc/binary, T:24>>, Socket, Context);
unmask_data(Opcode, <<O:16>>, MaskKey, RemainingData, Socket, Context, Acc) ->
    << MaskKey2:16, _:16 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    handle_frame(RemainingData, Opcode, <<Acc/binary, T:16>>, Socket, Context);
unmask_data(Opcode, <<O:8>>, MaskKey, RemainingData, Socket, Context, Acc) ->
    << MaskKey2:8, _:24 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    handle_frame(RemainingData, Opcode, <<Acc/binary, T:8>>, Socket, Context).


% Text frame
handle_frame(RemainingData, 1, Message, Socket, Context) ->
    controller_websocket:handle_message(Message, Context),
    handle_data(RemainingData, Socket, Context);
% Binary frame
handle_frame(RemainingData, 2, Message, Socket, Context) ->
    controller_websocket:handle_message(Message, Context),
    handle_data(RemainingData, Socket, Context);
% Close control frame
handle_frame(_RemainingData, 8, _Message, Socket, Context) ->
    close({normal, closed}, Socket, Context);
% Ping control frame
handle_frame(RemainingData, 9, Message, Socket, Context) ->
    % send a pong
    Len = hybi_payload_length(byte_size(Message)),
    send(Socket, <<1:1, 0:3, 10:4, 0:1, Len/bits, Message/binary>>),
    handle_data(RemainingData, Socket, Context);
% Pong control frame
handle_frame(RemainingData, 10, _Message, Socket, Context) ->
    handle_data(RemainingData, Socket, Context).


%% @TODO: log any errors
close(_Reason, Socket, _Context) ->
    send(Socket, <<1:1,0:3,8:4,0:8>>),
    ok.


%% ============================== SEND DATA =====================================



%% @doc Start the loop passing data (scripts) from the page to the browser
start_send_loop(Socket, Context) ->
    % We want to receive any exit signal (including 'normal') from the socket's process.
    process_flag(trap_exit, true),
    z_session_page:websocket_attach(self(), Context),
    send_loop(Socket, Context).


send_loop(Socket, Context) ->
    receive
        {send_data, Data} ->
            send_frame(Socket, Data),
            z_websocket_hybi17:send_loop(Socket, Context);
        {'EXIT', _FromPid, _Reason} ->
            % Exit of the socket's process, stop sending data.
            exit;
        _ ->
            z_websocket_hybi17:send_loop(Socket, Context)
    after ?PING_TIMEOUT ->
        send_ping(Socket),
        send_loop(Socket, Context)
    end.


% Send a text frame
send_frame(Socket, Data) ->
    Len = hybi_payload_length(iolist_size(Data)),
    send(Socket, [<<1:1, 0:3, 1:4, 0:1, Len/bits>>, Data]).

% Send a ping to see if the UA is still listening
send_ping(Socket) ->
    Data = <<"Zzz?">>,
    Len = hybi_payload_length(iolist_size(Data)),
    send(Socket, [<<1:1, 0:3, 9:4, 0:1, Len/bits>>, Data]).


%% @doc Send data to the user agent
send(undefined, _Data) ->
    ok;
send(Socket, Data) ->
    case mochiweb_socket:send(Socket, iolist_to_binary(Data)) of
        ok -> ok;
        {error, closed} -> exit(closed);
        _ -> exit(normal)
    end.



hybi_payload_length(N) ->
    case N of
        N when N =< 125 -> << N:7 >>;
        N when N =< 16#ffff -> << 126:7, N:16 >>;
        N when N =< 16#7fffffffffffffff -> << 127:7, N:64 >>
    end.

