%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc WebSocket draft-hixie-thewebsocketprotocol-75 (Chrome 4; Safari 5.0.0)

%% Copyright 2010-2011 Marc Worrell
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

-module(z_websocket_hixie75).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    start/2,
    
    receive_loop/4,
    start_send_loop/2,
    send_loop/2,
    
    send/2
]).


% Hixie-75 (Chrome 4; Safari 5.0.0)
% First draft protocol version, this code should be removed in due time.
start(ReqData, Context1) ->
    Hostname = m_site:get(hostname, Context1),
    WebSocketPath = iolist_to_binary(["/websocket?z_pageid=", mochiweb_util:quote_plus(z_context:get_q("z_pageid", Context1))]),
    Protocol = case wrq:is_ssl(ReqData) of true -> "https"; _ -> "http" end,
    Socket = webmachine_request:socket(ReqData),
    Data = ["HTTP/1.1 101 Web Socket Protocol Handshake", 13, 10,
            "Upgrade: WebSocket", 13, 10,
            "Connection: Upgrade", 13, 10,
            "WebSocket-Origin: ",Protocol,"://", Hostname, 13, 10,
            "WebSocket-Location: ws://", Hostname, WebSocketPath, 13, 10,
            13, 10
            ],
    ok = send(Socket, Data),
    spawn_link(fun() -> z_websocket_hixie75:start_send_loop(Socket, Context1) end),
    z_websocket_hixie75:receive_loop(none, nolength, Socket, Context1).


%% ============================== RECEIVE DATA =====================================

%% @doc Start receiving messages from the websocket
receive_loop(Buff, Length, Socket, Context) ->
    case mochiweb_socket:recv(Socket, 0, infinity) of
        {ok, Received} ->
            handle_data(Buff, Length, Received, Socket, Context);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Upack any data frames, send them to the handling functions.
handle_data(none, nolength, <<0,T/binary>>, Socket, Context) ->
    <<Type, _/binary>> = T,
    case Type =< 127 of
        true -> 
            handle_data(<<>>, nolength, T, Socket, Context);
        false ->
            {Length, LenBytes} = unpack_length(T),
            <<_:LenBytes/bytes, Rest:Length/bytes>> = T,
            handle_data(<<>>, Length, Rest, Socket, Context)
    end;

%% Extract frame ending with 255
handle_data(none, nolength, <<>>, Socket, Context) ->
    z_websocket_hixie75:receive_loop(none, nolength, Socket, Context);
handle_data(<<>>, nolength, <<255,_T/binary>>, _Socket, _Context) ->
    % A packet of <<0,255>> signifies that the ua wants to close the connection
    ua_close_request;
handle_data(Msg, nolength, <<255,T/binary>>, Socket, Context) ->
    controller_websocket:handle_message(Msg, Context),
    handle_data(none, nolength, T, Socket, Context);
handle_data(Msg, nolength, <<H,T/binary>>, Socket, Context) ->
    handle_data(<<Msg/binary, H>>, nolength, T, Socket, Context);

%% Extract frame with length bytes
handle_data(Msg, 0, T, Socket, Context) ->
    controller_websocket:handle_message(Msg, Context),
    handle_data(none, nolength, T, Socket, Context);
handle_data(Msg, Length, <<H,T/binary>>, Socket, Context) when is_integer(Length) and Length > 0 ->
    handle_data(<<Msg/binary, H>>, Length-1, T, Socket, Context);

%% Data ended before the end of the frame, loop to fetch more
handle_data(Msg, Length, <<>>, Socket, Context) ->
    z_websocket_hixie75:receive_loop(Msg, Length, Socket, Context).


%% @doc Unpack the length bytes
%% author: Davide Marquês (From yaws_websockets.erl)
unpack_length(Binary) ->
    unpack_length(Binary, 0, 0).
unpack_length(Binary, LenBytes, Length) ->
    <<_:LenBytes/bytes, B, _/bitstring>> = Binary,
    B_v = B band 16#7F,
    NewLength = (Length * 128) + B_v,
    case B band 16#80 of
    16#80 ->
        unpack_length(Binary, LenBytes + 1, NewLength);
    0 ->
        {NewLength, LenBytes + 1}
    end.

% %% @doc Pack the length in 7 bits bytes
% pack_length(N) ->
%     pack_length(N, []).
% 
% pack_length(N, Acc) ->
%     N1 = N div 128,
%     B = N rem 128,
%     case Acc of
%         [] ->
%             pack_length(N1, [B|Acc]);
%         _ ->
%             case N1 of
%                 0 -> [B+128|Acc];
%                 _ -> pack_length(N1, [B+128|Acc])
%             end
%     end.


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
            case send(Socket, [0, Data, 255]) of
                ok -> z_websocket_hixie75:send_loop(Socket, Context);
                closed -> closed
            end;
        {'EXIT', _FromPid, _Reason} ->
            % Exit of the socket's process, stop sending data.
            exit;
        _ ->
            z_websocket_hixie75:send_loop(Socket, Context)
    end.



%% @doc Send data to the user agent
send(undefined, _Data) ->
    ok;
send(Socket, Data) ->
    case mochiweb_socket:send(Socket, iolist_to_binary(Data)) of
        ok -> ok;
        {error, closed} -> closed;
        _ -> exit(normal)
    end.

