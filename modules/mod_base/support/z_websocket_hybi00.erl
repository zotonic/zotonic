%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc WebSocket draft-ietf-hybi-thewebsocketprotocol-00 (Chrome 6; Safari 5.0)

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

-module(z_websocket_hybi00).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    start/3
]).


start(WsKey1, ReqData, Context1) ->
    Hostname = m_site:get(hostname, Context1),
    WebSocketPath = iolist_to_binary(["/websocket?z_pageid=", mochiweb_util:quote_plus(z_context:get_q("z_pageid", Context1))]),
    Protocol = case wrq:is_ssl(ReqData) of true -> "https"; _ -> "http" end,
    Socket = webmachine_request:socket(ReqData),

    %% Sec-Websocket stuff
    Key1 = process_key(WsKey1),
    Key2 = process_key(z_context:get_req_header("sec-websocket-key2", Context1)),
    {ok, Body} = mochiweb_socket:recv(Socket, 8, infinity),
    SignKey = crypto:md5(<<Key1:32/integer, Key2:32/integer, Body/binary>>),

    %% Send the handshake
    Data = ["HTTP/1.1 101 Web Socket Protocol Handshake", 13, 10,
            "Upgrade: WebSocket", 13, 10,
            "Connection: Upgrade", 13, 10,
            "Sec-WebSocket-Origin: ",Protocol,"://", Hostname, 13, 10,
            "Sec-WebSocket-Location: ws://", Hostname, WebSocketPath, 13, 10,
            "Sec-WebSocket-Protocol: zotonic", 13, 10,
            13, 10,
            <<SignKey/binary>>
            ],
    ok = z_websocket_hixie75:send(Socket, Data),
    spawn_link(fun() -> z_websocket_hixie75:start_send_loop(Socket, Context1) end),
    z_websocket_hixie75:receive_loop(none, nolength, Socket, Context1).


%% Process a key from the websockey handshake, return an integere
%% @spec process_key(string()) -> integer
process_key(L) ->
    {Number,Spaces} = split_key(L, [], 0),
    Number div Spaces.

    split_key([], DAcc, Spaces) ->
        {list_to_integer(lists:reverse(DAcc)), Spaces};
    split_key([C|Rest], DAcc, Spaces) when C >= $0, C =< $9 ->
        split_key(Rest, [C|DAcc], Spaces);
    split_key([32|Rest], DAcc, Spaces) ->
        split_key(Rest, DAcc, Spaces+1);
    split_key([_|Rest], DAcc, Spaces) ->
        split_key(Rest, DAcc, Spaces).


