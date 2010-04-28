%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Ery Lee<ery.lee@gmail.com>

%% @doc
%% The module <strong>{@module}</strong> implements a echo component.
%%
%% <p>
%% This is a example use of the exmpp_component
%% </p>
%%
%% <p>
%% Usage:
%% </p>
%% <pre>{ok, session} = echo_component:start().
%% echo_component:stop(Session).</pre>
-module(echo_component).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([start/0, stop/1]).
-export([init/0]).

start() ->
    spawn(?MODULE, init, []).

stop(EchoComPid) ->
    EchoComPid ! stop.

init() ->
    application:start(exmpp),
    XmppCom = exmpp_component:start(),
    exmpp_component:auth(XmppCom, "monet.opengoss.com", "public"),
    _StreamId = exmpp_component:connect(XmppCom, "localhost", 5288),
    exmpp_component:handshake(XmppCom),
    loop(XmppCom).

loop(XmppCom) ->
    receive
        stop ->
            exmpp_component:stop(XmppCom);
        %% If we receive a message, we reply with the same message
        Record = #received_packet{packet_type=message, raw_packet=Packet} ->
            io:format("~p~n", [Record]),
            echo_packet(XmppCom, Packet),
            loop(XmppCom);
        Record ->
            io:format("~p~n", [Record]),
            loop(XmppCom)
    end.

%% Send the same packet back for each message received
echo_packet(XmppCom, Packet) ->
    From = exmpp_xml:get_attribute(Packet, from, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, to, <<"service@monet.opengoss.com">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, from, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, to, From),
    NewPacket = exmpp_xml:remove_attribute(TmpPacket2, id),
    exmpp_component:send_packet(XmppCom, NewPacket).
