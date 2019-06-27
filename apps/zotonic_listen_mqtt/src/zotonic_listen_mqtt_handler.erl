%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc MQTT protocol handler for ranch

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

-module(zotonic_listen_mqtt_handler).

-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).
-export([
    recv_connect/3,
    loop/3,
    transport/3
]).

-define(MQTT_CONNECT_TIMEOUT, 10000).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("mqtt_packet_map/include/mqtt_packet_map.hrl").

-spec start_link( ranch:ref(), Socket :: pid(), module(), map() ) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [ Ref, Socket, Transport, Opts ]),
    {ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
    ok = ranch:accept_ack(Ref),
    ?MODULE:recv_connect(Socket, Transport, Opts#{ data => <<>> }).

%% @todo Close connection if connect packet is too large
recv_connect(Socket, Transport, #{ data := Data } = State) ->
    case Transport:recv(Socket, 0, ?MQTT_CONNECT_TIMEOUT) of
        {ok, Rcvd} ->
            Data1 = <<Data/binary, Rcvd/binary>>,
            {ok, {PeerIP, _}} = Transport:peername(Socket),
            Options = #{
                transport => fun(D) -> ?MODULE:transport(Transport, Socket, D) end,
                connection_pid => self(),
                peer_ip => PeerIP,
                context_prefs => #{
                }
            },
            case mqtt_sessions:incoming_connect(Data1, Options) of
                {ok, {SessionRef, Rest}} ->
                    Self = self(),
                    SessionMonitorPid = erlang:spawn_link(fun() -> session_monitor(Self, Socket, Transport, SessionRef) end),
                    State1 = State#{
                        data => undefined,
                        session_ref => SessionRef,
                        session_monitor => SessionMonitorPid
                    },
                    loop_data(Rest, Socket, Transport, State1);
                {error, incomplete_packet} ->
                    ?MODULE:recv_connect(Socket, Transport, State#{ data => Data1 });
                {error, expect_connect} ->
                    lager:info("MQTT: refusing connect with wrong packet type"),
                    ok = Transport:close(Socket);
                {error, _} ->
                    % Invalid packet or unkown host - just close the connection
                    ok = Transport:close(Socket)
            end;
        {error, _} ->
            ok = Transport:close(Socket)
    end.

%% @doc Receive data loop
loop(Socket, Transport, State) ->
    case Transport:recv(Socket, 0, infinity) of
        {ok, Data} ->
            loop_data(Data, Socket, Transport, State);
        _ ->
            ok = Transport:close(Socket)
    end.

loop_data(Data, Socket, Transport, #{ session_ref := SessionRef } = State) ->
    case mqtt_sessions:incoming_data(SessionRef, Data) of
        ok ->
            ?MODULE:loop(Socket, Transport, State);
        {error, Reason} ->
            lager:info("MQTT incoming_data/2 returned: ~p", [ Reason ]),
            ok = Transport:close(Socket)
    end.

% --------------------------------- Support Routines ----------------------------------

%% @doc Transport data to client, called by mqtt_sessions.
transport(Transport, Socket, Payload) when is_binary(Payload) ->
    case Transport:send(Socket, Payload) of
        ok ->
            ok;
        {error, _} = Error ->
            Transport:close(Socket),
            Error
    end;
transport(Transport, Socket, disconnect) ->
    Transport:close(Socket).


%% @doc Small session monitor process linked to the connection handler.
session_monitor(ConnectionPid, Socket, Transport, SessionPid) ->
    MRefSession = erlang:monitor(process, SessionPid),
    MRefConnection = erlang:monitor(process, ConnectionPid),
    receive
        {'DOWN', MRefSession, process, SessionPid, _Reason} ->
            Transport:close(Socket),
            erlang:exit(session_down);
        {'DOWN', MRefConnection, process, ConnectionPid, _Reason} ->
            ok
    end.
