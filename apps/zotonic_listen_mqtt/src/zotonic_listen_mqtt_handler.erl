%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2020 Marc Worrell
%% @doc MQTT protocol handler for ranch

%% Copyright 2019-2020 Marc Worrell
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
    transport/4
]).

%% Connect data should arrive before this timeout (msec)
-define(MQTT_CONNECT_TIMEOUT, 4000).

%% Connect should be finished before this timeout (msec)
-define(MQTT_CONNECT_MAX_TIMEOUT, 10000).

%% Maximum size for the connect packet
-define(MQTT_CONNECT_MAXSIZE, 1024).


-include_lib("zotonic_core/include/zotonic.hrl").

-spec start_link( ranch:ref(), Socket :: pid(), module(), map() ) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [ Ref, Socket, Transport, Opts ]),
    {ok, Pid}.

init(Ref, _Socket, Transport, Opts) ->
    timer:send_after(?MQTT_CONNECT_MAX_TIMEOUT, connect_check),
    {ok, NewSocket} = ranch:handshake(Ref),
    ?MODULE:recv_connect(NewSocket, Transport, Opts#{ data => <<>> }).

recv_connect(Socket, Transport, State) ->
    receive
        connect_check ->
            _ = Transport:close(Socket)
        after 0 ->
            recv_connect_1(Socket, Transport, State)
    end.

recv_connect_1(Socket, Transport, #{ data := Data } = State) ->
    case Transport:recv(Socket, 0, ?MQTT_CONNECT_TIMEOUT) of
        {ok, Rcvd} ->
            recv_connect_data(<<Data/binary, Rcvd/binary>>, Socket, Transport, State);
        {error, _} ->
            ok = Transport:close(Socket)
    end.

recv_connect_data(ConnectData, Socket, Transport, _State) when size(ConnectData) > ?MQTT_CONNECT_MAXSIZE ->
    ?LOG_INFO("MQTT: refusing connect with large connect packet"),
    ok = Transport:close(Socket);
recv_connect_data(ConnectData, Socket, Transport, State) ->
    Self = self(),
    {ok, {PeerIP, _}} = Transport:peername(Socket),
    Options = #{
        transport => fun(D) -> ?MODULE:transport(Transport, Socket, Self, D) end,
        connection_pid => self(),
        peer_ip => PeerIP,
        context_prefs => #{
            user_agent => <<"MQTT">>
        }
    },
    case mqtt_sessions:incoming_connect(ConnectData, Options) of
        {ok, {SessionRef, Rest}} ->
            Self = self(),
            SessionMonitorPid = proc_lib:spawn_link(fun() -> session_monitor(Self, Socket, Transport, SessionRef) end),
            State1 = State#{
                data => undefined,
                session_ref => SessionRef,
                session_monitor => SessionMonitorPid
            },
            loop_data(Rest, Socket, Transport, State1);
        {error, incomplete_packet} ->
            ?MODULE:recv_connect(Socket, Transport, State#{ data => ConnectData });
        {error, expect_connect} ->
            ?LOG_INFO(#{
                text => "MQTT: refusing connect with wrong packet type",
                in => zotonic_listen_mqtt,
                reason => expect_connect,
                src => inet:ntoa(PeerIP),
                protocol => mqtt
            }),
            ok = Transport:close(Socket);
        {error, unknown_host} ->
            %% Common auth mistake when connecting MQTT clients to zotonic. Because most clients don't
            %% report the connection error, it is good to at least have a message in the log.
            ?LOG_INFO(#{
                text => "MQTT: refusing connect with unknown host. Use \"example.com:localuser\" as username",
                in => zotonic_listen_mqtt,
                reason => unknown_host,
                src => inet:ntoa(PeerIP),
                protocol => mqtt
            }),
            ok = Transport:close(Socket);
        {error, Reason} ->
            % Invalid packet or unkown host - just close the connection
            ?LOG_INFO(#{
                text => "MQTT: refusing connect with invalid packet or unkown host",
                in => zotonic_listen_mqtt,
                reason => Reason,
                src => inet:ntoa(PeerIP),
                protocol => mqtt
            }),
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
            {ok, {PeerIP, _}} = Transport:peername(Socket),
            ?LOG_INFO(#{
                text => "MQTT incoming_data/2 with unexpected data",
                in => zotonic_listen_mqtt,
                reason => Reason,
                src => inet:ntoa(PeerIP),
                protocol => mqtt
            }),
            ok = Transport:close(Socket)
    end.

% --------------------------------- Support Routines ----------------------------------

%% @doc Transport data to client, called by mqtt_sessions.
transport(Transport, Socket, _Self, Payload) when is_binary(Payload) ->
    case Transport:send(Socket, Payload) of
        ok ->
            ok;
        {error, _} = Error ->
            Transport:close(Socket),
            Error
    end;
transport(Transport, Socket, Self, {ok, Payload}) when is_binary(Payload) ->
    transport(Transport, Socket, Self, Payload);
transport(Transport, Socket, Self, disconnect) ->
    Transport:close(Socket),
    timer:apply_after(500, erlang, exit, [ Self, disconnect ]).


%% @doc Small session monitor process linked to the connection handler.
-spec session_monitor( pid(), pid(), module(), pid() ) -> ok | {error, session_down}.
session_monitor(ConnectionPid, Socket, Transport, SessionPid) ->
    MRefSession = erlang:monitor(process, SessionPid),
    MRefConnection = erlang:monitor(process, ConnectionPid),
    receive
        {'DOWN', MRefSession, process, SessionPid, _Reason} ->
            Transport:close(Socket),
            erlang:exit(ConnectionPid, session_down),
            {error, session_down};
        {'DOWN', MRefConnection, process, ConnectionPid, _Reason} ->
            ok
    end.
