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

-define(MQTT_REQUEST_TIMEOUT, 60000).

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
    case Transport:recv(Socket, 0, ?MQTT_REQUEST_TIMEOUT) of
        {ok, Rcvd} ->
            Data1 = <<Data/binary, Rcvd/binary>>,
            case mqtt_packet_map:decode(Data1) of
                {ok, {Msg, Rest}} ->
                    handle_connect(Msg, Socket, Transport, State#{ data => Rest });
                {error, incomplete_packet} ->
                    ?MODULE:recv_connect(Socket, Transport, State#{ data => Data1 });
                {error, _} ->
                    % Invalid packet - just close the connection
                    ok = Transport:close(Socket)
            end;
        {error, _} ->
            ok = Transport:close(Socket)
    end.

handle_connect(#{ type := connect, username := Username } = Msg, Socket, Transport, State) when is_binary(Username) ->
    handle_connect_vhost( binary:split(Username, <<":">>), Msg, Socket, Transport, State);
handle_connect(#{ type := connect } = Msg, Socket, Transport, _State) ->
    connack_close(?MQTT_RC_BAD_USERNAME_PASSWORD, Msg, Socket, Transport);
handle_connect(#{ type := Type }, Socket, Transport, _State) ->
    lager:info("MQTT: refusing connect with first packet type ~p", [Type]),
    connack_close(?MQTT_RC_PROTOCOL_ERROR, #{ protocol_version => 5 }, Socket, Transport).

handle_connect_vhost([ Host, Username ], Msg, Socket, Transport, State) ->
    case get_site_for_hostname(Host) of
        {ok, Site} ->
            Msg1 = Msg#{ username => Username },
            State1 = State#{
                site => Site,
                protocol_version => maps:get(protocol_version, Msg)
            },
            case handle_message(Msg1, Socket, Transport, State1) of
                {ok, State2} ->
                    loop_data(<<>>, Socket, Transport, State2);
                {error, Reason} ->
                    lager:info("MQTT handle incoming data returned: ~p", [ Reason ]),
                    ok = Transport:close(Socket)
            end;
        undefined ->
            connack_close(?MQTT_RC_SERVER_UNAVAILABLE, Msg, Socket, Transport)
    end;
handle_connect_vhost([ Username ], Msg, Socket, Transport, _State) ->
    lager:info("Close connection, no vhost in username: ~p", [Username]),
    connack_close(?MQTT_RC_BAD_USERNAME_PASSWORD, Msg, Socket, Transport).


%% @doc Receive data loop
loop(Socket, Transport, State) ->
    case Transport:recv(Socket, 0, ?MQTT_REQUEST_TIMEOUT) of
        {ok, Data} ->
            loop_data(Data, Socket, Transport, State);
        _ ->
            ok = Transport:close(Socket)
    end.

loop_data(Data, Socket, Transport, State) ->
    case handle_incoming_data(Data, Socket, Transport, State) of
        {ok, State1} ->
            ?MODULE:loop(Socket, Transport, State1);
        {error, Reason} ->
            lager:info("MQTT handle incoming data returned: ~p", [ Reason ]),
            ok = Transport:close(Socket)
    end.

% -------------------------------------------------------------------

connack_close(ReasonCode, #{ protocol_version := PV }, Socket, Transport) ->
    Msg = #{
        type => connack,
        reason_code => ReasonCode
    },
    {ok, MsgBin} = mqtt_packet_map:encode(PV, Msg),
    _ = Transport:send(Socket, MsgBin),
    ok = Transport:close(Socket).

% -------------------------------------------------------------------

handle_incoming_data(NewData, Socket, Transport, #{ data := Data } = State) ->
    case decode_incoming_data(<<Data/binary, NewData/binary>>, Socket, Transport, State) of
        {ok, RestData, State1} ->
            {ok, State1#{ data => RestData }};
        {error, _Reason} = Error ->
            Error
    end.

decode_incoming_data(<<>>, _Socket, _Transport, State) ->
    {ok, <<>>, State};
decode_incoming_data(Data, Socket, Transport, #{ protocol_version := ProtocolVersion } = State) ->
    case mqtt_packet_map:decode(ProtocolVersion, Data) of
        {ok, {Msg, RestData}} ->
            case handle_message(Msg, Socket, Transport, State) of
                {ok, State1} ->
                    decode_incoming_data(RestData, Socket, Transport, State1);
                {error, _} = Error ->
                    Error
            end;
        {error, incomplete_packet} ->
            {ok, Data, State};
        {error, _} = Error ->
            Error
    end.

%% Send the message to the attached MQTT session
handle_message(Msg, Socket, Transport, #{ site := Site } = State) ->
    {ok, {PeerIP, _}} = Transport:peername(Socket),
    MsgOptions = case maps:get(type, Msg) of
        connect ->
            #{
                transport => fun(D) -> ?MODULE:transport(Transport, Socket, D) end,
                connection_pid => self(),
                peer_ip => PeerIP,
                context_prefs => #{
                    % user_id => z_acl:user(Context),
                    % language => z_context:language(Context),
                    % timezone => z_context:tz(Context),
                    % auth_options => z_context:get(auth_options, Context, #{})
                }
            };
        _ ->
            #{}
    end,
    OptSessionRef = maps:get(session_ref, State, undefined),
    case mqtt_sessions:incoming_message(Site, OptSessionRef, Msg, MsgOptions) of
        {ok, undefined} ->
            {ok, State};
        {ok, OptSessionRef} ->
            {ok, State};
        {ok, NewSessionRef} ->
            Self = self(),
            SessionMonitorPid = erlang:spawn_link(fun() -> session_monitor(Self, Socket, Transport) end),
            {ok, State#{ session_ref => NewSessionRef, session_monitor => SessionMonitorPid }};
        {error, _} = Error ->
            Error
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


%% @doc Find the site or fallback site that will handle the request
-spec get_site_for_hostname(binary()) -> {ok, atom()} | undefined.
get_site_for_hostname(Hostname) when is_binary(Hostname) ->
    case z_sites_dispatcher:get_site_for_hostname(Hostname) of
        {ok, Site} ->
            case z_sites_manager:wait_for_running(Site) of
                ok -> {ok, Site};
                {error, _} -> undefined
            end;
        undefined ->
            undefined
    end.

%% @doc Small session monitor process linked to the connection handler.
session_monitor(ConnectionPid, Socket, Transport) ->
    _MRefConnection = erlang:monitor(process, ConnectionPid),
    receive
        {'DOWN', _MRef, process, SessionPid, _Reason} when is_pid(SessionPid) ->
            Transport:close(Socket),
            erlang:exit(session_down)
    end.


