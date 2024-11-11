%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018-2024 Marc Worrell
%% @doc Zotonic specific callbacks for MQTT connections
%% @end

%% Copyright 2018-2024 Marc Worrell
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

-module(z_mqtt_sessions_runtime).

-export([
    vhost_pool/1,
    pool_default/0,

    new_user_context/3,
    control_message/3,
    ping/1,
    connect/4,
    reauth/2,
    is_allowed/4,
    is_valid_message/3
    ]).

-behaviour(mqtt_sessions_runtime).

-define(none(A), (A =:= undefined orelse A =:= <<>>)).

-include_lib("mqtt_packet_map/include/mqtt_packet_map.hrl").
-include_lib("../../include/zotonic.hrl").

%% @doc Return the MQTT pool name for the given hostname.
-spec vhost_pool( binary() ) -> {ok, atom()} | {error, unknown_host}.
vhost_pool( <<"zotonic_site_status">> ) ->
    case z_sites_manager:wait_for_running(zotonic_site_status) of
        ok -> {ok, zotonic_site_status};
        {error, _} -> {error, unknown_host}
    end;
vhost_pool( Hostname ) ->
    case z_sites_dispatcher:get_site_for_hostname(Hostname) of
        {ok, Site} ->
            case z_sites_manager:wait_for_running(Site) of
                ok -> {ok, Site};
                {error, _} -> {error, unknown_host}
            end;
        undefined ->
            {error, unknown_host}
    end.

-spec pool_default() -> {error, unknown_host}.
pool_default() ->
    {error, unknown_host}.


% TODO: differentiate publish and subscribe on bridged reply/public topics

-spec new_user_context(Site, ClientId, SessionOptions) -> Context when
    Site :: atom(),
    ClientId :: binary(),
    SessionOptions :: mqtt_sessions:session_options(),
    Context :: z:context().
new_user_context( Site, ClientId, SessionOptions ) ->
    exometer:update([zotonic, Site, mqtt, connects], 1),
    Prefs = maps:get(context_prefs, SessionOptions, #{}),
    Context = z_context:new(Site),
    Context1 = Context#context{
        client_topic = [ <<"bridge">>, ClientId ],
        client_id = ClientId,
        routing_id = maps:get(routing_id, SessionOptions, <<>>)
    },
    Context2 = maybe_set_language(Prefs, Context1),
    Context3 = maybe_set_timezone(Prefs, Context2),
    Context4 = maybe_set_peer_ip(SessionOptions, Context3),
    Context5 = maybe_set_user_agent(Prefs, Context4),
    SessionId = maps:get(cotonic_sid, Prefs, undefined),
    Context6 = z_context:set_session_id(SessionId, Context5),
    z_context:logger_md(Context6),
    Context6.

-spec control_message(SubTopic, Message, Context) -> {ok, Context1} when
    SubTopic :: list( binary() ),
    Message :: mqtt_packet_map:mqtt_packet(),
    Context :: z:context(),
    Context1 :: z:context().
control_message([ <<"auth">> ], #{ payload := Payload }, Context) ->
    ContextUserId = z_acl:user(Context),
    AuthUserId = maps:get(<<"user_id">>, Payload, undefined),
    if
        ContextUserId =:= AuthUserId ->
            Context1 = maybe_set_language(Payload, Context),
            Context2 = maybe_set_timezone(Payload, Context1),
            Context3 = z_notifier:foldl(#request_context{ phase = refresh }, Context2, Context2),
            z_auth:publish_user_session(Context3),
            {ok, Context3};
        true ->
            % Kill and force reconnect - switching users
            timer:apply_after(100, mqtt_sessions_process, kill, [self()]),
            {ok, Context}
    end;
control_message([ <<"sid">> ], #{ payload := Payload }, Context) ->
    Context1 = maybe_set_sid(Payload, Context),
    Context2 = z_notifier:foldl(#request_context{ phase = refresh }, Context1, Context1),
    {ok, Context2};
control_message(_Topic, _Packet, Context) ->
    {ok, Context}.

-spec ping(Context) -> {ok, Context1} when
    Context :: z:context(),
    Context1 :: z:context().
ping(Context) ->
    z_auth:publish_user_session(Context),
    {ok, Context}.

maybe_set_peer_ip(#{ peer_ip := PeerIp }, Context) ->
    z_context:set(peer_ip, PeerIp, Context);
maybe_set_peer_ip(#{ }, Context) ->
    Context.

maybe_set_language(#{ <<"preferences">> := #{ <<"language">> := <<>> } }, Context) ->
    Context;
maybe_set_language(#{ <<"preferences">> := #{ <<"language">> := Lang } }, Context) when is_binary(Lang) ->
    set_language(Lang, Context);
maybe_set_language(#{ language := <<>> }, Context) ->
    Context;
maybe_set_language(#{ language := Lang }, Context) ->
    set_language(Lang, Context);
maybe_set_language(_Payload, Context) ->
    Context.

maybe_set_timezone(#{ <<"preferences">> := #{ <<"timezone">> := <<>> } }, Context) ->
    Context;
maybe_set_timezone(#{ <<"preferences">> := #{ <<"timezone">> := Timezone } }, Context) when is_binary(Timezone) ->
    z_context:set_tz(Timezone, Context);
maybe_set_timezone(#{ timezone := <<>> }, Context) ->
    Context;
maybe_set_timezone(#{ timezone := Timezone}, Context) when is_binary(Timezone) ->
    z_context:set_tz(Timezone, Context);
maybe_set_timezone(_Payload, Context) ->
    Context.

maybe_set_sid(#{ <<"options">> := #{ <<"sid">> := <<>> } }, Context) ->
    Context;
maybe_set_sid(#{ <<"options">> := #{ <<"sid">> := Sid } }, Context) when is_binary(Sid) ->
    z_context:set_session_id(Sid, Context);
maybe_set_sid(_Payload, Context) ->
    Context.

maybe_set_user_agent(#{ user_agent := UserAgent }, Context) when is_binary(UserAgent) ->
    UserAgent1 = z_string:truncatechars(UserAgent, 300),
    z_context:set(user_agent, UserAgent1, Context);
maybe_set_user_agent(_Payload, Context) ->
    Context.

set_language(Lang, Context) ->
    case z_module_manager:is_provided(mod_translation, Context) of
        true ->
            mod_translation:set_language(Lang, Context);
        false ->
            z_context:set_language(Lang, Context)
    end.

set_connect_context_options(Options, Context) ->
    Prefs = maps:get(context_prefs, Options, #{}),
    Context1 = maybe_set_language(Prefs, Context),
    Context2 = maybe_set_timezone(Prefs, Context1),
    Context3 = z_context:set(auth_options, maps:get(auth_options, Prefs, #{}), Context2),
    Context4 = maybe_set_peer_ip(Options, Context3),
    Context5 = case maps:get(cotonic_sid, Prefs, undefined) of
        undefined -> Context4;
        Sid -> z_context:set_session_id(Sid, Context4)
    end,
    Context6 = maybe_set_user_agent(Prefs, Context5),
    z_acl:logon_refresh(Context6).

set_connect_properties(#{ <<"cotonic_sid">> := Sid }, Context) when not ?none(Sid) ->
    RoutingId = Context#context.routing_id,
    Context1 = z_context:set_session_id(Sid, Context),
    { #{ <<"cotonic-routing-id">> => RoutingId }, Context1 };
set_connect_properties(_Properties, Context) ->
    { #{}, Context }.


%% @doc Force reauthentication of the user-agent with the server. Kills the current
%% MQTT session after requesting the client to refresh by calling the auth model.
%% This function is triggered for websocket connections by publishing to the topic:
%% user/+UserId/session/logoff.
subscribe_forced_logoff(Context) ->
    case z_acl:user(Context) of
        undefined ->
            {error, no_user};
        UserId ->
            % Start process to prevent unsubscribe on clean session start.
            SessionPid = self(),
            {ok, ClientId} = z_context:client_id(Context),
            z_proc:spawn_link_md(
                fun() ->
                    ok = z_mqtt:subscribe(
                        [ <<"~user">>, <<"session">>, <<"logoff">> ],
                        Context),
                    MRef = erlang:monitor(process, SessionPid),
                    garbage_collect(),
                    receive
                        {'DOWN', MRef, process, _Pid, _Reason} ->
                            ok;
                        {mqtt_msg, _Msg} ->
                            ?LOG_INFO(#{
                                in => zotonic_core,
                                text => <<"Forced logoff of MQTT session">>,
                                client_id => ClientId,
                                user_id => UserId
                            }),
                            ok = z_mqtt:publish(
                                [ <<"bridge">>, ClientId, <<"model">>, <<"auth">>, <<"post">>, <<"logoff">> ],
                                #{},
                                Context),
                            % Give the connection a bit of time to send the message to the client
                            % and then kill this MQTT session.
                            timer:apply_after(1000, mqtt_sessions_process, kill, [SessionPid])
                    end
                end)
    end.

-spec connect( mqtt_packet_map:mqtt_packet(), boolean(), mqtt_sessions:msg_options(), z:context()) -> {ok, mqtt_packet_map:mqtt_packet(), z:context()} | {error, term()}.
connect(#{ type := connect, username := U, password := P, properties := Props }, false,
        #{ context_prefs := #{ user_id := UserId } = Prefs } = Options,
        Context) when ?none(U), ?none(P) ->
    % No session, accept user from the mqtt controller
    AuthOptions = maps:get(auth_options, Prefs, #{}),
    Context1 = set_connect_context_options(Options, Context),
    {ConnAckProps, Context2} = set_connect_properties(Props, Context1),
    Context3 = if
        UserId =:= undefined ->
            Context2;
        true ->
            CtxUser = z_acl:logon(UserId, AuthOptions, Context2),
            subscribe_forced_logoff(CtxUser),
            CtxUser
    end,
    z_auth:publish_user_session(Context3),
    ConnAck = #{
        type => connack,
        reason_code => ?MQTT_RC_SUCCESS,
        properties => ConnAckProps
    },
    z_context:logger_md(Context3),
    {ok, ConnAck, Context3};
connect(#{ type := connect, username := U, password := P }, true,
        #{ context_prefs := #{ user_id := UserId } } = Options,
        Context) when ?none(U), ?none(P) ->
    % Existing session, user from the mqtt controller must be the user in the mqtt session
    case z_acl:user(Context) of
        UserId ->
            ConnAck = #{
                type => connack,
                reason_code => ?MQTT_RC_SUCCESS
            },
            Context1 = set_connect_context_options(Options, Context),
            z_context:logger_md(Context1),
            {ok, ConnAck, Context1};
        _SomeOtherUser ->
            ConnAck = #{
                type => connack,
                reason_code => ?MQTT_RC_NOT_AUTHORIZED
            },
            {ok, ConnAck, Context}
    end;
connect(#{ type := connect, username := U, password := P }, _IsSessionPresent, Options, Context) when ?none(U), ?none(P) ->
    % Anonymous login, and no user from the MQTT controller
    case z_acl:user(Context) of
        undefined ->
            ConnAck = #{
                type => connack,
                reason_code => ?MQTT_RC_SUCCESS
            },
            Context1 = set_connect_context_options(Options, Context),
            z_context:logger_md(Context1),
            {ok, ConnAck, Context1};
        _SomeUser ->
            ConnAck = #{
                type => connack,
                reason_code => ?MQTT_RC_NOT_AUTHORIZED
            },
            {ok, ConnAck, Context}
    end;
connect(#{ type := connect, username := U, password := P, properties := Props }, IsSessionPresent, Options, Context) when not ?none(U), not ?none(P) ->
    % User login, and no user from the MQTT controller
    % The username might be something like: "example.com:localuser"
    Username = case split_vhost_username(U) of
                   [ _VHost, U1 ] -> U1;
                   _ -> U
               end,
    LogonArgs = #{
        <<"username">> => Username,
        <<"password">> => P
    },
    case z_notifier:first(#logon_submit{ payload = LogonArgs }, Context) of
        {ok, UserId} when is_integer(UserId) ->
            % Authentication is successful, accept if user not changed
            % or this is a new session.
            IsAuthOk = not IsSessionPresent
                       orelse z_acl:user(Context) =:= UserId,
            case IsAuthOk of
                true ->
                    Context1 = set_connect_context_options(Options, Context),
                    {ConnAckProps, Context2} = set_connect_properties(Props, Context1),
                    case z_auth:logon(UserId, Context2) of
                        {ok, ContextAuth} ->
                            ConnAck = #{
                                type => connack,
                                reason_code => ?MQTT_RC_SUCCESS,
                                properties => ConnAckProps
                            },
                            z_auth:publish_user_session(ContextAuth),
                            subscribe_forced_logoff(ContextAuth),
                            z_context:logger_md(ContextAuth),
                            {ok, ConnAck, ContextAuth};
                        {error, user_not_enabled} ->
                            ?LOG_INFO(#{
                                in => zotonic_core,
                                text => <<"Logon attempt of disabled user">>,
                                user_id => UserId,
                                result => error,
                                reason => user_not_enabled,
                                protocol => mqtt
                            }),
                            ConnAck = #{
                                type => connack,
                                reason_code => ?MQTT_RC_NOT_AUTHORIZED
                            },
                            {ok, ConnAck, Context}
                    end;
                false ->
                    ?LOG_INFO(#{
                        in => zotonic_core,
                        text => <<"Logon attempt of different user for existing in session">>,
                        user_id => UserId,
                        session_user_id => z_acl:user(Context),
                        result => error,
                        reason => user_mismatch,
                        protocol => mqtt
                    }),
                    ConnAck = #{
                        type => connack,
                        reason_code => ?MQTT_RC_NOT_AUTHORIZED
                    },
                    {ok, ConnAck, Context}
            end;
        {error, _Reason} ->
            ConnAck = #{
                type => connack,
                reason_code => ?MQTT_RC_BAD_USERNAME_PASSWORD
            },
            {ok, ConnAck, Context};
        undefined ->
            ?LOG_WARNING(#{
                in => zotonic_core,
                text => <<"Auth module error: #logon_submit{} returned undefined.">>,
                protocol => mqtt
            }),
            ConnAck = #{
                type => connack,
                reason_code => ?MQTT_RC_ERROR
            },
            {ok, ConnAck, Context}
    end;
connect(#{ type := connect, properties := #{ authentication_method := _AuthMethod } = Props }, _IsSessionPresent, _Options, Context) ->
    % User logs on using extended authentication method
    _AuthData = maps:get(authentication_data, Props, undefined),
    % ... handle extended authentication handshake
    ConnAck = #{
        type => connack,
        reason_code => ?MQTT_RC_NOT_AUTHORIZED
    },
    {ok, ConnAck, Context};
connect(_Packet, _IsSessionPresent, _Options, Context) ->
    % Extended authentication
    ConnAck = #{
        type => connack,
        reason_code => ?MQTT_RC_NOT_AUTHORIZED
    },
    {ok, ConnAck, Context}.


%% @doc Re-authentication. This is called when the client requests a re-authentication (or replies in a AUTH re-authentication).
-spec reauth( mqtt_packet_map:mqtt_packet(), z:context()) -> {ok, mqtt_packet_map:mqtt_packet(), z:context()} | {error, term()}.
reauth(#{ type := _auth }, _Context) ->
    {error, notsupported}.


-spec is_allowed( publish | subscribe, mqtt_sessions_runtime:topic(), mqtt_packet_map:mqtt_packet(), z:context()) -> boolean().
is_allowed(Action, Topic, Packet, Context) when Action =:= subscribe; Action =:= publish ->
    z_stats:record_event(broker, Action, Context),
    is_allowed(z_acl:is_admin(Context), Action, Topic, Packet, Context).

is_allowed(true, publish, _Topic, _Packet, _Context) ->
    true;
is_allowed(true, subscribe, Topic, _Packet, Context) ->
    is_allowed_admin_subscribe(Topic, Context);
is_allowed(false, Action, Topic, Packet, Context) ->
    is_allowed_acl(Action, Topic, Packet, Context).

% Check if it is allowed for the admin to subscribe to a topic.
is_allowed_admin_subscribe([<<"$SYS">>, <<"site">>, Site | _], Context) ->
    case z_context:site(Context) of
        zotonic_status_site ->
            % admin of the status site is allowed to subscribe to sys
            % topics of all sites.
            true;
        Host ->
            % A normal site admin is allowed to subscribe to site specific
            % sys topics.
            Site =:= z_convert:to_binary(Host)
    end;
is_allowed_admin_subscribe([<<"$SYS">>, <<"erlang">> | _], _Context) ->
    % and to zotonic node wide erlang topics
    true;
is_allowed_admin_subscribe([<<"$SYS">> | _], _Context) ->
    % other sys topics are disallowed
    false;
is_allowed_admin_subscribe(_, _Context) ->
    true.

is_allowed_acl(_Action, [ '#' ], _Packet, _Context) ->
    false;
is_allowed_acl(Action, Topic, Packet, Context) ->
    Object = #acl_mqtt{
        topic = Topic,
        is_wildcard = lists:any(fun is_wildcard/1, Topic),
        packet = Packet
    },
    case z_acl:maybe_allowed(Action, Object, Context) of
        true -> true;
        false -> false;
        undefined -> is_allowed(Action, Topic, Context)
    end.


is_allowed(_Action,   [ <<"test">> | _ ], _Context) -> true;
is_allowed(_Action,   [ <<"public">> | _ ], _Context) -> true;
is_allowed(_Action,   [ <<"reply">>, <<"call-", _/binary>>, _ ], _Context) -> true;
% Models MUST implement their own access control for get/ post/ delete
is_allowed(publish,   [ <<"model">>, _Model, <<"get">> | _ ], _Context) -> true;
is_allowed(publish,   [ <<"model">>, _Model, <<"post">> | _ ], _Context) -> true;
is_allowed(publish,   [ <<"model">>, _Model, <<"delete">> | _ ], _Context) -> true;
% End generic models ACL
is_allowed(publish,   [ <<"model">>, Model,  <<"event">>, Id | _ ], Context)
    when Model =:= <<"rsc">>;
         Model =:= <<"media">>;
         Model =:= <<"identity">> ->
    z_acl:rsc_editable( m_rsc:rid(Id, Context), Context );
is_allowed(subscribe, [ <<"model">>, Model,  <<"event">>, Id | _ ], Context)
    when Model =:= <<"rsc">>;
         Model =:= <<"media">>;
         Model =:= <<"identity">> ->
    case is_wildcard(Id) of
        true -> false;
        false -> z_acl:rsc_visible( m_rsc:rid(Id, Context), Context )
    end;
% Model events can be view iff the user has use permission on the module
is_allowed(subscribe, [ <<"model">>, Model, <<"event">> | _ ], Context) ->
    try
        case z_module_indexer:find(model, Model, Context) of
            {ok, #module_index{ module = zotonic_core }} ->
                true;
            {ok, #module_index{ module = Module }} ->
                z_acl:is_allowed(use, Module, Context);
            {error, enoent} ->
                false
        end
    catch
        _:_ ->
            false
    end;
is_allowed(publish,   [ <<"bridge">>, _Remote, <<"reply">> | _ ], _Context) -> true;
is_allowed(publish,   [ <<"bridge">>, _Remote, <<"public">> | _ ], _Context) -> true;
is_allowed(_Action,   [ <<"bridge">>, Remote | _ ], Context) when is_binary(Remote) ->
    % User of the bridge session MUST be the same as the user publishing
    case is_wildcard(Remote) of
        true -> false;
        false ->
            case       Context#context.routing_id =:= Remote
                orelse Context#context.client_id  =:= Remote
            of
                true ->
                    true;
                false ->
                    case z_auth:is_auth(Context) of
                        true ->
                            % Only works if remote is a client-id
                            case mqtt_sessions:get_user_context(z_context:site(Context), Remote) of
                                {ok, UserContext} ->
                                    z_acl:user(UserContext) =:= z_acl:user(Context);
                                {error, _} ->
                                    false
                            end;
                        false ->
                            false
                    end
            end
    end;
is_allowed(_Action,   [ <<"client">>, ClientId | _ ], Context) when is_binary(ClientId) ->
    case is_wildcard(ClientId) of
        true -> false;
        false -> Context#context.client_id =:= ClientId
    end;
is_allowed(subscribe, [ <<"user">> ], Context) -> z_auth:is_auth(Context);
is_allowed(_Action,   [ <<"user">>, User | _ ], Context) when is_binary(User); is_integer(User) ->
    try
        UserId = z_convert:to_integer(User),
        UserId =:= z_acl:user(Context)
        orelse z_acl:rsc_editable(UserId, Context)
    catch
        _:_ -> false
    end;
is_allowed(_Action, _Topic, _Context) ->
    false.

is_wildcard('+') -> true;
is_wildcard('#') -> true;
is_wildcard(<<"+">>) -> true;
is_wildcard(<<"#">>) -> true;
is_wildcard(B) when is_binary(B) -> false;
is_wildcard(B) when is_integer(B) -> false.

%% @doc If the connection is authenticated, then the connection user MUST be the session user.
-spec is_valid_message( mqtt_packet_map:mqtt_packet(), mqtt_sessions:msg_options(), z:context() ) -> boolean().
is_valid_message(_Msg, #{ auth_user_id := UserId }, Context) ->
    case z_acl:user(Context) of
        UserId -> true;
        _ -> false
    end;
is_valid_message(_Msg, _Options, _Context) ->
    true.

split_vhost_username(Username) ->
    case z_config:get(mqtt_username_format_0x, false) of
        true ->
            % "example.com:localuser"  (Zotonic 1.x format)
            % "localuser@examplesite"  (Zotonic 0.x format)
            split_vhost_username(Username, Username);
        false ->
            % "example.com:localuser"  (Zotonic 1.x format)
            binary:split(Username, <<":">>)
    end.

split_vhost_username(<<>>, Username) ->
    % Username without designation
    [Username];
split_vhost_username(<<":", _/binary>>, Username) ->
    % "example.com:localuser"  (Zotonic 1.x format)
    binary:split(Username, <<":">>);
split_vhost_username(<<"@", _/binary>>, Username) ->
    % "localuser@examplesite"  (Zotonic 0.x format)
    [Username, VHost] = binary:split(Username, <<"@">>),
    [VHost, Username];
split_vhost_username(<<_/utf8, R/binary>>, Username) ->
    split_vhost_username(R, Username).
