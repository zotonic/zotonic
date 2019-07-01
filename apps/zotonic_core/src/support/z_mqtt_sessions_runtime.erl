%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018-2019 Marc Worrell
%% @doc Zotonic specific callbacks for MQTT connections

%% Copyright 2018-2019 Marc Worrell
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
    connect/2,
    reauth/2,
    is_allowed/4,
    is_valid_message/3,

    context_updates/3
    ]).

-behaviour(mqtt_sessions_runtime).

-define(none(A), (A =:= undefined orelse A =:= <<>>)).

-include_lib("mqtt_packet_map/include/mqtt_packet_map.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


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

-spec new_user_context( atom(), binary(), mqtt_sessions:session_options() ) -> z:context().
new_user_context( Site, ClientId, SessionOptions ) ->
    Context = z_context:new(Site),
    Context1 = Context#context{
        client_topic = [ <<"bridge">>, ClientId ],
        client_id = ClientId,
        routing_id = maps:get(routing_id, SessionOptions)
    },
    Prefs = maps:get(context_prefs, SessionOptions, #{}),
    Context2 = case maps:get(language, Prefs, undefined) of
        undefined -> Context1;
        Language -> z_context:set_language(Language, Context1)
    end,
    Context3 = case maps:get(timezone, Prefs, undefined) of
        undefined -> Context2;
        Tz -> z_context:set_tz(Tz, Context2)
    end,
    AuthOptions = maps:get(auth_options, Prefs, #{}),
    Context4 = z_context:set(auth_options, Prefs, Context3),
    Context5 = case Prefs of
        #{ user_id := undefined } ->
            % Anonymous user on the transport -- typical for anonymous HTTP connection
            Context4;
        #{ user_id := UserId } ->
            % User authenticated on the transport -- typical for HTTP connection with auth cookie
            z_acl:logon_prefs(UserId, AuthOptions, Context4);
        #{ } ->
            % No user on the transport - typical for a MQTT connection
            Context4
    end,
    mqtt_sessions:subscribe(
        Site,
        [ <<"client">>, ClientId, <<"config">> ],
        {?MODULE, context_updates, [Site, ClientId]},
        z_acl:sudo(Context4)),
    z_context:set(peer_ip, maps:get(peer_ip, SessionOptions, undefined), Context5).

context_updates(Site, ClientId, #{ message := #{ payload := Payload} }) ->
    mqtt_sessions:update_user_context(Site, ClientId, fun(Ctx) -> update_context_fun(Payload, Ctx) end).

update_context_fun(Payload, Context) ->
    Context1 = maybe_set_language(Payload, Context),
    maybe_set_timezone(Payload, Context1).

maybe_set_language(#{ <<"language">> := <<>> }, Context) ->
    Context;
maybe_set_language(#{ <<"language">> := Lang }, Context) when is_binary(Lang) ->
    try
        mod_translation:set_language(Lang, Context)
    catch
        error:undef ->
            z_context:set_language(Lang, Context)
    end;
maybe_set_language(_Payload, Context) ->
    Context.

maybe_set_timezone(#{ <<"timezone">> := <<>> }, Context) ->
    Context;
maybe_set_timezone(#{ <<"timezone">> := Timezone }, Context) when is_binary(Timezone) ->
    z_context:set_tz(Timezone, Context);
maybe_set_timezone(_Payload, Context) ->
    Context.


-spec connect( mqtt_packet_map:mqtt_packet(), z:context()) -> {ok, mqtt_packet_map:mqtt_packet(), z:context()} | {error, term()}.
connect(#{ type := connect, username := U, password := P }, Context) when ?none(U), ?none(P) ->
    % Anonymous login -- continue with the user we have (from http ws cookies etc)
    ConnAck = #{
        type => connack,
        reason_code => ?MQTT_RC_SUCCESS
    },
    {ok, ConnAck, Context};
connect(#{ type := connect, username := U, password := P }, Context) when not ?none(U), not ?none(P) ->
    Username = case binary:split(U, <<":">>) of
        [ _VHost, U1 ] -> U1;
        U1 -> U1
    end,
    LogonArgs = #{
        <<"username">> => Username,
        <<"password">> => P
    },
    case z_notifier:first(#logon_submit{ payload = LogonArgs }, Context) of
        {ok, UserId} when is_integer(UserId) ->
            IsAuthOk = not z_auth:is_auth(Context)
                       orelse z_acl:user(Context) =:= UserId,
            case IsAuthOk of
                true ->
                    Context1 = z_acl:logon(UserId, Context),
                    ConnAck = #{
                        type => connack,
                        reason_code => ?MQTT_RC_SUCCESS,
                        properties => #{
                            % TODO: also return a token that can be exchanged for a cookie
                            % ... token ...
                        }
                    },
                    {ok, ConnAck, Context1};
                false ->
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
            lager:warning("Auth module error: #logon_submit{} returned undefined."),
            ConnAck = #{
                type => connack,
                reason_code => ?MQTT_RC_ERROR
            },
            {ok, ConnAck, Context}
    end;
connect(#{ type := connect, properties := #{ authentication_method := _AuthMethod } = Props }, Context) ->
    % User logs on using extended authentication method
    _AuthData = maps:get(authentication_data, Props, undefined),
    % ... handle extended authentication handshake
    ConnAck = #{
        type => connack,
        reason_code => ?MQTT_RC_NOT_AUTHORIZED
    },
    {ok, ConnAck, Context};
connect(_Packet, Context) ->
    % Extended authentication
    ConnAck = #{
        type => connack,
        reason_code => ?MQTT_RC_NOT_AUTHORIZED
    },
    {ok, ConnAck, Context}.


%% @spec Re-authentication. This is called when the client requests a re-authentication (or replies in a AUTH re-authentication).
-spec reauth( mqtt_packet_map:mqtt_packet(), z:context()) -> {ok, mqtt_packet_map:mqtt_packet(), z:context()} | {error, term()}.
reauth(#{ type := auth }, _UserContext) ->
    {error, notsupported}.


-spec is_allowed( publish | subscribe, mqtt_sessions_runtime:topic(), mqtt_packet_map:mqtt_packet(), z:context()) -> boolean().
is_allowed(Action, Topic, Packet, Context) when Action =:= subscribe; Action =:= publish ->
    z_acl:is_admin(Context) orelse is_allowed_acl(Action, Topic, Packet, Context).

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
%% TODO: change the model access to NOT allowed per default (ACL should allow/disallow access)
is_allowed(publish,   [ <<"model">>, _Model, <<"get">> | _ ], _Context) -> true;
is_allowed(publish,   [ <<"model">>, _Model, <<"post">> | _ ], _Context) -> true;
is_allowed(publish,   [ <<"model">>, _Model, <<"delete">> | _ ], _Context) -> true;
is_allowed(subscribe, [ <<"model">>, _Model, <<"event">> | _ ], _Context) -> true;
%% End todo.
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

