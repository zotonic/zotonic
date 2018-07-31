%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell

%% @doc Acces control for MQTT topics

%% Copyright 2018 Marc Worrell
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

%% @doc MQTT sessions runtime ACL interface.
%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell

%% Copyright 2018 Marc Worrell
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
    new_user_context/3,
    connect/2,
    reauth/2,
    is_allowed/4
    ]).

-behaviour(mqtt_sessions_runtime).

-define(none(A), (A =:= undefined orelse A =:= <<>>)).

-include_lib("mqtt_packet_map/include/mqtt_packet_map.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


% TODO: differentiate publish and subscribe on bridged reply/public topics
% TODO: check authentication credentials
% TODO: if reconnect, check against previous credentials (MUST be the same)

-spec new_user_context( atom(), binary(), mqtt_sessions:session_options() ) -> z:context().
new_user_context( Site, ClientId, Options ) ->
    Context = z_context:new(Site),
    Context1 = Context#context{
        client_topic = [ <<"bridge">>, ClientId ],
        client_id = ClientId,
        routing_id = maps:get(routing_id, Options)
    },
    z_context:set(peer_ip, maps:get(peer_ip, Options, undefined), Context1).


-spec connect( mqtt_packet_map:mqtt_packet(), z:context()) -> {ok, mqtt_packet_map:mqtt_packet(), z:context()} | {error, term()}.
connect(#{ type := connect, username := U, password := P }, Context) when ?none(U), ?none(P) ->
    % Anonymous login
    ConnAck = #{
        type => connack,
        reason_code => ?MQTT_RC_SUCCESS
    },
    {ok, ConnAck, Context#context{ user_id = undefined, acl = undefined }};
connect(#{ type := connect, username := U, password := P }, Context) when not ?none(U), not ?none(P) ->
    LogonArgs = [
        {<<"username">>, U},
        {<<"password">>, P}
    ],
    case z_notifier:first(#logon_submit{ query_args = LogonArgs }, Context) of
        {ok, UserId} when is_integer(UserId) ->
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
connect(#{ type := connect, properties := #{ authentication_method := AuthMethod } = Props }, Context) ->
    % User logs on using extended authentication method
    AuthData = maps:get(authentication_data, Props, undefined),
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
is_allowed(publish,   [ <<"model">>, _Model, <<"get">> | _ ], _Context) -> true;
is_allowed(publish,   [ <<"model">>, _Model, <<"post">> | _ ], _Context) -> true;
is_allowed(publish,   [ <<"model">>, _Model, <<"delete">> | _ ], _Context) -> true;
is_allowed(subscribe, [ <<"model">>, _Model, <<"event">> | _ ], _Context) -> true;
is_allowed(publish,   [ <<"bridge">>, _Remote, <<"reply">> | _ ], _Context) -> true;
is_allowed(publish,   [ <<"bridge">>, _Remote, <<"public">> | _ ], _Context) -> true;
is_allowed(subscribe, [ <<"bridge">>, Remote | _ ], Context) ->
    Context#context.routing_id =:= Remote orelse Context#context.client_id =:= Remote;
is_allowed(subscribe, [ <<"user">> ], Context) -> z_auth:is_auth(Context);
is_allowed(_Action,   [ <<"user">>, User | _ ], Context) ->
    try
        UserId = binary_to_integer(User),
        UserId =:= z_acl:user(Context)
        orelse z_acl:rsc_editable(UserId, Context)
    catch
        _:_ -> false
    end;
is_allowed(_Action, _Topic, _Context) ->
    false.

is_wildcard(<<"+">>) -> true;
is_wildcard(<<"#">>) -> true;
is_wildcard(B) when is_binary(B) -> false.

