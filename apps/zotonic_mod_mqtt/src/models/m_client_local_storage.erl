%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2026 Marc Worrell
%% @doc Store information in the localStorage on the client.
%% @end

%% Copyright 2019-2026 Marc Worrell
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

-module(m_client_local_storage).
-moduledoc("
Model to access the `localStorage` on the client (browser).

The client-id and routing topic in the *context* must be set when calling the functions in this module. This is the case
for all MQTT topic calls from the client.

The localStorage on the client is accessed via publishing to the topic `~client/model/localStorage/...`. In the client
the topic is `model/localStorage/...`.
").

-export([
    get/2,
    get/3,
    put/3,
    put/4,

    get_secure/2,
    put_secure/4,

    delete/2,
    delete/3,

    fetch_device_id/1,
    ensure_device_id/1
]).

-type key() :: binary() | atom().

-type value() :: number()
               | boolean()
               | binary()
               | #{ key() => value() }
               | tuple().

-type error() :: timeout
               | no_client
               | invalid_topic
               | forged
               | badarg
               | expired.

-define(SECRET_LENGTH, 32).
-define(DEVICE_ID_LENGTH, 20).


-spec get( key(), z:context() ) -> {ok, value()} | {error, error()}.
get(Key, Context) ->
    case z_context:client_topic(Context) of
        {ok, Topic} -> get(Key, Topic, Context);
        {error, _} = Error -> Error
    end.

-spec get( key(), mqtt_sessions:topic(), z:context() ) -> {ok, value()} | {error, error()}.
get(_Key, undefined, _Context) ->
    {error, no_client};
get(Key, BridgeTopic, Context) ->
    BridgeTopic1 = mqtt_packet_map_topic:normalize_topic(BridgeTopic),
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"localStorage">>, <<"get">>, z_convert:to_binary(Key) ],
    z_mqtt:call(Topic, undefined, Context).

-spec put( key(), value(), z:context() ) -> ok | {error, error()}.
put(Key, Value, Context) ->
    case z_context:client_topic(Context) of
        {ok, Topic} -> put(Key, Value, Topic, Context);
        {error, _} = Error -> Error
    end.

-spec put_secure( key(), value(), pos_integer(), z:context() ) -> ok | {error, error()}.
put_secure(Key, Value, TTL, Context) ->
    ExpTerm = termit:expiring(Value, TTL),
    Encoded = termit:encode_base64(ExpTerm, secret(Context)),
    put(Key, Encoded, Context).

-spec get_secure( key(), z:context() ) -> {ok, term()} | {error, error()}.
get_secure(Key, Context) ->
    case get(Key, Context) of
        {ok, Encoded} ->
            case termit:decode_base64(Encoded, secret(Context)) of
                {ok, ExpTerm} ->
                    termit:check_expired(ExpTerm);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

secret(Context) ->
    case m_config:get_value(mod_mqtt, local_storage_secret, Context) of
        <<>> -> generate_local_storage_secret(Context);
        undefined -> generate_local_storage_secret(Context);
        Secret -> Secret
    end.

-spec generate_local_storage_secret( z:context() ) -> binary().
generate_local_storage_secret(Context) ->
    Secret = z_ids:id(?SECRET_LENGTH),
    m_config:set_value(mod_mqtt, local_storage_secret, Secret, Context),
    Secret.

-spec put( key(), value(), mqtt_sessions:topic(), z:context() ) -> ok | {error, error()}.
put(_Key, _Value, undefined, _Context) ->
    {error, no_client};
put(Key, Value, BridgeTopic, Context) ->
    BridgeTopic1 = mqtt_packet_map_topic:normalize_topic(BridgeTopic),
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"localStorage">>, <<"post">>, z_convert:to_binary(Key) ],
    z_mqtt:publish(Topic, Value, Context).


-spec delete( key(), z:context() ) -> ok | {error, error()}.
delete(Key, Context) ->
    case z_context:client_topic(Context) of
        {ok, Topic} -> delete(Key, Topic, Context);
        {error, _} = Error -> Error
    end.

-spec delete( key(), mqtt_sessions:topic(), z:context() ) -> ok | {error, error()}.
delete(_Key, undefined, _Context) ->
    {error, no_client};
delete(Key, BridgeTopic, Context) ->
    BridgeTopic1 = mqtt_packet_map_topic:normalize_topic(BridgeTopic),
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"localStorage">>, <<"delete">>, z_convert:to_binary(Key) ],
    z_mqtt:publish(Topic, undefined, Context).


%% @doc Return the unique device id. Might fetch it from the local storage of the
%% user agent. The device id is cached in the process memo cache.
-spec fetch_device_id( z:context() ) -> {ok, binary()} | {error, error()}.
fetch_device_id(Context) ->
    case z_memo:get(z_device_id) of
        undefined ->
            case z_context:get(z_device_id, Context) of
                {ok, _} = OK ->
                    OK;
                {error, _} = Error ->
                    Error;
                undefined ->
                    Result = get_device_id(Context),
                    z_memo:set(z_device_id, Result),
                    Result;
                Result ->
                    Result
            end;
        Result ->
            Result
    end.

%% @doc Return the unique device id. Might generate a new id and store it
%% in the local storage of the user-agent.
-spec ensure_device_id( z:context() ) -> {{ok, binary()}, z:context()} | {{error, error()}, z:context()}.
ensure_device_id(Context) ->
    case fetch_device_id(Context) of
        {ok, _} = Result ->
            {Result, Context};
        {error, enoent} ->
            Result = set_device_id(Context),
            z_memo:set(z_device_id, Result),
            {Result, z_context:set(z_device_id, Result, Context)};
        {error, _} = Error ->
            {Error, Context}
    end.

get_device_id(Context) ->
    case get(<<"z.deviceId">>, Context) of
        {ok, <<DeviceId:?DEVICE_ID_LENGTH/binary>>} ->
            {ok, DeviceId};
        {ok, _} ->
            {error, enoent};
        {error, _} = Error ->
            Error
    end.

set_device_id(Context) ->
    Id = z_ids:id(?DEVICE_ID_LENGTH),
    case put(<<"z.deviceId">>, Id, Context) of
        ok ->
            {ok, Id};
        {error, _} = Error ->
            Error
    end.
