%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Store information in the localStorage on the client.

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

-module(m_client_local_storage).

-export([
    get/2,
    get/3,
    put/3,
    put/4,
    delete/2,
    delete/3,

    device_id/1
]).

-type key() :: binary() | atom().

-type value() :: number()
               | boolean()
               | binary()
               | #{ key() => value() }
               | tuple().

-type error() :: timeout
               | no_client
               | invalid_topic.

-spec get( key(), z:context() ) -> {ok, value()} | {error, error()}.
get(Key, Context) ->
    get(Key, z_context:client_topic(Context), Context).

-spec get( key(), mqtt_sessions:topic(), z:context() ) -> {ok, value()} | {error, error()}.
get(_Key, undefined, _Context) ->
    {error, no_client};
get(Key, BridgeTopic, Context) ->
    BridgeTopic1 = mqtt_packet_map_topic:normalize_topic(BridgeTopic),
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"localStorage">>, <<"get">>, z_convert:to_binary(Key) ],
    z_mqtt:call(Topic, undefined, Context).

-spec put( key(), value(), z:context() ) -> ok | {error, error()}.
put(Key, Value, Context) ->
    put(Key, Value, z_context:client_topic(Context), Context).

-spec put( key(), value(), mqtt_sessions:topic(), z:context() ) -> ok | {error, error()}.
put(_Key, _Value, undefined, _Context) ->
    {error, no_client};
put(Key, Value, BridgeTopic, Context) ->
    BridgeTopic1 = mqtt_packet_map_topic:normalize_topic(BridgeTopic),
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"localStorage">>, <<"post">>, z_convert:to_binary(Key) ],
    z_mqtt:publish(Topic, Value, Context).


-spec delete( key(), z:context() ) -> ok | {error, error()}.
delete(Key, Context) ->
    delete(Key, z_context:client_topic(Context), Context).

-spec delete( key(), mqtt_sessions:topic(), z:context() ) -> ok | {error, error()}.
delete(_Key, undefined, _Context) ->
    {error, no_client};
delete(Key, BridgeTopic, Context) ->
    BridgeTopic1 = mqtt_packet_map_topic:normalize_topic(BridgeTopic),
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"localStorage">>, <<"delete">>, z_convert:to_binary(Key) ],
    z_mqtt:publish(Topic, undefined, Context).


-spec device_id( z:context() ) -> {{ok, binary()}, z:context()} | {{error, error()}, z:context()}.
device_id(Context) ->
    case z_memo:get(z_device_id) of
        undefined ->
            RC = {Result, _Context1} = device_id_1(Context),
            z_memo:set(z_device_id, Result),
            RC;
        Result ->
            {Result, Context}
    end.

device_id_1(Context) ->
    case z_context:get(z_device_id, Context) of
        {ok, _} = OK ->
            {OK, Context};
        {error, _} = Error ->
            {Error, Context};
        undefined ->
            Result = case get(<<"z.deviceId">>, Context) of
                {ok, DeviceId} when is_binary(DeviceId), DeviceId =/= <<>> ->
                    {ok, DeviceId};
                {ok, _} ->
                    set_device_id(Context);
                {error, _} = Error ->
                    Error
            end,
            Context1 = z_context:set(z_device_id, Result, Context),
            {Result, Context1}
    end.

set_device_id(Context) ->
    Id = z_ids:id(),
    case put(<<"z.deviceId">>, Id, Context) of
        ok ->
            {ok, Id};
        {error, _} = Error ->
            Error
    end.
