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
    delete/3
]).

-type key() :: binary() | atom().

-type value() :: number()
               | boolean()
               | binary()
               | #{ key() => value() }
               | tuple().

-type error() :: timeout
               | no_client.

-spec get( key(), z:context() ) -> {ok, value()} | {error, error()}.
get(Key, Context) ->
    get(Key, z_context:client_topic(Context), Context).

-spec get( key(), mqtt_sessions:topic(), z:context() ) -> {ok, value()} | {error, error()}.
get(_Key, undefined, _Context) ->
    {error, no_client_id};
get(Key, BridgeTopic, Context) ->
    BridgeTopic1 = mqtt_sessions:normalize_topic(BridgeTopic),
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"localStorage">>, <<"get">>, z_convert:to_binary(Key) ],
    z_mqtt:call(Topic, undefined, Context).

-spec put( key(), value(), z:context() ) -> ok | {error, error()}.
put(Key, Value, Context) ->
    put(Key, Value, z_context:client_topic(Context), Context).

-spec put( key(), value(), mqtt_sessions:topic(), z:context() ) -> ok | {error, error()}.
put(Key, Value, BridgeTopic, Context) ->
    BridgeTopic1 = mqtt_sessions:normalize_topic(BridgeTopic),
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"localStorage">>, <<"post">>, z_convert:to_binary(Key) ],
    z_mqtt:publish(Topic, Value, Context).


-spec delete( key(), z:context() ) -> ok | {error, error()}.
delete(Key, Context) ->
    delete(Key, z_context:client_topic(Context), Context).

-spec delete( key(), mqtt_sessions:topic(), z:context() ) -> ok | {error, error()}.
delete(Key, BridgeTopic, Context) ->
    BridgeTopic1 = mqtt_sessions:normalize_topic(BridgeTopic),
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"localStorage">>, <<"delete">>, z_convert:to_binary(Key) ],
    z_mqtt:publish(Topic, undefined, Context).
