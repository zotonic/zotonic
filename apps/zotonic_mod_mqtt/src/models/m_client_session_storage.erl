%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Store information in the sessionStorage on the client.

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


-module(m_client_session_storage).

-export([
    get/2,
    get/3,
    put/3,
    put/4,
    delete/2,
    delete/3,

    get_subkey/3,
    get_subkey/4,
    put_subkey/4,
    put_subkey/5,
    delete_subkey/3,
    delete_subkey/4
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
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"sessionStorage">>, <<"get">>, z_convert:to_binary(Key) ],
    z_mqtt:call(Topic, undefined, Context).

-spec put( key(), value(), z:context() ) -> ok | {error, error()}.
put(Key, Value, Context) ->
    put(Key, Value, z_context:client_topic(Context), Context).

-spec put( key(), value(), mqtt_sessions:topic(), z:context() ) -> ok | {error, error()}.
put(_Key, _Value, undefined, _Context) ->
    {error, no_client};
put(Key, Value, BridgeTopic, Context) ->
    BridgeTopic1 = mqtt_packet_map_topic:normalize_topic(BridgeTopic),
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"sessionStorage">>, <<"post">>, z_convert:to_binary(Key) ],
    z_mqtt:publish(Topic, Value, Context).


-spec delete( key(), z:context() ) -> ok | {error, error()}.
delete(Key, Context) ->
    delete(Key, z_context:client_topic(Context), Context).

-spec delete( key(), mqtt_sessions:topic(), z:context() ) -> ok | {error, error()}.
delete(_Key, undefined, _Context) ->
    {error, no_client};
delete(Key, BridgeTopic, Context) ->
    BridgeTopic1 = mqtt_packet_map_topic:normalize_topic(BridgeTopic),
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"sessionStorage">>, <<"delete">>, z_convert:to_binary(Key) ],
    z_mqtt:publish(Topic, undefined, Context).



-spec get_subkey( key(), key(), z:context() ) -> {ok, value()} | {error, error()}.
get_subkey(Key, SubKey, Context) ->
    get_subkey(Key, SubKey, z_context:client_topic(Context), Context).

-spec get_subkey( key(), key(), mqtt_sessions:topic(), z:context() ) -> {ok, value()} | {error, error()}.
get_subkey(_Key, _SubKey, undefined, _Context) ->
    {error, no_client};
get_subkey(Key, SubKey, BridgeTopic, Context) ->
    BridgeTopic1 = mqtt_packet_map_topic:normalize_topic(BridgeTopic),
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"sessionStorage">>, <<"get">>, z_convert:to_binary(Key), z_convert:to_binary(SubKey) ],
    z_mqtt:call(Topic, undefined, Context).

-spec put_subkey( key(), key(), value(), z:context() ) -> ok | {error, error()}.
put_subkey(Key, SubKey, Value, Context) ->
    put_subkey(Key, SubKey, Value, z_context:client_topic(Context), Context).

-spec put_subkey( key(), key(), value(), mqtt_sessions:topic(), z:context() ) -> ok | {error, error()}.
put_subkey(_Key, _SubKey, _Value, undefined, _Context) ->
    {error, no_client};
put_subkey(Key, SubKey, Value, BridgeTopic, Context) ->
    BridgeTopic1 = mqtt_packet_map_topic:normalize_topic(BridgeTopic),
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"sessionStorage">>, <<"post">>, z_convert:to_binary(Key), z_convert:to_binary(SubKey) ],
    z_mqtt:publish(Topic, Value, Context).


-spec delete_subkey( key(), key(), z:context() ) -> ok | {error, error()}.
delete_subkey(Key, SubKey, Context) ->
    delete_subkey(Key, SubKey, z_context:client_topic(Context), Context).

-spec delete_subkey( key(), key(), mqtt_sessions:topic(), z:context() ) -> ok | {error, error()}.
delete_subkey(_Key, _Subkey, undefined, _Context) ->
    {error, no_client};
delete_subkey(Key, SubKey, BridgeTopic, Context) ->
    BridgeTopic1 = mqtt_packet_map_topic:normalize_topic(BridgeTopic),
    Topic = BridgeTopic1 ++ [ <<"model">>, <<"sessionStorage">>, <<"delete">>, z_convert:to_binary(Key), z_convert:to_binary(SubKey) ],
    z_mqtt:publish(Topic, undefined, Context).
