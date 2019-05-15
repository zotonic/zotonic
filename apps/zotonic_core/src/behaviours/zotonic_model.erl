%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2018 Marc Worrell
%% @doc Model behaviour

%% Copyright 2009-2018 Marc Worrell
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

-module(zotonic_model).
-author("Marc Worrell <marc@worrell.nl").

-type opt_msg() :: mqtt_packet_map:mqtt_packet() | undefined.
-type return() :: {ok, {term(), list()}} | {error, unknown_path | term()}.

-export_type([ opt_msg/0, return/0 ]).

%% Called from templates, MQTT and API
-callback m_get( list(), opt_msg(), z:context() ) -> return().

%% Routed from MQTT and API
-callback m_post( list( binary() ), opt_msg(), z:context() ) -> {ok, term()} | {error, term()}.

%% Routed from MQTT and API
-callback m_delete( list( binary() ), opt_msg(), z:context() ) -> {ok, term()} | {error, term()}.

-optional_callbacks([ m_post/3, m_delete/3 ]).
