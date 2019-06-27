%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc MQTT ssl protocol handler for ranch

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

-module(zotonic_listen_mqtt_handler_tls).

-behaviour(ranch_protocol).

-export([start_link/4]).
-export([connection_process/5]).

-spec start_link( ranch:ref(), ssl:sslsocket(), module(), map() ) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
    Pid = proc_lib:spawn_link(
                ?MODULE,
                connection_process,
                [ self(), Ref, Socket, Transport, Opts ]),
    {ok, Pid}.

-spec connection_process( pid(), ranch:ref(), ssl:sslsocket(), module(), map() ) -> ok.
connection_process(_Parent, Ref, Socket, Transport, Opts) ->
    % TODO: Fetch the SNI hostname
    zotonic_listen_mqtt_handler:init(Ref, Socket, Transport, Opts#{ is_ssl => true } ).
