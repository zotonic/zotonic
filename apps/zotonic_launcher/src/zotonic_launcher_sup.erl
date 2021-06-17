%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Read configurations and launch zotonic_core

%% Copyright 2017 Marc Worrell
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

-module(zotonic_launcher_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    lager:info("================"),
    lager:info("Zotonic starting"),
    lager:info("================"),
    lager:info("Init files used:"),
    [ lager:info("- ~s", [Cfg]) || Cfg <- zotonic_launcher_config:erlang_config_files( node() ) ],
    lager:info("Config files used:"),
    ZotonicCfgs = zotonic_launcher_config:zotonic_config_files( node() ),
    [ lager:info("- ~s", [Cfg]) || Cfg <- ZotonicCfgs ],
    lager:info("================"),
    case zotonic_launcher_config:read_configs( ZotonicCfgs ) of
        {ok, Config} ->
            zotonic_launcher_config:load_configs(Config),
            supervisor:start_link({local, ?SERVER}, ?MODULE, []);
        {error, _} = Error ->
            lager:error("Fatal error reading configuration files."),
            Error
    end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ZotonicConfig = application:get_all_env(zotonic),
    Children = [
        %% Launch the zotonic core supervisor (uses the read configs)
        {zotonic_core,
            {zotonic_core_sup, start_link, [ZotonicConfig]},
            permanent, 5000, supervisor, dynamic},

        %% HTTP listener
        {zotonic_listen_http,
            {zotonic_listen_http, start_link, []},
            permanent, 5000, worker, [zotonic_listen_http]},


        %% MQTT listener
        {zotonic_listen_mqtt,
            {zotonic_listen_mqtt, start_link, []},
            permanent, 5000, worker, [zotonic_listen_mqtt]}
    ],
    Children1  =case zotonic_listen_smtp:child_spec() of
        ignore ->
            Children;
        SMTPChildSpec ->
            Children ++ [ SMTPChildSpec ]
    end,
    {ok, { {one_for_one, 1, 5}, Children1 }}.

%%====================================================================
%% Internal functions
%%====================================================================
