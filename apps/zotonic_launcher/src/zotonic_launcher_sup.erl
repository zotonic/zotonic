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
    lager:info("Config files used:"),
    [lager:info("- ~s", [Cfg]) || Cfg <- zotonic_launcher_config:config_files()],
    lager:info("================"),
    zotonic_launcher_config:load_configs(),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ZotonicConfig = application:get_all_env(zotonic),
    {ok, { {one_for_one, 1, 5}, [

        %% Launch the zotonic core supervisor (uses the the read configs)
        {zotonic_core,
            {zotonic_core_sup, start_link, [ZotonicConfig]},
            permanent, 5000, supervisor, dynamic},

        %% File watcher, keep track of changed files for modules, templates etc.
        {z_filewatcher_sup,
            {z_filewatcher_sup, start_link, []},
            permanent, 10100, supervisor, dynamic},

        %% HTTP listener
        {zotonic_listen_http,
            {zotonic_listen_http, start_link, []},
            permanent, 5000, worker, [zotonic_listen_http]},

        %% SMTP listener
        {zotonic_listen_smtp,
            {zotonic_listen_smtp, start_link, []},
            permanent, 5000, worker, [zotonic_listen_smtp]}

    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
