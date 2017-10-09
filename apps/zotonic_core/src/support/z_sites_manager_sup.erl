%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012-2017 Maas-Maarten Zeeman
%% @doc Supervisor for sites dispatcher and manager

%% Copyright 2012-2017 Maas-Maarten Zeeman
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

-module(z_sites_manager_sup).
-author('Maas-Maarten Zeeman <mmzeeman@xs4all.nl>').

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc API for starting the sites dispatcher and manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Sites supervisor, supervises all sites
    SitesSup = {z_sites_sup,
                {z_sites_sup, start_link, []},
                permanent, 5000, supervisor, [z_sites_sup]},

    % Sites dispatcher, matches hosts and paths to sites and resources.
    Dispatcher = {z_sites_dispatcher,
                  {z_sites_dispatcher, start_link, []},
                  permanent, 5000, worker, dynamic},

    % Sites manager, starts all enabled sites
    SitesManager = {z_sites_manager,
                {z_sites_manager, start_link, []},
                permanent, 5000, worker, dynamic},

    % The dispatcher and sites manager have dependencies.
    % Both should be restarted when one of them fails.
    {ok, {{one_for_all, 10, 3600}, [ SitesSup, Dispatcher, SitesManager ]}}.

