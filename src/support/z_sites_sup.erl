%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012 Maas-Maarten Zeeman
%% @doc Supervisor for sites dispatcher and manager 

%% Copyright 2012 Maas-Maarten Zeeman
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

-module(z_sites_sup).
-author('Maas-Maarten Zeeman <mmzeeman@xs4all.nl>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-include_lib("zotonic.hrl").

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),
    zotonic_sup:upgrade(?MODULE, Specs),
    z_sites_sup:upgrade().

%% @spec start_link() -> ServerRet
%% @doc API for starting the sites dispatcher and manager
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    z_stats:new(#counter{name=requests}, #stats_from{system=webzmachine}),
    z_stats:new(#counter{name=requests}, #stats_from{system=db}),
    z_stats:new(#counter{name=out}, #stats_from{system=webzmachine}),
    z_stats:new(#histogram{name=duration}, #stats_from{system=webzmachine}),
    z_stats:new(#histogram{name=duration}, #stats_from{system=db}),


    % Sites supervisor, starts all enabled sites
    SitesManager = {z_sites_manager,
                {z_sites_manager, start_link, []},
                permanent, 5000, worker, dynamic},

    % Sites disapatcher, matches hosts and paths to sites and resources.            
    Dispatcher = {z_sites_dispatcher,
                  {z_sites_dispatcher, start_link, []},
                  permanent, 5000, worker, dynamic},

    % The dispatcher and sites manager have dependencies. 
    % Both should be restarted when one of them fails.
    {ok, {{one_for_all, 10, 3600}, [SitesManager, Dispatcher]}}.

