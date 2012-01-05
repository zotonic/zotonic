%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Supervisor zynamo services.

%% Copyright 2012 Marc Worrell
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

-module(z_site_services_sup).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-include_lib("zotonic.hrl").

%% @spec start_link(Host) -> ServerRet
%% @doc API for starting the site supervisor.
start_link(Host) ->
    supervisor:start_link(?MODULE, Host).


%% @spec init(Host) -> SupervisorTree
%% @doc Supervisor callback, returns the supervisor tree for a zotonic site
init(Args) ->
    {ok, {{one_for_one, 100, 5}, [
        {zotonic_service_siteconfig, 
                {zotonic_service_siteconfig, start_link, [Args]},
                permanent, 5000, worker, dynamic}
    ]}}.

