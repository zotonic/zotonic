%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Supervisor for all sites running inside Zotonic.  Starts the sites 
%% according to the config files in the sites subdirectories.

%% Copyright 2009 Marc Worrell
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
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0, update_dispatchinfo/0, get_sites/0, get_site_contexts/0, get_site_config/1]).

%% supervisor callbacks
-export([init/1]).

-include_lib("zotonic.hrl").
-include_lib("wm_host_dispatch_list.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @doc Update the webmachine dispatch information. Collects dispatch information from all sites and sends it
%% to webmachine for updating its dispatch lists and host information.
update_dispatchinfo() ->
    DispatchList = [ fetch_dispatchinfo(Site) || Site <- get_sites() ],
    application:set_env(webmachine, dispatch_list, DispatchList).

    fetch_dispatchinfo(Site) ->
        Name = z_utils:name_for_host(z_dispatcher, Site),
        {Host, Hostname, Hostalias, DispatchList} = z_dispatcher:dispatchinfo(Name),
        WMList = [list_to_tuple(tl(tuple_to_list(Disp))) || Disp <- DispatchList],
        #wm_host_dispatch_list{host=Host, hostname=Hostname, hostalias=Hostalias, redirect=true, dispatch_list=WMList}.


%% @doc Return a list of site names.
%% @spec get_sites -> [ atom() ]
get_sites() ->
	[Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]. 


%% @doc Return a list of contexts initialized for all active sites.
%% @spec get_site_contexts -> [ Context ]
get_site_contexts() ->
	[ z_context:new(Name) || Name <- get_sites() ].


%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id),
		      ok
	      end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.


%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Sites = scan_sites(),
    {ok, {{one_for_one, 1000, 10}, sites_to_spec(Sites, [])}}.

    %% @doc Define a site supervisor for all enabled sites.
    sites_to_spec([], Acc) ->
        Acc;
    sites_to_spec([SiteProps|Rest], Acc) ->
        Enabled = proplists:get_value(enabled, SiteProps, false),
        case Enabled of
            true ->
                {host, Name} = proplists:lookup(host, SiteProps),
                Spec = {
                    Name,
                    {z_site_sup, start_link, [SiteProps]},
                    permanent, 5000, worker, dynamic
                },
                sites_to_spec(Rest, [Spec|Acc]);
            false ->
                sites_to_spec(Rest, Acc)
        end.


%% @doc Scan all sites subdirectories for the site configurations.
%% @spec scan_sites -> [ SiteProps ]
scan_sites() ->
    SitesDir = filename:join([code:lib_dir(zotonic, priv), "sites", "*", "config"]),
    Configs = [ parse_config(C) || C <- filelib:wildcard(SitesDir) ],
    [ C || C <- Configs, is_list(C) ].


    parse_config(C) ->
        case file:consult(C) of
            {ok, [H|_]} -> 
                H;
            {error, Reason} = Error ->
                ?ERROR("Could not consult site config ~p: error ~p", [C, Reason]),
                Error
        end.

%% @doc Fetch the configuration of a specific site.
%% @spec site_config(Site::atom()) -> SiteProps::list() | {error, Reason}
get_site_config(Site) ->
    ConfigFile = filename:join([code:lib_dir(zotonic, priv), "sites", Site, "config"]),
    parse_config(ConfigFile).

