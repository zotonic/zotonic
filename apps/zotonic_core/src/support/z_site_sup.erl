%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2020 Marc Worrell
%% @doc Supervisor for a zotonic site.

%% Copyright 2009-2020 Marc Worrell
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

-module(z_site_sup).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%% Called when the installer is done for a certain Site.
-export([
    install_phase1/1,
    install_done/1
]).

%% @spec start_link(Site) -> ServerRet
%% @doc API for starting the site supervisor.
-spec start_link(Site :: atom()) -> {ok, pid()} | {error, term()}.
start_link(Site) ->
    Name = z_utils:name_for_site(?MODULE, Site),
    case supervisor:start_link({local, Name}, ?MODULE, Site) of
        {ok, Pid} ->
            {ok, Pid};
        {error, _} = Error ->
            Error
    end.

%% @doc Supervisor callback, returns the supervisor tree for a zotonic site
-spec init(atom()) -> {ok, {supervisor:sup_flags(), [ supervisor:child_spec() ]}}.
init(Site) ->
    % TODO: replace this with something more robust, problem
    % is that phase1 calls z_sites_manager for the site config, and
    % that this module is called by z_sites_manager. So blocking the
    % call. A solution is to decouple the config scanning from
    % the sites supervisor.
    logger:set_process_metadata(#{
        site => Site,
        module => ?MODULE
    }),
    z_proc:spawn_link_md(fun() -> install_phase1(Site) end),
    z_sites_manager:set_site_status(Site, starting),
    {ok, {{one_for_all, 2, 1}, [
        {z_trans_server,
            {z_trans_server, start_link, [Site]},
            permanent, 5000, worker, dynamic},
        {z_module_sup,
            {z_module_sup, start_link, [Site]},
            permanent, 5000, supervisor, dynamic}
    ]}}.

install_phase1(Site) ->
    ok = m_site:load_config(Site),
    ok = z_stats:init_site(Site),
    SiteProps = m_site:all(Site),
    SiteSupName = z_utils:name_for_site(?MODULE, Site),
    case z_db_pool:child_spec(Site, SiteProps) of
        undefined ->
            start_installer_processes(SiteSupName, SiteProps);
        DbSpec when is_tuple(DbSpec); is_map(DbSpec) ->
            supervisor:start_child(SiteSupName, DbSpec),
            case wait_for_db(Site) of
                ok -> start_installer_processes(SiteSupName, SiteProps);
                {error, _} = Error -> Error
            end
    end.

start_installer_processes(SiteSupName, SiteProps) ->
    % The installer needs the database pool, depcache and translation.
    InstallerModule = proplists:get_value(installer, SiteProps, z_install_update),
    Processes = [
        {z_depcache,
            {z_depcache, start_link, [SiteProps]},
            permanent, 5000, worker, dynamic},
        {z_install_update,
            {InstallerModule, start_link, [SiteProps]},
            permanent, 1, worker, dynamic}
    ],
    lists:foreach(
            fun(Child) ->
                case supervisor:start_child(SiteSupName, Child) of
                    {ok, _} -> ok;
                    {error, {already_started, _}} -> ok
                end
            end,
            Processes).

%% @doc Wait till the database driver started. Wait max 10 seconds.
wait_for_db(Site) ->
    wait_for_db(Site, 1000).

wait_for_db(Site, 0) ->
    ?LOG_ERROR(#{
        text => <<"Timeout waiting for database driver">>,
        in => zotonic_core,
        site => Site
    }),
    {error, timeout};
wait_for_db(Site, N) ->
    case z_db:has_connection(Site) of
        true ->
            ok;
        false ->
            timer:sleep(10),
            wait_for_db(Site, N-1)
    end.

%% @doc Called by z_installer after the database installation is done.
%%      We can now start all other site processes.
-spec install_done(atom()) -> ok.
install_done(Site) when is_atom(Site) ->
    Dispatcher = {z_dispatcher,
                {z_dispatcher, start_link, [Site]},
                permanent, 5000, worker, dynamic},

    SiteServices = {z_site_services_sup,
                {z_site_services_sup, start_link, [Site]},
                permanent, 5000, supervisor, dynamic},

    ModuleIndexer = {z_module_indexer,
                {z_module_indexer, start_link, [Site]},
                permanent, 5000, worker, dynamic},

    ModuleManager = {z_module_manager,
                {z_module_manager, start_link, [Site]},
                permanent, 5000, worker, dynamic},

    PostStartup = {z_site_startup,
                    {z_site_startup, start_link, [Site]},
                    permanent, 5000, worker, dynamic},

    Processes = [
            Dispatcher,
            SiteServices,
            ModuleIndexer,
            ModuleManager,
            PostStartup
        ],

    Name = z_utils:name_for_site(?MODULE, Site),
    lists:foreach(
            fun(Child) ->
                case supervisor:start_child(Name, Child) of
                    {ok, _} -> ok;
                    {error, {already_started, _}} -> ok
                end
            end,
            Processes).
