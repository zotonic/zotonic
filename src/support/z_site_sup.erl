%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Supervisor for a zotonic server. Starts webmachine, all base zotonic processes and the site supervisor.

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

-module(z_site_sup).
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
init(Host) ->
    % On (re)start we use the newest site config.
    SiteProps = z_sites_manager:get_site_config(Host),

    Notifier = {z_notifier,
                {z_notifier, start_link, [SiteProps]}, 
                permanent, 5000, worker, dynamic},

    Depcache = {z_depcache,
                {z_depcache, start_link, [SiteProps]}, 
                permanent, 5000, worker, dynamic},

    Translation = {z_trans_server, 
                {z_trans_server, start_link, [SiteProps]},
                permanent, 5000, worker, dynamic},

    % The installer needs the database pool, depcache and translation.
    Installer = {z_installer,
                {z_installer, start_link, [SiteProps]},
                permanent, 1, worker, dynamic},

    % Continue with the normal per-site servers
    Session = {z_session_manager,
                {z_session_manager, start_link, [SiteProps]}, 
                permanent, 5000, worker, dynamic},

    Dispatcher = {z_dispatcher,
                {z_dispatcher, start_link, [SiteProps]}, 
                permanent, 5000, worker, dynamic},

    Template = {z_template,
                {z_template, start_link, [SiteProps]}, 
                permanent, 5000, worker, dynamic},

    MediaClass = {z_mediaclass, 
                {z_mediaclass, start_link, [SiteProps]},
                permanent, 5000, worker, dynamic},

    DropBox = {z_dropbox,
                {z_dropbox, start_link, [SiteProps]}, 
                permanent, 5000, worker, dynamic},

    Pivot = {z_pivot_rsc,
                {z_pivot_rsc, start_link, [SiteProps]}, 
                permanent, 5000, worker, dynamic},

    ModuleIndexer = {z_module_indexer,
                {z_module_indexer, start_link, [SiteProps]},
                permanent, 5000, worker, dynamic},

    Modules = {z_module_manager,
                {z_module_manager, start_link, [SiteProps]},
                permanent, 5000, worker, dynamic},

    PostStartup = {z_site_startup,
                    {z_site_startup, start_link, [SiteProps]},
                    permanent, 5000, worker, dynamic},

    Processes = [
            Notifier, Depcache, Translation, Installer, Session, 
            Dispatcher, Template, MediaClass, DropBox, Pivot,
            ModuleIndexer, Modules,
            PostStartup
    ],
    {ok, {{one_for_all, 2, 1}, add_db_pool(Host, Processes, SiteProps)}}.


%% @doc Optionally add the db pool connection
add_db_pool(Host, Processes, SiteProps) ->
    DefaultDatabase = case Host of 
                          default -> "zotonic";
                          _ -> atom_to_list(Host) 
                      end,
    case proplists:get_value(dbdatabase, SiteProps, DefaultDatabase) of
        none -> 
            %% No database connection needed
            Processes;
        DbDatabase ->
            % Add a db pool to the site's processes
            DbHost     = proplists:get_value(dbhost,     SiteProps, z_config:get(dbhost)),
            DbPort     = proplists:get_value(dbport,     SiteProps, z_config:get(dbport)),
            DbUser     = proplists:get_value(dbuser,     SiteProps, z_config:get(dbuser)),
            DbPassword = proplists:get_value(dbpassword, SiteProps, z_config:get(dbpassword)),
            DbSchema   = proplists:get_value(dbschema,   SiteProps, z_config:get(dbschema)),
            DbOpts     = [ {host, DbHost}, {port, DbPort}, 
                           {username, DbUser}, {password, DbPassword}, 
                           {database, DbDatabase}, {schema, DbSchema} ],

            [ {Host, 
                {pgsql_pool, start_link, [Host, 10, DbOpts]},
                permanent, 5000, worker, dynamic} ] ++ Processes
    end.
