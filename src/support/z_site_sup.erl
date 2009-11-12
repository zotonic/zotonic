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

%% @spec start_link(SiteProps) -> ServerRet
%% @doc API for starting the site supervisor.
start_link(SiteProps) ->
    supervisor:start_link(?MODULE, SiteProps).


%% @spec init(SiteProps) -> SupervisorTree
%% @doc Supervisor callback, returns the supervisor tree for a zotonic site
init(SiteProps) ->
    % Default site name
    {host, Host} = proplists:lookup(host, SiteProps),

    % Make sure that the database is added to the db pool
    case whereis(Host) of
        undefined ->
            DefaultDatabase = case Host of default -> "zotonic"; _ -> atom_to_list(Host) end,
            
            DbHost     = proplists:get_value(dbhost,     SiteProps, "localhost"),
            DbPort     = proplists:get_value(dbport,     SiteProps, 5432),
            DbUser     = proplists:get_value(dbuser,     SiteProps, "zotonic"),
            DbPassword = proplists:get_value(dbpassword, SiteProps, ""),
            DbDatabase = proplists:get_value(dbdatabase, SiteProps, DefaultDatabase),
            DbOpts     = [{host, DbHost}, {port, DbPort}, {username, DbUser}, {password, DbPassword}, {database, DbDatabase}],

            epgsql_pool:start_pool(Host, 10, DbOpts);
        Pid when is_pid(Pid) -> 
            ok
    end,

    Depcache = {z_depcache,
                {z_depcache, start_link, [SiteProps]}, 
                permanent, 5000, worker, dynamic},

    Notifier = {z_notifier,
                {z_notifier, start_link, [SiteProps]}, 
                permanent, 5000, worker, dynamic},

    Installer = {z_installer,
                {z_installer, start_link, [SiteProps]},
                permanent, 1, worker, dynamic},

    Session = {z_session_manager,
                {z_session_manager, start_link, [SiteProps]}, 
                permanent, 5000, worker, dynamic},

    Visitor = {z_visitor_manager,
                {z_visitor_manager, start_link, [SiteProps]}, 
                permanent, 5000, worker, dynamic},

    Dispatcher = {z_dispatcher,
                {z_dispatcher, start_link, [SiteProps]}, 
                permanent, 5000, worker, dynamic},

    Template = {z_template,
                {z_template, start_link, [SiteProps]}, 
                permanent, 5000, worker, dynamic},

    Scomp = {z_scomp,
                {z_scomp, start_link, [SiteProps]}, 
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

    Modules = {z_module_sup,
                {z_module_sup, start_link, [SiteProps]},
                permanent, 5000, worker, dynamic},

    Processes = [
            Depcache, Notifier, Installer, Session, Visitor, 
            Dispatcher, Template, Scomp, DropBox, Pivot,
            ModuleIndexer, Modules
    ],
    {ok, {{one_for_one, 1000, 10}, Processes}}.

