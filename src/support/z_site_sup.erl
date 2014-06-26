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
    lager:md([
        {site, Host},
        {module, ?MODULE}
      ]),
    ok = z_stats:init_site(Host),
    {ok, SiteProps} = z_sites_manager:get_site_config(Host),

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
    InstallerModule = proplists:get_value(installer, SiteProps, z_installer),
    Installer = {z_installer,
                {InstallerModule, start_link, [SiteProps]},
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

    MediaCleanup = {z_media_cleanup_server,
                {z_media_cleanup_server, start_link, [SiteProps]}, 
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
            Dispatcher, Template, MediaClass, Pivot, DropBox, MediaCleanup,
            ModuleIndexer, Modules,
            PostStartup
        ],

    Processes1 = case z_db_pool:child_spec(Host, SiteProps) of
                     undefined -> Processes;
                     DbSpec when is_tuple(DbSpec) ->
                         [DbSpec | Processes ]
                 end,
    {ok, {{one_for_all, 2, 1}, Processes1}}.
