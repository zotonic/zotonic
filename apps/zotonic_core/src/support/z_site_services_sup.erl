%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc Supervisor for services for a site. These can be restarted without affecting other parts.

%% Copyright 2020 Marc Worrell
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

%% @doc API for starting the site services supervisor.
start_link(SiteProps) ->
    {site, Site} = proplists:lookup(site, SiteProps),
    Name = z_utils:name_for_site(?MODULE, Site),
    supervisor:start_link({local, Name}, ?MODULE, SiteProps).


%% @doc Supervisor callback, returns the supervisor tree for the zotonic site services
-spec init( proplists:proplist() ) -> {ok, {supervisor:sup_flags(), [ supervisor:child_spec() ]}}.
init(SiteProps) ->
    {site, Site} = proplists:lookup(site, SiteProps),
    lager:md([
        {site, Site},
        {module, ?MODULE}
      ]),

    KeyServerName = z_utils:name_for_site(keyserver, Site),
    KeyServer = {keyserver_sup,
                 {keyserver_sup, start_link, [KeyServerName]},
                 permanent, 5000, supervisor, dynamic},

    MqttPool = {mqtt_sessions_pool_sup,
                {mqtt_sessions_pool_sup, start_link, [Site]},
                permanent, 5000, supervisor, dynamic},

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

    EdgeLog = {z_edge_log_server,
                {z_edge_log_server, start_link, [SiteProps]},
                permanent, 5000, worker, dynamic},

    Processes = [
            KeyServer, MqttPool,
            Template, MediaClass, Pivot, DropBox,
            MediaCleanup, EdgeLog
        ],

    {ok, {{one_for_one, 500, 10}, Processes}}.
