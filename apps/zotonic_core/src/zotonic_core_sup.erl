%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%% @doc Supervisor for the zotonic application, started by zotonic_launcher.

%% Copyright 2009-2017 Marc Worrell
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

-module(zotonic_core_sup).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(supervisor).

-compile([{parse_transform, lager_transform}]).

%% External exports
-export([ start_link/1 ]).

%% supervisor callbacks
-export([ init/1 ]).

%% @doc Start the main zotonic supervisor and some helpers
-spec start_link(list()) -> {ok, pid()}.
start_link(Options) ->
    lists:foreach(
        fun({K,V}) -> application:set_env(zotonic_core, K, V) end,
        Options),
    z_config:init_app_env(),
    zotonic_core:setup(),
    z_stats:init(),
    z_tempfile_cleanup:start(),
    ensure_job_queues(),
    ensure_sidejobs(),
    mqtt_sessions_runtime(),
    inets:start(httpc, [{profile, zotonic}]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc supervisor callback.
init([]) ->
    spawn_delayed_status(),
    z_filehandler:start_observers(),
    Processes = [
        % Access Logger
        {z_access_syslog,
            {z_access_syslog, start_link, []},
            permanent, 5000, worker, [z_access_syslog, z_buffered_worker]},

        % SMTP gen_server for sending emails
        {z_email_server,
            {z_email_server, start_link, []},
            permanent, 5000, worker, [z_email_server]},

        {z_file_sup,
            {z_file_sup, start_link, []},
            permanent, 5000, supervisor, dynamic},

        % Sites supervisor, starts all enabled sites
        {z_sites_manager_sup,
            {z_sites_manager_sup, start_link, []},
            permanent, 10100, supervisor, dynamic},

        % File watcher, keep track of changed files for modules, templates etc.
        {z_file_mtime,
            {z_file_mtime, start_link, []},
            permanent, 10100, worker, dynamic}
    ],
    {ok, {{one_for_one, 1000, 10}, Processes}}.

spawn_delayed_status() ->
    erlang:spawn(fun() ->
        timer:sleep(4000),
        lager:info("================"),
        lager:info("Sites Status"),
        lager:info("================"),
        SitesStatus = maps:to_list(z_sites_manager:get_sites()),
        {Running, Other} = lists:partition(
            fun({_Site, Status}) -> Status =:= running end,
            SitesStatus),
        lists:map(
            fun
              ({Site, running}) when Site =/= zotonic_site_status ->
                  Ctx = z_context:new(Site),
                  lager:info("~p ~s ~-40s~n",
                             [Site, running, z_context:abs_url(<<"/">>, Ctx)]);
              ({Site, Status}) ->
                  lager:info("~p - ~s~n", [Site, Status])
            end,
            Running ++ Other),
        lager:info("================")
    end).

%% @doc Ensure all job queues
ensure_job_queues() ->
    ensure_job_queue(
        media_preview_jobs,
        [
            {regulators, [
                {counter, [
                    {limit, 3},
                    {modifiers, [{cpu, 1}]}
                ]}
            ]}
        ]),
    ensure_job_queue(
        manage_module_jobs,
        [
            {regulators, [
                {counter, [
                    {limit, 1}
                ]}
            ]}
        ]),
    ok.

ensure_job_queue(Name, Options) ->
    case jobs:queue_info(Name) of
        undefined -> jobs:add_queue(Name, Options);
        {queue, _Props} -> ok
    end.

%% @doc The supervisor for websocket requests and other transient processes.
ensure_sidejobs() ->
    sidejob:new_resource(zotonic_sessionjobs, sidejob_supervisor, z_config:get(sessionjobs_limit)),
    sidejob:new_resource(zotonic_sidejobs, sidejob_supervisor, z_config:get(sidejobs_limit)).


mqtt_sessions_runtime() ->
    case mqtt_sessions:runtime() of
        mqtt_sessions_runtime ->
            mqtt_sessions:set_runtime(z_mqtt_sessions_runtime);
        _ ->
            ok
    end.
