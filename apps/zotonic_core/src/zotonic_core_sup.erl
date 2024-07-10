%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Supervisor for the zotonic application, started by zotonic_launcher.
%% @end

%% Copyright 2009-2024 Marc Worrell
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

-include_lib("kernel/include/logger.hrl").

%% External exports
-export([ start_link/1 ]).

%% supervisor callbacks
-export([ init/1 ]).

%% @doc Start the main zotonic supervisor and some helpers. The zotonic_core:setup/1 function
%% must be called before starting this supervisor. The setup is normally called by zotonic_launcher.
-spec start_link(list()) -> {ok, pid()}.
start_link(Options) ->
    lists:foreach(
        fun({K,V}) -> application:set_env(zotonic_core, K, V) end,
        Options),
    z_config:init_app_env(),
    zotonic_filewatcher_sup:start_watchers(),
    z_jsxrecord:init(),
    z_stats:init_system(),
    z_tempfile_cleanup:start(),
    ensure_job_queues(),
    ensure_sidejobs(),
    z_ssl_dhfile:ensure_dhfile(),
    mqtt_sessions_runtime(),
    inets:start(httpc, [{profile, zotonic}]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc supervisor callback.
init([]) ->
    log_start_warnings(),
    z_filehandler:start_observers(),
    LogBufferSize = z_config:get(log_http_buffer_size),
    Processes = [
        % Ring buffer for http request logs
        {ringbuffer_normal,
            {ringbuffer_process, start_link, [ zotonic_http_metrics_normal, LogBufferSize ]},
            permanent, 5000, worker, [ ringbuffer_process ]},

        {ringbuffer_prio,
            {ringbuffer_process, start_link, [ zotonic_http_metrics_prio, LogBufferSize ]},
            permanent, 5000, worker, [ ringbuffer_process ]},

        % Http request metrics handling. Accepts priority list of buffers to consume.
        {z_stats,
            {z_stats, start_link, [ [ zotonic_http_metrics_prio, zotonic_http_metrics_normal ] ]},
            permanent, 5000, worker, [z_stats]},

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

log_start_warnings() ->
    case application:get_env(ssl, session_lifetime) of
        {ok, _} ->
            ok;
        undefined ->
            ?LOG_WARNING(
                "SSL application using Erlang defaults, it is recommended"
                "to change this configuration in your erlang.config")
    end.

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
    ensure_job_queue(
        zotonic_singular_job,
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
    z_sidejob:init().


mqtt_sessions_runtime() ->
    case mqtt_sessions:runtime() of
        mqtt_sessions_runtime ->
            mqtt_sessions:set_runtime(z_mqtt_sessions_runtime);
        _ ->
            ok
    end.
