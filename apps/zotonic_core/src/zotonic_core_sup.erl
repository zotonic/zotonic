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
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Start the main zotonic supervisor and some helpers
-spec start_link(list()) -> {ok, pid()}.
start_link(Options) ->
    lists:foreach(
        fun({K,V}) -> application:set_env(zotonic_core, K, V) end,
        Options),
    z_config:init_app_env(),
    z_stats:init(),
    z_tempfile_cleanup:start(),
    ensure_job_queues(),
    ensure_mnesia_schema(),
    ensure_sidejobs(),
    inets:start(httpc, [{profile, zotonic}]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    erlang:spawn(fun() ->
            timer:sleep(4000),
            lager:info("================"),
            lager:info("Sites Status"),
            lager:info("================"),
            lists:map(
                fun
                  ([Site, running|_]) when Site =/= zotonic_site_status ->
                      Ctx = z_context:new(Site),
                      lager:info("~p ~s ~-40s~n",
                                 [Site, running, z_context:abs_url(<<"/">>, Ctx)]);
                  ([Site, Status|_]) ->
                      lager:info("~p - ~s~n", [Site, Status])
                end,
                z_sites_manager:get_sites_status()),
            lager:info("================")
        end),

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
        {z_sites_sup,
            {z_sites_sup, start_link, []},
            permanent, 10100, supervisor, dynamic},

        % File watcher, keep track of changed files for modules, templates etc.
        {z_file_mtime,
            {z_file_mtime, start_link, []},
            permanent, 10100, worker, dynamic}
    ],
    {ok, {{one_for_one, 1000, 10}, Processes}}.


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


%% @doc Ensure that mnesia has created its schema in the configured priv/data/mnesia directory.
-spec ensure_mnesia_schema() -> ok.
ensure_mnesia_schema() ->
    case mnesia_dir() of
        {ok, Dir} ->
            case filelib:is_dir(Dir) andalso filelib:is_regular(filename:join(Dir, "schema.DAT")) of
                true -> ok;
                false -> ok = mnesia:create_schema([node()])
            end;
        undefined ->
            lager:info("No mnesia directory defined, running without persistent email queue and filezcache.~n"
                       "To enable persistency, add to erlang.config: {mnesia,[{dir,\"priv/mnesia\"}]}~n~n"),
            ok
    end.

mnesia_dir() ->
    application:load(mnesia),
    case application:get_env(mnesia, dir) of
        {ok, none} -> undefined;
        {ok, ""} -> undefined;
        {ok, Dir} -> {ok, Dir};
        undefined ->
            PrivDir = case code:priv_dir(zotonic) of
                {error, bad_name} -> code:priv_dir(zotonic_core);
                ZotonicPrivDir when is_list(ZotonicPrivDir) -> ZotonicPrivDir
            end,
            MnesiaDir = filename:join([ PrivDir, "mnesia", atom_to_list(node()) ]),
            case z_filelib:ensure_dir(MnesiaDir) of
                ok -> {ok, MnesiaDir};
                {error, _} = Error ->
                    lager:error("Could not create mnesia dir \"~s\": ~p",
                                [MnesiaDir, Error]),
                    undefined
            end
    end.

