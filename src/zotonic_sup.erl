%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2016 Marc Worrell
%% @doc Supervisor for the zotonic application.

%% Copyright 2009-2016 Marc Worrell
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

-module(zotonic_sup).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(supervisor).

-compile([{parse_transform, lager_transform}]).

%% External exports
-export([start_link/0, upgrade/0, upgrade/2]).

%% supervisor callbacks
-export([init/1]).

-include_lib("zotonic.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    z_config:init_app_env(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade(Name, NewSpecs) -> ok
%% @doc Add processes if necessary.
upgrade(SupName, Specs) ->
    Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(SupName)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(SupName, Id),
                      supervisor:delete_child(SupName, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(SupName, Spec) || Spec <- Specs],
    ok.

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),
    upgrade(?MODULE, Specs).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    ensure_job_queues(),
    ensure_sidejobs(),

    %% Access Logger
    Logger = {z_access_syslog,
              {z_access_syslog, start_link, []},
              permanent, 5000, worker, [z_access_syslog, z_buffered_worker]},

    %% SMTP gen_servers: one for sending mails, the other for receiving email
    SmtpServer = {z_email_server,
                  {z_email_server, start_link, []},
                  permanent, 5000, worker, [z_email_server]},

    SmtpReceiveServer = {z_email_receive_server,
                        {z_email_receive_server, start_link, []},
                        permanent, 5000, worker, [z_email_receive_server]},

    FilesSup = {z_file_sup,
                 {z_file_sup, start_link, []},
                 permanent, 5000, supervisor, dynamic},

    %% Sites supervisor, starts all enabled sites
    SitesSup = {z_sites_sup,
                {z_sites_sup, start_link, []},
                permanent, 10100, supervisor, dynamic},

    %% File watcher, keep track of changed files for modules, templates etc.
    FSWatchSup = {z_filewatcher_sup,
                {z_filewatcher_sup, start_link, []},
                permanent, 10100, supervisor, dynamic},

    Processes = [
                 Logger,
                 SmtpServer, SmtpReceiveServer,
                 FilesSup,
                 SitesSup,
                 FSWatchSup
                 | get_extensions()
                ],

    z_stats:init(),

    lager:info(""),
    lager:info("Zotonic starting"),
    lager:info("================"),
    lager:info("Config files used:"),
    [lager:info("- ~s", [Cfg])
      || [Cfg] <- proplists:get_all_values(config, init:get_arguments()) ],
    lager:info(""),

    erlang:spawn(fun() ->
            timer:sleep(4000),
            lager:info(""),
            lists:map(
                fun
                  ([zotonic_status|_]) ->
                      ok;
                  ([Site, Status|_]) ->
                      Ctx = z_context:new(Site),
                      lager:info("~-40s ~p ~s~n",
                                 [z_context:abs_url(<<"/">>, Ctx), Site, Status])
                end,
                z_sites_manager:get_sites_status()),
            lager:info("")
        end),

    {ok, {{one_for_one, 1000, 10}, Processes}}.


%% @doc Ensure all job queues
ensure_job_queues() ->
    case jobs:queue_info(media_preview_jobs) of
        undefined ->
            jobs:add_queue(media_preview_jobs, [
                    {regulators, [
                          {counter, [
                                {limit, 3},
                                {modifiers, [{cpu, 1}]}
                          ]}
                    ]}
                ]);
        {queue, _Props} ->
            ok
    end.

%% @doc The supervisor for websocket requests and other transient processes.
ensure_sidejobs() ->
    sidejob:new_resource(zotonic_sessionjobs, sidejob_supervisor, z_config:get(sessionjobs_limit)),
    sidejob:new_resource(zotonic_sidejobs, sidejob_supervisor, z_config:get(sidejobs_limit)).

%% @doc Scan priv/extensions for ext_ folders and add those as childs to the supervisor.
get_extensions() ->
    Files = z_utils:wildcard(filename:join([z_utils:lib_dir(priv), "extensions", "ext_*"])),
    [
     begin
         Module = list_to_atom(filename:basename(F)),
         {Module,
          {Module, start_link, []},
          permanent, 5000, worker, dynamic}
     end
     || F <- Files].

