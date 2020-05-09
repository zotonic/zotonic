%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2012 Marc Worrell
%% @doc Supervisor for the zotonic application.

%% Copyright 2009-2012 Marc Worrell
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

%% SNI function
-export([sni_fun/1]).

%% supervisor callbacks
-export([init/1]).

-include_lib("zotonic.hrl").
-include_lib("zotonic_release.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
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

    %% Random id generation
    Ids = {z_ids,
           {z_ids, start_link, []},
           permanent, 5000, worker, [z_ids]},

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

    % Modification time cache
    MTime = {z_file_mtime,
            {z_file_mtime, start_link, []},
            permanent, 10100, worker, dynamic},

    Processes = [
                 Ids,
                 SmtpServer, SmtpReceiveServer,
                 FilesSup,
                 SitesSup,
                 FSWatchSup,
                 MTime | get_extensions()
                ],

    %% Listen to IP address and Port
    WebIp = z_config:get(listen_ip),
    WebPort = z_config:get(listen_port),
    SSLPort = z_config:get(ssl_listen_port),

    WebConfig = [
                  {dispatcher, z_sites_dispatcher},
                  {dispatch_list, []},
                  {backlog, z_config:get(inet_backlog)},
                  {acceptor_pool_size, z_config:get(inet_acceptor_pool_size)}
                ],

    SSLWebConfig = [
                  {dispatcher, z_sites_dispatcher},
                  {dispatch_list, []},
                  {backlog, z_config:get(ssl_backlog)},
                  {acceptor_pool_size, z_config:get(ssl_acceptor_pool_size)},
                  {ssl, true},
                  {ssl_opts, [{sni_fun, fun ?MODULE:sni_fun/1}]}
              ],

    %% Listen to the ip address and port for all sites.
    IPv4Opts = [{port, WebPort}, {ip, WebIp}],
    IPv4SSLOpts = [{port, SSLPort}, {ip, WebIp}],
    IPv6Opts = [{port, WebPort}, {ip, any6}],
    IPv6SSLOpts = [{port, SSLPort}, {ip, any6}],

    %% Webmachine/Mochiweb processes
    [IPv4Proc, IPv4SSLProc, IPv6Proc, IPv6SSLProc] =
        [[{Name,
           {webmachine_mochiweb, start,
            [Name, Opts]},
           permanent, 5000, worker, dynamic}]
         || {Name, Opts}
                <- [{webmachine_mochiweb, IPv4Opts ++ WebConfig},
                    {webmachine_mochiweb_ssl, IPv4SSLOpts ++ SSLWebConfig},
                    {webmachine_mochiweb_v6, IPv6Opts ++ WebConfig},
                    {webmachine_mochiweb_ssl_v6, IPv6SSLOpts ++ SSLWebConfig}]],

    %% When binding to all IP addresses ('any'), bind separately for ipv6 addresses
    EnableIPv6 = case WebIp of
                     any -> ipv6_supported();
                     _ -> false
                 end,
    EnableSSL = start_ssl_listeners(),

    Processes1 = case {EnableIPv6, EnableSSL} of
        {false, false} -> Processes ++ IPv4Proc;
        {false, true} -> Processes ++ IPv4Proc ++ IPv4SSLProc;
        {true, false} -> Processes ++ IPv4Proc ++ IPv6Proc;
        {true, true} -> Processes ++ IPv4Proc ++ IPv4SSLProc ++ IPv6Proc ++ IPv6SSLProc
    end,

    init_stats(),
    init_ua_classifier(),
    init_webmachine(),

    spawn(fun() ->
                  timer:sleep(4000),
                  lager:info(""),
                  lager:info("Zotonic started"),
                  lager:info("==============="),
                  lager:info("Config files used:"),
                  [lager:info("- ~s", [Cfg]) || [Cfg] <- proplists:get_all_values(config, init:get_arguments())],
                  lager:info(""),

                  log_start_warnings(EnableSSL),

                  case {EnableIPv6, EnableSSL} of
                      {false, false} ->
                          lager:info("Web server listening on IPv4 ~p:~p", [WebIp, WebPort]);
                      {false, true} ->
                          lager:info("Web server listening on IPv4 ~p:~p, SSL :~p", [WebIp, WebPort, SSLPort]);
                      {true, false} ->
                          lager:info("Web server listening on IPv4 ~p:~p, IPv6 ::~p", [WebIp, WebPort, WebPort]);
                      {true, true} ->
                          lager:info("Web server listening on IPv4 ~p:~p, IPv6 ::~p, SSL :~p", [WebIp, WebPort, WebPort, SSLPort])
                  end,

                  lager:info(""),
                  [lager:info("http://~-40s- ~s~n", [z_context:hostname_port(z:c(Site)), Status]) ||
                      [Site,Status|_] <- z_sites_manager:get_sites_status(), Site =/= zotonic_status]
          end),

    {ok, {{one_for_one, 1000, 10}, Processes1}}.

%% @doc Display startup warnings (if any).
%%
log_start_warnings(false) ->
    lager:warning(""),
    lager:warning("SSL support disabled. Please upgrade to Erlang 18.1 or higher"),
    lager:warning("");
log_start_warnings(_) ->
    case application:get_env(ssl, session_lifetime) of
        {ok, _} -> ok;
        undefined ->
            lager:warning(""),
            lager:warning("SSL application using Erlang defaults, it is recommended to change this configuration in your erlang.config"),
            lager:warning("")
    end.


%% @doc Initializes the stats collector.
%%
init_stats() ->
    z_stats:init().

%% @doc Initializes the ua classifier. When it is enabled it is loaded and
%% tested if it works.
init_ua_classifier() ->
    case z_config:get(use_ua_classifier) of
        true ->
            case ua_classifier:classify("") of
                {error, ua_classifier_nif_not_loaded} = Error ->
                    lager:error("ua_classifier: could not load the NIF. Check deps/ua_classifier or set Zotonic config {use_ua_classifier, false}."),
                    Error;
                {ok, _} ->
                    ok
            end;
        false ->
            ok
    end.

%% @doc Sets the application parameters for webmachine and starts the logger processes.
%%      NOTE: This part has been removed from webmachine_mochiweb:start/2 to avoid
%%      messing with application parameters when starting up a new wm-mochiweb process.
init_webmachine() ->
    application:set_env(webzmachine, server_header, "Zotonic"),
    set_webzmachine_default(webmachine_logger_module, z_stats),
    set_webzmachine_default(error_handler, controller),
    webmachine_sup:start_logger().

set_webzmachine_default(Par, Def) ->
    case application:get_env(webzmachine, Par) of
        undefined -> application:set_env(webzmachine, Par, Def);
        _ -> nop
    end.

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

%% @todo Exclude platforms that do not support raw ipv6 socket options
ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} -> true;
        {error, _} -> false
    end.

%% @doc Scan priv/extensions for ext_ folders and add those as childs to the supervisor.
get_extensions() ->
    Files = filelib:wildcard(filename:join([z_utils:lib_dir(priv), "extensions", "ext_*"])),
    [
     begin
         Module = list_to_atom(filename:basename(F)),
         {Module,
          {Module, start_link, []},
          permanent, 5000, worker, dynamic}
     end
     || F <- Files].

%% @doc Let sites return their own keys and certificates.
sni_fun(ServerName) ->
    case z_sites_dispatcher:get_host_for_domain(ServerName) of
        undefined -> undefined;
        {ok, Host} ->
            z_notifier:first(#ssl_options{server_name=ServerName}, z_context:new(Host))
    end.

-ifdef(no_ssl).
start_ssl_listeners() -> false.
-else.
start_ssl_listeners() -> true.
-endif.
