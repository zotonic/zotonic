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

%% SNI function
-export([sni_fun/1]).

%% supervisor callbacks
-export([init/1]).

-include_lib("zotonic.hrl").

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

    init_stats(),
    start_http_listeners(),

    lager:info(""),
    lager:info("Zotonic starting"),
    lager:info("================"),
    lager:info("Config files used:"),
    [ lager:info("- ~s", [Cfg]) 
      || [Cfg] <- proplists:get_all_values(config, init:get_arguments()) ],
    % lager:info(""),
    % [ lager:info("http://~-40s- ~s~n", [z_context:hostname_port(z:c(Site)), Status])
    %   || [Site,Status|_] <- z_sites_manager:get_sites_status(), Site =/= zotonic_status],

    {ok, {{one_for_one, 1000, 10}, Processes}}.

%% @doc Initializes the stats collector.
init_stats() ->
    z_stats:init().

%% @doc Start the HTTP listeners
start_http_listeners() ->
    application:set_env(cowmachine, server_header, <<"Zotonic/", (z_convert:to_binary(?ZOTONIC_VERSION))/binary>>),
    WebIp = z_config:get(listen_ip),
    WebPort = z_config:get(listen_port),
    SSLPort = z_config:get(ssl_listen_port),
    CowboyOpts = #{
        middlewares => [ z_sites_dispatcher, z_cowmachine_middleware ],
        env => #{}
    },

    lager:info("Web server listening on IPv4 ~p:~p, SSL ~p::~p", [WebIp, WebPort, WebIp, SSLPort]),
    {ok, _} = cowboy:start_clear(
        zotonic_http_listener_ipv4,
        z_config:get(inet_acceptor_pool_size),
        [   inet,
            {port, WebPort},
            {backlog, z_config:get(inet_backlog)}
            | case WebIp of any -> []; _ -> [{ip, WebIp}] end
        ],
        CowboyOpts),
    {ok, _} = cowboy:start_tls(
        zotonic_https_listener_ipv4,
        z_config:get(ssl_acceptor_pool_size),
        [   inet,
            {port, SSLPort},
            {backlog, z_config:get(ssl_backlog)},
            {certfile, "via_sni_fun"},
            {sni_fun, fun ?MODULE:sni_fun/1}
            | case WebIp of any -> []; _ -> [{ip, WebIp}] end
        ],
        CowboyOpts),

    case {WebIp, ipv6_supported()} of
        {any, true} ->
            lager:info("Web server listening on IPv6 ::~p, SSL ::~p", [WebPort, SSLPort]),
            {ok, _} = cowboy:start_clear(
                zotonic_http_listener_ipv6,
                z_config:get(inet_acceptor_pool_size),
                [   inet6,
                    {ipv6_v6only, true},
                    {port, WebPort},
                    {backlog, z_config:get(inet_backlog)}
                ],
                CowboyOpts),
            {ok, _} = cowboy:start_tls(
                zotonic_https_listener_ipv6,
                z_config:get(ssl_acceptor_pool_size),
                [   inet6,
                    {ipv6_v6only, true},
                    {port, SSLPort},
                    {backlog, z_config:get(ssl_backlog)},
                    {certfile, "via_sni_fun"},
                    {sni_fun, fun ?MODULE:sni_fun/1}
                ],
                CowboyOpts);
        _ ->
            ok
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
    Files = z_utils:wildcard(filename:join([z_utils:lib_dir(priv), "extensions", "ext_*"])),
    [
     begin
         Module = list_to_atom(filename:basename(F)),
         {Module,
          {Module, start_link, []},
          permanent, 5000, worker, dynamic}
     end
     || F <- Files].

%% @doc Let sites return their own keys and certificates.
sni_fun(Hostname) ->
    case z_sites_dispatcher:get_site_for_hostname(Hostname) of
        undefined -> undefined;
        {ok, Host} ->
            z_notifier:first(#ssl_options{server_name=Hostname}, z_context:new(Host))
    end.
