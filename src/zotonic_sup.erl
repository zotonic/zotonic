%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Supervisor for the zotonic application.

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

-module(zotonic_sup).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-include_lib("zotonic.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    {A1,A2,A3} = erlang:now(),
    random:seed(A1, A2, A3),

    % Random id generation
    Ids     = {z_ids,
               {z_ids, start_link, []}, 
               permanent, 5000, worker, dynamic},
    
    % File based configuration, manages the file priv/config
    Config  = {z_config,
               {z_config, start_link, []},
               permanent, 5000, worker, dynamic},

    % Image resizer, prevents to many images to be resized at once, bogging the processor.
    PreviewServer = {z_media_preview_server,
                     {z_media_preview_server, start_link, []}, 
                     permanent, 5000, worker, dynamic},

    % Sites disapatcher, matches hosts and paths to sites and resources.
    Dispatcher = {z_sites_dispatcher,
                  {z_sites_dispatcher, start_link, []},
                  permanent, 5000, worker, dynamic},
              
    % SMTP gen_servers: one for encoding and sending mails, the other for bounces
    SmtpServer = {z_email_server,
                  {z_email_server, start_link, []},
                  permanent, 5000, worker, dynamic},

    % Smtp listen to IP address, Domain and Port
    SmtpListenDomain = case os:getenv("ZOTONIC_SMTP_LISTEN_DOMAIN") of
                false -> z_config:get_dirty(smtp_listen_domain);
                SmtpListenDomain_ -> SmtpListenDomain_
            end,
    SmtpListenIp = case os:getenv("ZOTONIC_SMTP_LISTEN_IP") of
                false -> z_config:get_dirty(smtp_listen_ip);
                SmtpListenAny when SmtpListenAny == []; SmtpListenAny == "*"; SmtpListenAny == "any" -> any;
                SmtpListenIp_-> SmtpListenIp_
            end,   
    SmtpListenPort = case os:getenv("ZOTONIC_SMTP_LISTEN_PORT") of
                     false -> z_config:get_dirty(smtp_listen_port);
                     SmtpListenPort_ -> list_to_integer(SmtpListenPort_)
                 end,
    z_config:set_dirty(smtp_listen_domain, SmtpListenDomain),
    z_config:set_dirty(smtp_listen_ip, SmtpListenIp),
    z_config:set_dirty(smtp_listen_port, SmtpListenPort),

    SmtpBounceServer = {z_email_receive_server,
                        {z_email_receive_server, start_link, []},
                        permanent, 5000, worker, dynamic},

    % Sites supervisor, starts all enabled sites
    SitesSup = {z_sites_manager,
                {z_sites_manager, start_link, []},
                permanent, 5000, worker, dynamic},
                
    Processes = [
        Ids, Config, PreviewServer, Dispatcher,
        SmtpServer, SmtpBounceServer, 
        SitesSup
                ],

    % Listen to IP address and Port
    WebIp = case os:getenv("ZOTONIC_IP") of
                false -> z_config:get_dirty(listen_ip);
                Any when Any == []; Any == "*"; Any == "any" -> any;
                ConfIP -> ConfIP 
            end,   
    WebPort = case os:getenv("ZOTONIC_PORT") of
                  false -> z_config:get_dirty(listen_port); 
                  Anyport -> list_to_integer(Anyport) 
              end,
    WebPortSSL = case os:getenv("ZOTONIC_PORT_SSL") of
                     false -> z_config:get_dirty(listen_port_ssl); 
                     Anyport_ -> list_to_integer(Anyport_)
                 end,      
    z_config:set_dirty(listen_ip, WebIp),
    z_config:set_dirty(listen_port, WebPort),
    z_config:set_dirty(listen_port_ssl, WebPortSSL),    

    WebConfig = [ 
        {dispatcher, z_sites_dispatcher},
        {dispatch_list, []},
        {backlog, z_config:get_dirty(inet_backlog)}
                ],
    
    NonSSLOpts = [{port, WebPort}],
    SSLCertOpts = [{certfile, z_config:get_dirty(ssl_certfile)}, 
                   {keyfile, z_config:get_dirty(ssl_keyfile)}],
    SSLCertOpts2 = case z_config:get_dirty(ssl_cacertfile) of
                       undefined -> SSLCertOpts;
                       CACertFile -> [{cacertfile, CACertFile} | SSLCertOpts]
                   end,
    SSLOpts = [
        {port, WebPortSSL},
        {ssl, true},  
        {ssl_opts, SSLCertOpts2}
              ],

    IPv4Opts = [{ip, WebIp}], % Listen to the ip address and port for all sites.
    IPv6Opts = [{ip, any6}],

    % Webmachine/Mochiweb processes
    [IPv4Proc, IPv4SSLProc, IPv6Proc, IPv6SSLProc] =
        [[{Name,
           {webmachine_mochiweb, start,
            [Name, Opts]},                                 
           permanent, 5000, worker, dynamic}]
         || {Name, Opts} 
         <- [{webmachine_mochiweb, IPv4Opts ++ NonSSLOpts ++ WebConfig},
             {webmachine_mochiweb_ssl, IPv4Opts ++ SSLOpts ++ WebConfig},
             {webmachine_mochiweb_v6, IPv6Opts ++ NonSSLOpts ++ WebConfig},
             {webmachine_mochiweb_v6_ssl, IPv6Opts ++ SSLOpts ++ WebConfig}]],

    %% When binding to all IP addresses ('any'), bind separately for ipv6 addresses
    EnableIPv6 = case WebIp of
                     any -> ipv6_supported();
                     _ -> false
                 end,
    EnableSSL = z_config:get_dirty(ssl),
   
    Processes1 = 
        case {EnableIPv6, EnableSSL} of
            {true, true} ->
                Processes ++ IPv4Proc ++ IPv4SSLProc ++ IPv6Proc ++ IPv6SSLProc;
            {true, false} ->
                Processes ++ IPv4Proc ++ IPv6Proc;
            {false, true} ->
                Processes ++ IPv4Proc ++ IPv4SSLProc;
            {false, false} ->
                Processes ++ IPv4Proc
        end,

    init_webmachine(),

    {ok, {{one_for_one, 1000, 10}, Processes1}}.

%% @doc Sets the application parameters for webmachine and starts the logger processes.
%%      NOTE: This part has been removed from webmachine_mochiweb:start/2 to avoid
%%      messing with application parameters when starting up a new wm-mochiweb process.
init_webmachine() -> 
    ErrorHandler0 = z_config:get_dirty(webmachine_error_handler),        
    ErrorHandler = 
        case ErrorHandler0 of 
            undefined ->
                webmachine_error_handler;
            EH -> EH
        end,
    application:set_env(webzmachine, server_header, webmachine_request:server_header() ++ " Zotonic/" ++ ?ZOTONIC_VERSION),
    application:set_env(webzmachine, error_handler, ErrorHandler),        
        
    LogDir = z_config:get_dirty(log_dir),
    
    application:set_env(webzmachine, webmachine_logger_module, webmachine_logger),
    webmachine_sup:start_logger(LogDir),
    
    case z_config:get_dirty(enable_perf_logger) of
        true ->
            application:set_env(webzmachine, enable_perf_logger, true),
            webmachine_sup:start_perf_logger(LogDir);
        _ ->
            ignore
    end.

%% @todo Exclude platforms that do not support raw ipv6 socket options
ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} -> true;
        {error, _} -> false
    end.
