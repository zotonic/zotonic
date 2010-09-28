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

    % Sites supervisor, starts all enabled sites
    SitesSup = {z_sites_manager,
                {z_sites_manager, start_link, []},
                permanent, 5000, worker, dynamic},
                
    Processes = [
            Ids, Config, PreviewServer, Dispatcher, SitesSup
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
    z_config:set_dirty(listen_ip, WebIp),
    z_config:set_dirty(listen_port, WebPort),

    WebConfig = [
         {port, WebPort},
         {error_handler, z_config:get_dirty(webmachine_error_handler)},
         {enable_perf_logger, z_config:get_dirty(enable_perf_logger)},
         {log_dir, z_config:get_dirty(log_dir)},
         {dispatch, []},
         {backlog, z_config:get_dirty(inet_backlog)}
    ],
    application:set_env(webmachine, webmachine_logger_module, webmachine_logger),

    % Listen to the ip address and port for all sites.
    Processes1 = Processes ++ [{webmachine_mochiweb,
                                {webmachine_mochiweb, start, [webmachine_mochiweb, [{ip,WebIp}|WebConfig]]},
                                permanent, 5000, worker, dynamic}],
    
    %% When binding to all IP addresses ('any'), bind separately for ipv6 addresses
    Processes2 = case WebIp of
        any -> 
            case ipv6_supported() of
                true ->
                    Processes1 ++ [{webmachine_mochiweb_v6,
                                    {webmachine_mochiweb, start, [webmachine_mochiweb_v6, [{ip,any6}|WebConfig]]}, 
                                    permanent, 5000, worker, dynamic}];
                false ->
                    Processes1
            end;
        _ -> Processes1
    end,

    {ok, {{one_for_one, 1000, 10}, Processes2}}.


%% @todo Exclude platforms that do not support raw ipv6 socket options
ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} -> true;
        {error, _} -> false
    end.