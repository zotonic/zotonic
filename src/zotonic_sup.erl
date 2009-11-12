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
    % Listen to IP address and Port
    WebIp      = case os:getenv("ZOTONIC_IP")   of
        Any when Any == false; Any == []; Any == "*"; Any == "any" -> any;
        ConfIP -> ConfIP 
    end,   
    WebPort    = case os:getenv("ZOTONIC_PORT") of false -> 8000; Anyport -> list_to_integer(Anyport) end,   

    WebConfig = [
		 {ip, WebIp},
		 {port, WebPort},
		 {error_handler, z_webmachine_error_handler},
         {log_dir, filename:join([code:lib_dir(zotonic, priv), "log"])},
		 {dispatch, []},
		 {backlog, 500}
	],

    % Listen to the ip address and port for all sites.
    Webmachine = {webmachine_mochiweb,
	            {webmachine_mochiweb, start, [WebConfig]}, 
	            permanent, 5000, worker, dynamic},

    % Random id generation
    Ids     = {z_ids,
	            {z_ids, start_link, []}, 
	            permanent, 5000, worker, dynamic},
    
    % Database connection, connections are made by z_site_sup instances.
    Postgres = {epgsql_pool,
                {epgsql_pool, start_link, [[]]},
                permanent, 5000, worker, dynamic},
    
    % Image resizer, prevents to many images to be resized at once, bogging the processor.
    PreviewServer = {z_media_preview_server,
                {z_media_preview_server, start_link, []}, 
                permanent, 5000, worker, dynamic},

    % Sites supervisor, starts all enabled sites
    SitesSup = {z_sites_sup,
                {z_sites_sup, start_link, []},
                permanent, 5000, worker, dynamic},
                
    Processes = [
            Webmachine, Ids, Postgres, PreviewServer, SitesSup
    ],
    {ok, {{one_for_one, 1000, 10}, Processes}}.

