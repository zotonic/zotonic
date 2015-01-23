%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @doc Websocket Interface

%% Copyright 2014 Maas-Maarten Zeeman
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

-module(z_websocket).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([
    start/2,

    websocket_init/2,
    websocket_handle/3,
    websocket_info/3,
    websocket_handle_event/3
]).

-compile([{parse_transform, lager_transform}]).

-include_lib("zotonic.hrl").

% Send a ping every 30 seconds.
-define(PING_TIMEOUT, 30000).

start(ReqData, Context) ->
    Compress = false,
    ReqAdapter = z_ws_request_adapter:init(ReqData, Compress),
    z_ws_protocol:upgrade(ReqAdapter, [], ?MODULE, Context).

%%
%% Websocket callback handlers.
%%

websocket_init(_ReqData, #context{}=Context) ->
    H = z_context:get(ws_handler, Context),
    ok = H:websocket_init(Context),
    {ok, [], Context}.

websocket_handle(_ReqData, {text, Msg}, #context{}=Context) ->
    H = z_context:get(ws_handler, Context),
    z_depcache:in_process(true),
    case H:websocket_message(Msg, self(), Context) of
        ok ->
            z_utils:erase_process_dict(),
            {ok, Context};
        {ok, Context1} ->
            z_utils:erase_process_dict(),
            {ok, z_context:prune_for_scomp(Context1)}
    end;
websocket_handle(_ReqData, {pong, _Msg}, #context{}=Context) ->
    %% Received pong back from client... still alive.
    {ok, Context};
websocket_handle(_ReqData, {ws_ping, _Payload}, #context{}=Context) ->
    %% Ping control frame - ignore
    {ok, Context};
websocket_handle(_ReqData, {ws_pong, _Payload}, #context{}=Context) ->
    %% Pong control frame - ignore
    {ok, Context};
websocket_handle(_ReqData, Msg, #context{}=Context) ->
    lager:info("z_websocket: Received unexpected message from client. ~p~n", [Msg]),
    {ok, Context}.

websocket_info(_ReqData, ping, #context{}=Context) ->
    %% Send a ping to see if the UA is still listening
    erlang:send_after(?PING_TIMEOUT, self(), ping),
    {reply, {ping, <<"Zzz?">>}, Context};
websocket_info(_ReqData, {send_data, Message}, #context{}=Context) ->
    {reply, {text, Message}, Context};
websocket_info(_ReqData, Msg, #context{}=Context) ->
    H = z_context:get(ws_handler, Context),
    ok = H:websocket_info(Msg, self(), Context),
    {ok, Context}.

% @doc Websocket events.
%
websocket_handle_event(websocket_open, _Args, _Context) ->
    %% Start pinging the ua when the websocket is open.
    erlang:send_after(?PING_TIMEOUT, self(), ping),
    ok;
websocket_handle_event(websocket_close, _Args, Context) ->
    H = z_context:get(ws_handler, Context),
    H:websocket_terminate(normal, Context);

websocket_handle_event(websocket_error, Args, Context) ->
    handle_error(websocket_error, Args, Context);
websocket_handle_event(websocket_exit, Args, Context) ->
    handle_error(websocket_exit, Args, Context);
websocket_handle_event(websocket_throw, Args, Context) ->
    handle_error(websocket_throw, Args, Context).

handle_error(Event, Args, Context) ->
    lager:error("z_websocket: ~p: ~p~n", [Event, Args]),
    H = z_context:get(ws_handler, Context),
    H:websocket_terminate(Event, Context).

