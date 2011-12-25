%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-12-24

%% @doc Handles zynamo events for zotonic status sites overview

%% Copyright 2011 Arjan Scherpenisse
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


-module(zotonic_status_zynamo_page_events).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-behaviour(gen_event).

-export([init/1]).
-export([
	handle_event/2,
	terminate/2,
	code_change/3,
	handle_call/2,
	handle_info/2
]).

-include("zotonic.hrl").

-record(state, {context, timer}).

%%% gen_event callbacks
init(Context) -> {ok, #state{context=Context}}.

%% @doc Update the node status table
handle_event(_Event, State) ->
    case State#state.timer of
        undefined -> nop;
        _ -> timer:cancel(State#state.timer)
    end,
    {ok, Timer} = timer:send_after(200, render),
    {ok, State#state{timer=Timer}}.

handle_call(_, State) -> {stop, not_supported, State}.

handle_info(render, State) -> 
    render_update(State#state.context),
    {ok, State#state{timer=undefined}};
handle_info(_, State) -> {ok, State}.

terminate(_Reason, State) ->
	State.

code_change(_OldVsn, State, _Extra) ->
	{stop, not_supported, State}.

render_update(Context) ->
    Context1 = z_render:update("nodes", scomp_zotonic_status_nodes_status:ring_status_html(Context), Context),
    z_session_page:add_script(Context1).

    
