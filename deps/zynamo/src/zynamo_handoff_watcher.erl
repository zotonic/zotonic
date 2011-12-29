%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc Ensure that we have a handoff_fsm running for every node that came up.

%% Copyright 2010-2011 Marc Worrell
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

-module(zynamo_handoff_watcher).
-behaviour(gen_event).

-export([
    start/0,
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).


start() ->
    zynamo_event:add_handler(?MODULE, []),
    {ok, Nodes} = zynamo_manager:nodes(),
    [ zynamo_handoff_fsm_sup:start_fsm(Node) || Node <- Nodes ],
    ok.


init(_Args) ->
    {ok, []}.

handle_event({nodeup, Node}, State) ->
    zynamo_handoff_fsm_sup:start_fsm(Node),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Msg, State) ->
    {ok, ok, State}.

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.
