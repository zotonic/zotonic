%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%%
%% @doc FSM for sending handoff commands from one node to another

%% Copyright 2011 Marc Worrell
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

-module(zynamo_handoff_fsm).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([
    start_link/1
]).
-export([
    check_node_active/2,
    collect_services/2,
    handoff_service/2
]).

-include_lib("zynamo.hrl").


-record(state, {
    node :: node(),
    services :: [ {atom(), atom()} ]
}).

% Check every 10-20 seconds if there is anything to handoff
-define(HANDOFF_RANDOM_TIMEOUT, 10000).
-define(HANDOFF_SHORT_TIMEOUT,  10000).
-define(HANDOFF_LONG_TIMEOUT,   60000).

% Max timeout for the handoff command, the service might tarpit us, so we need a long timeout.
-define(HANDOFF_COMMAND_TIMEOUT, 300000).


%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link(list()) ->
                        {ok, pid()}
                      | {error, term()}.
start_link(Args) ->
    gen_fsm:start_link(?MODULE, [Args], []).


%% ====================================================================
%% gen_fsm callbacks
%% ====================================================================

init([Node]) ->
    {ok, check_node_active, #state{
        node=Node,
        services=[]
    }, handoff_timeout(short)}.


check_node_active(timeout, #state{node=Node} = State) ->
    case zynamo_manager:is_node_active(Node) of
        {ok, true} -> {next_state, collect_services, State, 0};
        _IsNotUp -> {next_state, check_node_active, State, handoff_timeout(long)}
    end.

collect_services(timeout, #state{node=Node} = State) ->
    {ok, Here} = zynamo_manager:list_node_services(node()),
    {ok, There} = zynamo_manager:list_node_services(Node),
    Overlap = lists:filter(fun(Service) -> lists:member(Service, There) end, Here),
    {next_state, handoff_service, State#state{services=Overlap}, 0}.


handoff_service(timeout, #state{services=[]} = State) ->
    {next_state, check_node_active, State, handoff_timeout(long)};
handoff_service(timeout, #state{services=[{Site,Service}|Rest], node=Node} = State) ->
    case do_handoff_check(Site, Service, Node) of
        {ok, done} ->
            {next_state, handoff_service, State#state{services=Rest}, 0};
        {ok, HandoffCommand} ->
            case do_handoff_command(Site, Service, Node, HandoffCommand) of
                ok ->
                    % Next handoff command for the same service
                    {next_state, handoff_service, State, 0};
                {error, _Reason} ->
                    % Error, skip service, try again later
                    {next_state, handoff_service, State#state{services=Rest}, 0}
            end;
        {error, _Reason} ->
            %% TODO: log Reason
            {next_state, handoff_service, State#state{services=Rest}, 0}
    end.

%% @doc When the node comes up, then cut the polling interval short 
handle_event(nodeup, _StateName, State) ->
    {next_state, check_node_active, State, handoff_timeout(short)};
handle_event(Event, _StateName, State) ->
    {stop, {unknown_event, Event}, State}.

handle_sync_event(Event, _From, _StateName, State) ->
    {stop, {unknown_sync_event, Event}, State}.

handle_info(Info, _StateName, State) ->
    {stop, {unknown_info, Info}, State}.

terminate(Reason, _StateName, _State) ->
    Reason.

code_change(_OldVsn, StateName, State, _Extra) -> 
    {ok, StateName, State}.


%% ====================================================================
%% internal support routines
%% ====================================================================


do_handoff_check(Site, Service, Node) ->
    case zynamo_manager:get_service_pid(Site, Service, node()) of
        {ok, Pid} -> gen_server:call(Pid, {handoff_check, Node});
        {error, _Reason} = Error -> Error
    end.

do_handoff_command(Site, Service, Node, HandoffCommand) ->
    case zynamo_request:command(Site, 
                                Service,
                                HandoffCommand,
                                [
                                   {node, [Node]},
                                   {n,1},
                                   {quorum,n},
                                   no_handoff,
                                   {timeout, ?HANDOFF_COMMAND_TIMEOUT}
                                ])
    of
        [{Node, _Handoff, {ok, _Version}}] ->
            % Report back to our local service that the handoff has been done
            case zynamo_manager:get_service_pid(Site, Service, node()) of
                {ok, Pid} -> gen_server:cast(Pid, {handoff_done, Node, HandoffCommand});
                {error, _Reason} = Error -> Error
            end;
        [{Node, _Handoff, {error, {conflict, _OtherVersion}}}] ->
            case zynamo_manager:get_service_pid(Site, Service, node()) of
                {ok, Pid} -> gen_server:cast(Pid, {handoff_done, Node, HandoffCommand});
                {error, _Reason} = Error -> Error
            end;
        [{Node, _Handoff, {error, _Reason} = Error}] ->
            Error;
        {error, _Reason} = Error ->
            Error
    end.


handoff_timeout(short) ->
    ?HANDOFF_SHORT_TIMEOUT + zynamo_random:uniform(?HANDOFF_RANDOM_TIMEOUT);
handoff_timeout(long) ->
    ?HANDOFF_LONG_TIMEOUT + zynamo_random:uniform(?HANDOFF_RANDOM_TIMEOUT).

