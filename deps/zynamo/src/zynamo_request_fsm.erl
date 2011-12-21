%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%%
%% @doc FSM for sending commands to services on nodes.

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

-module(zynamo_request_fsm).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([
    start_link/5
]).
-export([
    prepare/2,
    validate/2,
    execute/2,
    waiting_reply/2,
    return_replies/2,
    waiting_rest/2,
    finalize/2
]).

-include_lib("zynamo.hrl").

-record(state, {
    from            :: pid(),
    command         :: #zynamo_command{},
    pref_nodes      :: [ node() ],
    service_nodes   :: [ {node(),pid()} ],
    participating_nodes :: [ {node(), reference(), pos_integer(), [node()]} ],
    batch_nr        :: pos_integer(),
    primary_node    :: node(),
    unused_nodes    :: [ node() ],
    received_ok     :: [ {node(), [node()], term()} ],
    received_fail   :: [ {node(), [node()], term()} ],
    options         :: zynamo_request_options(),
    start_time      :: {non_neg_integer(),non_neg_integer(),non_neg_integer()},
    request_timeout :: pos_integer(),
    node_timeout    :: pos_integer(),
    n               :: pos_integer(),
    quorum          :: pos_integer()
}).

-define(NODE_TIMEOUT, 5000).
-define(FINAL_TIMEOUT, 20000).

-define(DEFAULT_N, 3).
-define(DEFAULT_QUORUM, 2).


%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link(pid(), #zynamo_command{}, [node()], [{node(),pid()}], zynamo_request_options()) ->
                        {ok, pid()}
                      | {error, term()}.
start_link(From, Command, PreferenceNodes, ServiceNodes, Options) ->
    gen_fsm:start_link(?MODULE, [From, Command, PreferenceNodes, ServiceNodes, Options], []).



%% ====================================================================
%% gen_fsm callbacks
%% ====================================================================

init([From, Command, PreferenceNodes, ServiceNodes, Options]) ->
    {ok, prepare, #state{
        from=From,
        command=Command,
        pref_nodes=PreferenceNodes,
        service_nodes=ServiceNodes,
        participating_nodes=[],
        batch_nr=1,
        unused_nodes=[],
        received_ok=[],
        received_fail=[],
        options=Options,
        start_time=now(),
        request_timeout=proplists:get_value(request_timeout, Options, ?ZYNAMO_REQUEST_TIMEOUT),
        node_timeout=proplists:get_value(node_timeout, Options, ?NODE_TIMEOUT),
        n=proplists:get_value(n, Options, ?DEFAULT_N),
        quorum=proplists:get_value(quorum, Options, ?DEFAULT_QUORUM)
    }, 0}.


prepare(timeout, #state{pref_nodes=PrefNodes, batch_nr=BatchNr, service_nodes=ServiceNodes, n=N} = State) ->
    {PrefN,Unused} = case N of
                       all -> {PrefNodes, []};
                       N when is_integer(N) -> take(N, PrefNodes)
                     end,
    UnusedService = lists:filter(fun(Node) ->
                                     lists:keyfind(Node, 1, ServiceNodes) =/= false
                                 end,
                                 Unused),
    % Check if the Pref nodes have the service running, if not then handoff to Unused
    % or one of the participating nodes.
    {PrefOk, PrefHandoff} = lists:partition(fun(Node) -> 
                                                lists:keyfind(Node, 1, ServiceNodes) =/= false 
                                            end,
                                            PrefN),
    PrefOkHF = [{Node, erlang:make_ref(), BatchNr, [Node]} || Node <- PrefOk ],
    % Assign handoff nodes for the preferred nodes that are not up
    case handoff(PrefHandoff, PrefOkHF, UnusedService, BatchNr) of
        no_nodes ->
            {next_state, validate, State#state{
                        participating_nodes = [],
                        unused_nodes = []
            }, 0};
        {ok, InitialNodes, UnusedNodes} ->
            {next_state, validate, State#state{
                        participating_nodes = InitialNodes,
                        unused_nodes = UnusedNodes
            }, 0}
    end.


validate(timeout, #state{participating_nodes=[]} = State) ->
    client_reply({error, no_nodes}, State),
    {stop, normal, State};
validate(timeout, #state{} = State) ->
    {PrimaryNode, _Ref, _BatchNr, _Handoff} = hd(State#state.participating_nodes),
    {next_state, execute, State#state{
                primary_node=PrimaryNode
    }, 0}.


execute(timeout, #state{
                    node_timeout=NodeTimeout, 
                    request_timeout=RequestTimeout
            } = State) ->
    erlang:send_after(NodeTimeout, self(), node_timeout),
    erlang:send_after(RequestTimeout, self(), request_timeout),
    send_command(State#state.participating_nodes, State),
    {next_state, waiting_reply, State}.


waiting_reply({error, FromNode, Ref, Reason}, State) ->
    % A node tells that he is unavailable, add some other nodes to the request
    % Fetch the handoff list of the node
    case lists:keytake(Ref, 2, State#state.participating_nodes) of
        {value, {FromNode, Ref, BatchNr, Handoffs}, RestParticipating} ->
            ReceivedFail = [ {FromNode, Handoffs, Reason} | State#state.received_fail ],
            State1 = State#state{
                received_fail=ReceivedFail,
                participating_nodes=RestParticipating
            },
            State2 = case State#state.batch_nr of
                        BatchNr ->
                            % Current request batch, allocate handoff nodes
                            error_handoff(Handoffs, State1);
                        _OlderBatch -> 
                            % Old request batch, already handoffs in progress
                            State1
                    end,
            case wait_for_quorum(State2) of
                false -> {next_state, return_replies, State2, 0};
                true -> {next_state, waiting_reply, State2}
            end;
        false ->
            % Node was not participating (anymore) - ignore
            {next_state, waiting_reply, State}
    end;
waiting_reply({ok, FromNode, Ref, Reply}, State) ->
    % Received a result from a node, collect all results
    case lists:keytake(Ref, 2, State#state.participating_nodes) of
        {value, {FromNode, Ref, _BatchNr, Handoff}, RestParticipating} ->
            Oks = [ {FromNode, Handoff, Reply} | State#state.received_ok ],
            State1 = State#state{
                participating_nodes = RestParticipating,
                received_ok = Oks
            },
            case wait_for_quorum(State1) of
                false -> {next_state, return_replies, State1, 0};
                true -> {next_state, waiting_reply, State1}
            end;
        false ->
            % Node was not participating (anymore) - ignore
            {next_state, waiting_reply, State}
    end;
waiting_reply(node_timeout, #state{node_timeout=NodeTimeout, 
                                   batch_nr=BatchNr,
                                   received_fail=Fails,
                                   participating_nodes=ParticipatingNodes} = State) ->
    % Assume the participating nodes are failing, add some nodes for their work.
    % We keep the old nodes on the participating list, so a slow node can still count against quorum.
    % Reallocate all requests from the former batch to a new batch.
    Timeout = [ {Node, HO, node_timeout} || {Node, _Req, BNr, HO} <- ParticipatingNodes, BNr == BatchNr ],
    Handoffs = lists:flatten([ HO || {_Node, _Req, BNr, HO} <- ParticipatingNodes, BNr == BatchNr ]),
    State1 = error_handoff_unused(Handoffs, State#state{batch_nr=BatchNr+1, received_fail=Fails++Timeout}),
    erlang:send_after(NodeTimeout, self(), node_timeout),
    {next_state, waiting_reply, State1};
waiting_reply(request_timeout, State) ->
    {next_state, return_replies, State, 0}.


return_replies(timeout, #state{options=Options, participating_nodes=ParticipatingNodes} = State) ->
    % Combine all replies, send result to caller.
    client_reply(State#state.received_ok, State),
    case proplists:get_value(final_action, Options) of
        undefined ->
            {next_state, finalize, State, 0};
        _Action ->
            case ParticipatingNodes of
                [] -> 
                    {next_state, finalize, State, 0};
                _ ->
                    erlang:send_after(?FINAL_TIMEOUT, self(), final_timeout),
                    {next_state, waiting_rest, State}
            end
    end;
% Late results
return_replies({error, _From, _Ref, _Reason} = Error, State) ->
    {next_state, return_replies, late_result(Error, State), 0};
return_replies({ok, _FromNode, _Ref, _Reply} = Ok, State) ->
    {next_state, return_replies, late_result(Ok, State), 0};
% Ignore late timeouts.
return_replies(node_timeout, State) ->
    {next_state, return_replies, State, 0};
return_replies(request_timeout, State) ->
    {next_state, return_replies, State, 0}.


%% @doc After we reached the quorum we might wait longer for read repairs or other actions.
waiting_rest({error, _FromNode, _Ref, _Reason} = Error, State) ->
    State1 = late_result(Error, State),
    case State1#state.participating_nodes of
        [] -> {next_state, finalize, State1, 0};
        _ -> {next_state, waiting_rest, State1}
    end;
waiting_rest({ok, _FromNode, _Ref, _Reply} = Ok, State) ->
    State1 = late_result(Ok, State),
    case State1#state.participating_nodes of
        [] -> {next_state, finalize, State1, 0};
        _ -> {next_state, waiting_rest, State1}
    end;
waiting_rest(final_timeout, State) ->
    {next_state, finalize, State, 0};
% Ignore late timeouts.
waiting_rest(request_timeout, State) ->
    {next_state, waiting_rest, State};
waiting_rest(node_timeout, State) ->
    {next_state, waiting_rest, State}.


finalize(timeout, #state{options=Options} = State) ->
    % Let the final handler perform its actions (read-repair etc)
    final_action(proplists:get_value(final_action, Options), State),
    % Update stats etc.
    % TODO
    {stop, normal, State};
% Ignore late results (edge case)
finalize({ok, _FromNode, _Ref, _Reply} = Ok, State) ->
    {next_state, finalize, late_result(Ok, State), 0};
finalize({error, _FromNode, _Ref, _Reason} = Error, State) ->
    {next_state, finalize, late_result(Error, State), 0};
% Ignore late timeouts.
finalize(final_timeout, State) ->
    {next_state, finalize, State, 0};
finalize(request_timeout, State) ->
    {next_state, finalize, State};
finalize(node_timeout, State) ->
    {next_state, finalize, State}.
    


handle_event(Event, _StateName, State) ->
    {stop, {unknown_event, Event}, State}.

handle_sync_event(Event, _From, _StateName, State) ->
    {stop, {unknown_sync_event, Event}, State}.

handle_info(request_timeout, StateName, State) ->
    ?MODULE:StateName(request_timeout, State);
handle_info(node_timeout, StateName, State) ->
    ?MODULE:StateName(node_timeout, State);
handle_info(final_timeout, StateName, State) ->
    ?MODULE:StateName(final_timeout, State);
handle_info({ok, _, _, _} = Ok, StateName, State) ->
    ?MODULE:StateName(Ok, State);
handle_info({error, _, _, _} = Error, StateName, State) ->
    ?MODULE:StateName(Error, State);
handle_info(Info, _StateName, State) ->
    {stop, {unknown_info, Info}, State}.

terminate(Reason, _StateName, _State) ->
    Reason.

code_change(_OldVsn, StateName, State, _Extra) -> 
    {ok, StateName, State}.


%% ====================================================================
%% internal support routines
%% ====================================================================


client_reply(Reply, #state{from=From, command=Command} = State) ->
    From ! {result, Command#zynamo_command.ref, Reply, get_stats(State)}.


%% Handle late incoming results, no handoffs needed.
late_result({error, FromNode, Ref, Reason}, State) ->
    case lists:keytake(Ref, 2, State#state.participating_nodes) of
        {value, {FromNode, Ref, _BatchNr, Handoffs}, RestParticipating} ->
            ReceivedFail = [ {FromNode, Handoffs, Reason} | State#state.received_fail ],
            State#state{
                received_fail=ReceivedFail,
                participating_nodes=RestParticipating
            };
        false ->
            State
    end;
late_result({ok, FromNode, Ref, Reply}, State) ->
    case lists:keytake(Ref, 2, State#state.participating_nodes) of
        {value, {FromNode, Ref, _BatchNr, Handoff}, RestParticipating} ->
            Oks = [ {FromNode, Handoff, Reply} | State#state.received_ok ],
            State#state{
                participating_nodes = RestParticipating,
                received_ok = Oks
            };
        false ->
            State
    end.


%% @doc Perform the final action after the command is finalized (and waiting for late data)
final_action(read_repair, #state{command=Command, received_ok=Oks, received_fail=Fails}) ->
    zynamo_read_repair:final_action(Command, Oks, Fails);
final_action({M,F}, #state{command=Command, received_ok=Oks, received_fail=Fails}) ->
    M:F(Command, Oks, Fails);
final_action(Pid, #state{command=Command, received_ok=Oks, received_fail=Fails}) when is_pid(Pid) ->
    Pid ! {zynamo_final, Command, Oks, Fails};
final_action(F, #state{command=Command, received_ok=Oks, received_fail=Fails}) when is_function(F) ->
    F(Command, Oks, Fails);
final_action(undefined, _State) ->
    ok.


%% @doc Check if the required quorum has been reached or is impossible to reach
wait_for_quorum(#state{received_ok=Oks, received_fail=Fails, service_nodes=ServiceNodes, quorum=Quorum}) ->
    case count_nodes(Oks) >= Quorum of
        true -> 
            % Got enough results
            false;
        false -> 
            % Only return 'true' when we are able to get enough results
            count_nodes(Fails) + Quorum =< length(ServiceNodes)
    end.


%% @doc Count the nodes in a received list.
count_nodes(L) ->
    length(get_nodes(L)).

get_nodes(L) ->
    get_nodes(L, []).

    get_nodes([], Acc) ->
        Acc;
    get_nodes([{N,_,_}|Rest], Acc) ->
        case lists:member(N, Acc) of
            true -> get_nodes(Rest, Acc);
            false -> get_nodes(Rest, [N|Acc])
        end.
    


%% TODO: define & fill the command stats
get_stats(_State) ->
    undefined.

%% @doc Send the command to the list of nodes
send_command(Nodes, State) ->
    ServiceNodes = State#state.service_nodes,
    PrimaryNode = State#state.primary_node,
    Command = State#state.command,
    [
        begin
            {Node, Pid} = lists:keyfind(Node, 1, ServiceNodes),
            Cmd = #zynamo_service_command{
                ref=Ref,
                is_primary= (Node =:= PrimaryNode),
                from=self(),
                handoff=Handoff,
                command=Command
            },
            gen_server:cast(Pid, Cmd)
        end
        || {Node, Ref, _BatchNr, Handoff} <- Nodes
    ].


%% @doc Received an error return from a node, try some other node
error_handoff(Handoff, State) ->
    Participating = [ Node || {Node,_,_,_} <- State#state.participating_nodes ],
    error_handoff(Handoff, State#state.unused_nodes++Participating, State).

error_handoff_unused(Handoff, State) ->
    error_handoff(Handoff, State#state.unused_nodes, State).

error_handoff(Handoff, Candidates, State) ->
    case handoff(Handoff, [], Candidates, State#state.batch_nr) of
        {ok, NewParticipating, NewUnused} ->
            send_command(NewParticipating, State),
            State#state{
                participating_nodes=NewParticipating ++ State#state.participating_nodes,
                unused_nodes=NewUnused
            };
        no_nodes ->
            State
    end.

%% @doc Assign the to-be-handed off nodes to unused nodes or participating nodes
handoff([], SelectedHF, Unused, _BatchNr) ->
    {ok, SelectedHF, Unused};
handoff([_|_], [], [], _BatchNr) ->
    no_nodes;
handoff([Node|Other], SelectedHF, [], BatchNr) ->
    % Add to random other node in the already participating nodes
    N = zynamo_random:uniform(length(SelectedHF)),
    SelectedHF1 = add_node(N, Node, SelectedHF, []),
    handoff(Other, SelectedHF1, [], BatchNr);
handoff([Node|RN], SelectedHF, [Unused|RU], BatchNr) ->
    handoff(RN, [{Unused, erlang:make_ref(), BatchNr, [Node]}|SelectedHF], RU, BatchNr).

    add_node(1, Node, [{N,Ref,BatchNr,HF}|Rest], Acc) ->
        lists:reverse([{N,Ref,BatchNr,[Node|HF]}|Rest], Acc);
    add_node(N, Node, [H|Rest], Acc) ->
        add_node(N-1, Node, Rest, [H|Acc]).


take(N, L) ->
    take(N, L, []).

    take(0, L, Acc) ->
        {lists:reverse(Acc), L};
    take(_N, [], Acc) ->
        {lists:reverse(Acc), []};
    take(N, [H|T], Acc) ->
        take(N-1, T, [H|Acc]).


