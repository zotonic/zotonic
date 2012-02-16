%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%%
%% @doc FSM for syncing data stores between nodes. 

%% Copyright 2012 Marc Worrell
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

-module(zynamo_sync_fsm).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([
    start_link/0
]).
-export([
    select_services/2,
    select_node/2,
    compare_hashes/2
]).

-include_lib("zynamo.hrl").


-record(state, {
    service :: {atom(), atom()},
    local_pid :: pid(),
    other_node :: node(),
    bucket_range :: {non_neg_integer(), non_neg_integer()},
    from_key :: any(),
    is_retry :: boolean(),
    services :: [ {atom(), atom()} ],
    history :: [ {{atom(), atom()}, node(), integer()} ],
    start_time :: integer()
}).


% Normal wait time between service/node polls
-define(SYNC_STEP_TIMEOUT, 10).
-define(SYNC_SHORT_TIMEOUT, 30000).  % 30 secs
-define(SYNC_LONG_TIMEOUT, 600000).  % 10 minutes

% Max timeout for the handoff command, the service might tarpit us, so we need a long timeout.
-define(SYNC_COMMAND_TIMEOUT, 300000).

% gen_server call timeouts, quite short to skip overloaded services
-define(CALL_TIMEOUT, 100).


%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link() ->
                        {ok, pid()}
                      | {error, term()}.
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).


%% ====================================================================
%% gen_fsm callbacks
%% ====================================================================

init(_Args) ->
    {A1,A2,A3} = erlang:now(),
    random:seed(A1, A2, A3),
    {ok, select_services, #state{
        history=[]
    }, sync_timeout(short)}.

% Select a service to sync.
select_services(timeout, #state{start_time=undefined} = State) ->
    {ok, Services} = zynamo_manager:list_node_services(node()),
    Next = lists:filter(fun({Site, Service}) -> is_sync_wanted(Site, Service, startup) end, Services),
    {next_state, select_node, State#state{services=zynamo_random:randomize(Next), start_time=now_secs()}, 0};
select_services(timeout, #state{history=His, start_time=StartTime} = State) ->
    {ok, Services} = zynamo_manager:list_node_services(node()),
    Now = now_secs(),
    Next = lists:filter(fun({Site, Service}) -> 
                            Ago = case lists:keyfind({Site,Service}, 1, His) of
                                        {_, _, LS} -> Now - LS;
                                        _ -> Now - StartTime
                                   end,
                            is_sync_wanted(Site, Service, Ago)
                        end,
                        Services),
    {next_state, select_node, State#state{services=zynamo_random:randomize(Next)}, 0}.

% Select a random node with the selected service running.
select_node(timeout, #state{services=[]} = State) ->
    {next_state, select_services, State, sync_timeout(long)};
select_node(timeout, #state{services=[{Site,Service}|RestServices]} = State) ->
    {ok, Where} = zynamo_manager:locate_service(Site, Service),
    case [ Pid || {Pid,_} <- Where ] of
        [] ->
            {next_state, select_node, State#state{services=RestServices}, 0};
        [Pid] when node(Pid) =:= node() ->
            {next_state, select_node, State#state{services=RestServices}, 0};
        Pids ->
            case zynamo_manager:get_service_pid(Site, Service, node()) of
                {ok, LocalPid} ->
                    {Candidates, BucketRange} = nodes_within_n(LocalPid, Pids),
                    case Candidates of
                        [] -> 
                            {next_state, select_node, State#state{services=RestServices}, 0};
                        _ ->
                            OtherNode = lists:nth(random:uniform(length(Candidates)), Candidates),
                            {next_state, compare_hashes, State#state{
                                    service={Site, Service},
                                    local_pid=LocalPid,
                                    other_node=OtherNode,
                                    bucket_range=BucketRange,
                                    from_key=undefined,
                                    is_retry=false,
                                    history=[ {{Site,Service}, OtherNode, now_secs()} | State#state.history ],
                                    services=RestServices}, 0}
                    end;
                {error, _} ->
                    {next_state, select_node, State#state{services=RestServices}, 0}
            end
    end.

% Start syncing content between this node and the random other node
compare_hashes(timeout, #state{from_key='$end_of_table'} = State) ->
    {next_state, select_node, State, sync_timeout(short)};
compare_hashes(timeout, #state{service={Site,Service}, other_node=OtherNode, bucket_range=BucketRange, from_key=FromKey} = State) ->
    % Request the first hash from the remote service
    % The keys are always checked in ascending order, the newer keys have a higher probability of being out of sync
    Command = #zynamo_command{
        command=list_hash,
        value=#zynamo_list_args{
                    return_version=true,
                    return_value=false,
                    return_gone=true,
                    bucket_range=BucketRange,
                    offset={key, FromKey},
                    limit=?ZYNAMO_SYNC_HASH_LIMIT
            }
    },
    Options = [
        {result, raw}, 
        {node, [node(), OtherNode]}, 
        {n, 2}, 
        {quorum, n},
        no_handoff,
        {timeout, ?SYNC_COMMAND_TIMEOUT}
    ],
    case zynamo_request:command(Site, Service, Command, Options) of
        #zynamo_result{status=ok, is_quorum=true, value=Result} ->
            {_Node, _, #zynamo_service_result{value=Local}} = lists:keyfind(node(), 1, Result),
            {OtherNode, _, #zynamo_service_result{value=Remote}} = lists:keyfind(OtherNode, 1, Result),
            case {Local, Remote} of
                {[], []} ->
                    % Reached end, continue with another service and node
                    {next_state, select_node, State, sync_timeout(short)};
                {Hs, Hs} ->
                    % Equal hashes, next offset
                    {LastKey, _Hs} = lists:last(Local),
                    {next_state, compare_hashes, State#state{from_key=LastKey}, sync_timeout(step)};
                _ ->
                    % Hashes not equal, check subrange comparisons to drill down
                    LastKey = diff_hashes(Local, Remote, FromKey),
                    case diff_block(LastKey, State) of
                        {ToRemote, ToLocal} ->
                            case {copy_values(ToRemote, node(), OtherNode, Site, Service),
                                  copy_values(ToLocal, OtherNode, node(), Site, Service)}
                            of
                                {ok,ok} ->
                                    % Continue at the offset where we found the first diff
                                    case State#state.is_retry of
                                        true ->
                                            NextKey = next_key(Local, Remote, LastKey),
                                            {next_state, compare_hashes, 
                                                State#state{from_key=NextKey, is_retry=false}, 
                                                sync_timeout(step)};
                                        false ->
                                            {next_state, compare_hashes, 
                                                State#state{from_key=LastKey, is_retry=(FromKey==LastKey)}, 
                                                sync_timeout(step)}
                                    end;
                                _ ->
                                    % Error: skip to next service
                                    {next_state, select_node, State, sync_timeout(short)}
                            end;
                        error ->
                            % Error: skip to next service
                            {next_state, select_node, State, sync_timeout(short)}
                    end
            end;
        _ ->
            % Error or quorum not reached, try another service on another node
            % Service might not support the list_hash command, preventing the sync from happening.
            {next_state, select_node, State, sync_timeout(step)}
    end.

%% @doc When the node comes up, then cut the polling interval short 
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

is_sync_wanted(Site, Service, SecsAgo) ->
    case zynamo_manager:get_service_pid(Site, Service, node()) of
        {ok, Pid} ->
            case catch gen_server:call(Pid, {is_sync_wanted, SecsAgo}, ?CALL_TIMEOUT) of
                {ok, IsWanted} -> IsWanted;
                _ -> false
            end;
        {error, _Reason} ->
            false
    end.
            

%% @doc Check what the typical 'N' value is of this service, based on that we select
%%      which nodes are possible candidates for the sync.
%%      We are only checking the bucket range of the current node in the future ring, offloading
%%      from the past to the future ring should be handled by different processes.
%% @todo Here we are assuming that under normal circumstances all nodes have the same services.
nodes_within_n(LocalPid, ServicePids) ->
    case catch gen_server:call(LocalPid, sync_n, ?CALL_TIMEOUT) of
        {ok, all} ->
            {[ node(Pid) || Pid <- ServicePids, Pid /= LocalPid ], {0,zynamo_ring:ring_buckets()-1}};
        {ok, N} when N =< 1 ->
            case zynamo_manager:get_ring_range(future,node()) of
                {ok, BucketRange} -> {[], BucketRange};
                _Error -> {[], {1,0}}
            end;
        {ok, N} ->
            case zynamo_manager:get_ring_range(future,node()) of
                {ok, BucketRange} ->
                    {ok, RingNodes} = zynamo_manager:nodes(),
                    FollowNodes = lists:dropwhile(fun(Node) -> Node /= node() end, RingNodes++RingNodes),
                    case [ Node || Node <- FollowNodes, Node /= node() ]  of
                        [] ->
                            {[], BucketRange};
                        Ns ->
                            {NodeCandidates,_} = lists:split(erlang:min(length(RingNodes)-1, N-1), Ns),
                            ServiceNodes = [ node(Pid) || Pid <- ServicePids ],
                            {lists:filter(fun(Node) -> lists:member(Node, ServiceNodes) end, NodeCandidates), BucketRange}
                    end;
                _Error ->
                    {[], {1,0}}
            end;
        _ ->
            {[], {1,0}}
    end.


%% @doc Find the next key to sync from
next_key(Local, Remote, K) ->
    min_next_key(next_key1(Local, K), next_key1(Remote, K)).
    
    min_next_key('$end_of_table', B) -> B;
    min_next_key(A, '$end_of_table') -> A;
    min_next_key(A, B) -> erlang:min(A, B).

    next_key1([], _K) -> '$end_of_table';
    next_key1([{N,_}|_], K) when K < N -> N;
    next_key1([_|Rest], K) -> next_key1(Rest, K).


%% @doc Find the key from where two hash sub blocks are differing. Return that key
%%      so that we can fetch all keys from there and diff the keys.
diff_hashes([X|Ls], [X|Rs], _LastKey) ->
    {LastKey,_Hs} = X,
    diff_hashes(Ls, Rs, LastKey);
diff_hashes(_, _, LastKey) ->
    LastKey.


diff_block(FromKey, #state{service={Site, Service}, other_node=OtherNode}) ->
    % Request the first hash from the remote service
    Command = #zynamo_command{
        command=list,
        value=#zynamo_list_args{
                    offset={key, FromKey}, 
                    limit=?ZYNAMO_SYNC_HASH_STEP,
                    return_gone=true,
                    return_value=false,
                    return_version=true
                }
    },
    Options = [
        {result, raw}, 
        {node, [node(), OtherNode]}, 
        {n, 2}, 
        {quorum, n},
        no_handoff,
        {timeout, ?SYNC_COMMAND_TIMEOUT}
    ],
    case zynamo_request:command(Site, Service, Command, Options) of
        #zynamo_result{status=ok, is_quorum=true, value=Result} ->
            {_Node, _,  #zynamo_service_result{value=Local}} = lists:keyfind(node(), 1, Result),
            {OtherNode, _, #zynamo_service_result{value=Remote}} = lists:keyfind(OtherNode, 1, Result),
            diff_keys(Local, Remote, [], []);
        _ ->
            error
    end.
    
    diff_keys([], [], ToRemote, ToLocal) ->
        {ToRemote, ToLocal};
    diff_keys([], 
              [#zynamo_service_result{key=R}|Rs], ToRemote, ToLocal) ->
        diff_keys([], Rs, ToRemote, [R|ToLocal]);
    diff_keys([#zynamo_service_result{key=L}|Ls], 
              [], ToRemote, ToLocal) ->
        diff_keys(Ls, [], [L|ToRemote], ToLocal);
    diff_keys([#zynamo_service_result{key=K,version=V}|Ls], 
              [#zynamo_service_result{key=K,version=V}|Rs], ToRemote, ToLocal) ->
        diff_keys(Ls, Rs, ToRemote, ToLocal);
    diff_keys([#zynamo_service_result{key=K,version=VL}|Ls], 
              [#zynamo_service_result{key=K,version=VR}|Rs], ToRemote, ToLocal) ->
        case zynamo_version:is_newer(VL, VR) of
            true -> diff_keys(Ls, Rs, [K|ToRemote], ToLocal);
            false -> diff_keys(Ls, Rs, ToRemote, [K|ToLocal])
        end;
    diff_keys([#zynamo_service_result{key=KL}|Ls], 
              [#zynamo_service_result{key=KR}|_] = Rs, ToRemote, ToLocal) when KL < KR ->
        diff_keys(Ls, Rs, [KL|ToRemote], ToLocal);
    diff_keys(Ls, [#zynamo_service_result{key=KR}|Rs], ToRemote, ToLocal) ->
        diff_keys(Ls, Rs, ToRemote, [KR|ToLocal]).



copy_values([], _FromNode, _ToNode, _Site, _Service) ->
    ok;
copy_values([K|Ks], FromNode, ToNode, Site, Service) ->
    case zynamo_request:get(Site, Service, K, 
                            [{node, [FromNode]}, {n,1}, {quorum,n}, no_handoff, {result, merge}]) 
    of
        #zynamo_result{status=ok, is_quorum=true, is_found=false} ->
            % Shouldn't happen, might be a database cleanup. Skip.
            copy_values(Ks, FromNode, ToNode, Site, Service);
        #zynamo_result{status=ok, is_quorum=true, version=Version, value=Value, is_gone=IsGone} ->
            Result = case IsGone of
                        true ->
                            zynamo_request:delete(Site, Service, K, Version, 
                                                    [{node, [ToNode]}, {n,1}, {quorum,n}, no_handoff, {result, merge}]);
                        false ->
                            zynamo_request:put(Site, Service, K, Version, Value, 
                                                    [{node, [ToNode]}, {n,1}, {quorum,n}, no_handoff, {result, merge}])
                     end,
            case Result of
                #zynamo_result{status=ok, is_quorum=true} ->
                    % All ok, next key
                    copy_values(Ks, FromNode, ToNode, Site, Service);
                #zynamo_result{status=conflict} ->
                    % Might be updated while we are looping over the values, reverse the sync.
                    case zynamo_request:get(Site, Service, K, 
                                            [{node, [ToNode]}, {n,1}, {quorum,n}, no_handoff, {result, merge}])
                    of
                        #zynamo_result{status=ok, is_quorum=true, is_found=true} ->
                            ignore;
                        #zynamo_result{status=ok, is_quorum=true, is_gone=true, version=OtherVersion} ->
                            zynamo_request:delete(Site, Service, K, OtherVersion, 
                                                  [{node, [FromNode]}, {n,1}, {quorum,n}, no_handoff, {result, merge}]);
                        #zynamo_result{status=ok, is_quorum=true, version=OtherVersion, value=OtherValue} ->
                            % Just put the new version/value, ignore the result.
                            zynamo_request:put(Site, Service, K, OtherVersion, OtherValue, 
                                               [{node, [FromNode]}, {n,1}, {quorum,n}, no_handoff, {result, merge}]);
                        _ ->
                            ignore
                    end,
                    copy_values(Ks, FromNode, ToNode, Site, Service);
                _Other ->
                    % Something more serious, stop the sync altogether.
                    error
            end;
        _Other ->
            % Key might be updated or deleted, skip to next
            copy_values(Ks, FromNode, ToNode, Site, Service)
    end.



sync_timeout(step) ->
    ?SYNC_STEP_TIMEOUT + zynamo_random:uniform(?SYNC_STEP_TIMEOUT);
sync_timeout(short) ->
    ?SYNC_SHORT_TIMEOUT + zynamo_random:uniform(?SYNC_SHORT_TIMEOUT);
sync_timeout(long) ->
    ?SYNC_LONG_TIMEOUT + zynamo_random:uniform(?SYNC_LONG_TIMEOUT).


now_secs() ->
    {A,B,_} = now(),
    A * 1000000 + B.

