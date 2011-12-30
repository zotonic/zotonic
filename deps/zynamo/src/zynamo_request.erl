%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc zynamo request - handle commands sent to nodes in the ring

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

%% Basic functions for accessing services on the zynamo ring.
%% The routines work with the usual Dynamo quorum and eventual consistency.

-module(zynamo_request).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    get/3,
    get/4,

    list/2,
    list/4,
    
    put/5,
    put/6,

    delete/4,
    delete/5,
    
    command/3,
    command/4,
    command/5,

    reply/2,
    
    coverage/3
]).

-include("zynamo.hrl").

-type list_receiver() :: list() | {pid(), reference()} | function() | {atom(), atom()}.

%% @doc Read a value from the zynamo ring. Returns 'maybe' when the quorum has not been reached.
get(Site, Service, Key) ->
    get(Site, Service, Key, []).

-spec get(atom(), atom(), Key::term(), zynamo_request_options()) -> 
                                          {ok, Value :: term(), zynamo_request_stats()} 
                                        | {maybe, Value :: term(), zynamo_request_stats()} 
                                        | {multiple, [Value :: term()], zynamo_request_stats()} 
                                        | {error, Reason :: term()}.
get(Site, Service, Key, Options) ->
    do_command(Site, Service, Key, #zynamo_command{command=get, key=Key}, Options).




%% @doc List all key/values on the servers, tryes to deduplicate.
-spec list(atom(), atom()) -> {ok, list()} | {error, term()}.
list(Site, Service) ->
    list(Site, Service, [], []).

-spec list(atom(), atom(), list_receiver(), list( zynamo_request_option() | return_value | no_return_version) ) -> 
    {ok, list()} | {ok, term()} | {ok, {pid(), reference()}}| {error, term()}.
list(Site, Service, Receiver, Options) ->
    case proplists:get_all_values(node, Options) of
        [] ->
            % Calculate our own coverage
            N = proplists:get_value(n, Options, 1),
            Nodes = coverage(Site, Service, N);
        _Ns ->
            % Use the mentioned node(s)
            Buckets = get_buckets(Site, Service, undefined, Options),
            {ok, Ranges} = get_ring_range(get, Options),
            {ok, ServicePidData} = zynamo_manager:locate_service(Site, Service),
            ServiceNodes = [ {node(Pid), Pid} || {Pid,_Data} <- ServicePidData ],
            Nodes = collect_preference_nodes(Buckets, Ranges, ServiceNodes)
    end,
    do_list(Site, Service, Nodes, Receiver, Options).
    

%% @doc Read a value from the zynamo ring. Returns 'maybe' when the quorum has not been reached.
-spec put(atom(), atom(), term(), zynamo_data_version(), term()) -> 
                                          {ok, zynamo_request_stats()} 
                                        | {maybe, zynamo_request_stats()} 
                                        | {error, Reason :: term()}.
put(Site, Service, Key, Version, Value) ->
    put(Site, Service, Key, Version, Value, []).

-spec put(atom(), atom(), term(), zynamo_data_version(), term(), zynamo_request_options()) -> 
                                          {ok, zynamo_request_stats()} 
                                        | {maybe, zynamo_request_stats()} 
                                        | {error, Reason :: term()}.
put(Site, Service, Key, Version, Value, Options) ->
    do_command(Site, Service, Key, #zynamo_command{command=put, key=Key, version=Version, value=Value}, Options).


%% @doc Delete a value from the zynamo ring. Returns 'maybe' when the quorum has not been reached.
-spec delete(atom(), atom(), term(), zynamo_data_version()) -> 
                                          {ok, zynamo_request_stats()} 
                                        | {maybe, zynamo_request_stats()} 
                                        | {error, Reason :: term()}.
delete(Site, Service, Key, Version) ->
    delete(Site, Service, Key, Version, []).

-spec delete(atom(), atom(), term(), zynamo_data_version(), zynamo_request_options()) -> 
                                          {ok, zynamo_request_stats()} 
                                        | {maybe, zynamo_request_stats()} 
                                        | {error, Reason :: term()}.
delete(Site, Service, Key, Version, Options) ->
    do_command(Site, Service, Key, #zynamo_command{command=delete, key=Key, version=Version}, Options).


%% @doc Send a command to a site/service
command(Site, Service, #zynamo_command{} = Command) ->
    do_command(Site, Service, undefined, Command, []).

command(Site, Service, #zynamo_command{} = Command, Options) when is_list(Options) ->
    do_command(Site, Service, undefined, Command, Options);
command(Site, Service, Key, #zynamo_command{} = Command) ->
    do_command(Site, Service, Key, Command, []).

command(Site, Service, Key, #zynamo_command{} = Command, Options) ->
    do_command(Site, Service, Key, Command, Options).


%% @doc Helper function for sending reply back from a service.
reply(Reply, #zynamo_service_command{ref=Ref, from=From}) ->
    case is_pid(From) of
        true -> From ! {ok, node(), Ref, Reply};
        false -> nop
    end.
    



%% @doc Send a command to a quorum of servers.  Servers are selected based on the
%%      provided key and options.
do_command(Site, Service, Key, Command, Options) ->
    Buckets = get_buckets(Site, Service, Key, Options),
    {ok, Ranges} = get_ring_range(Command, Options),
    {ok, ServicePidData} = zynamo_manager:locate_service(Site, Service),
    ServiceNodes = [ {node(Pid), Pid} || {Pid,_Data} <- ServicePidData ],
    case collect_preference_nodes(Buckets, Ranges, ServiceNodes) of
        [] ->
            {error, no_nodes};
        PreferenceNodes ->
            Ref = erlang:make_ref(),
            Command1 = Command#zynamo_command{ref=Ref, bucket_nr=bucket_nr(Key, Buckets)},
            {ok, Pid} = zynamo_request_fsm_sup:start_fsm([self(), Command1, PreferenceNodes, ServiceNodes, Options]),
            wait_for_result(Pid, Ref)
    end.
    
    bucket_nr(Key, []) -> zynamo_hash:hash(Key) rem zynamo_ring:ring_buckets();
    bucket_nr(_Key, [{bucket,Nr}|_]) -> Nr;
    bucket_nr(Key, [_|Rest]) -> bucket_nr(Key, Rest).


%% @doc Wait for a result from the FSM. Monitor the FSM so that we can stop if it crashes.
%%      Trust the FSM for handling the timeout values.
wait_for_result(Pid, CommandRef) ->
    MRef = erlang:monitor(process, Pid),
    receive 
        {result, CommandRef, Result, _Stats} -> 
            erlang:demonitor(MRef, [flush]),
            Result;
        {'DOWN', MRef, process, _Pid, Reason} ->
            % Result might be a bit slower than the down message from a successful FSM
            receive
                {result, CommandRef, Result, _Stats} -> 
                    Result
            after 0 ->  
                {error, Reason}
            end
    end.


%% @doc Determine the starting bucket nr and if this is random for this operation.
get_buckets(Site, Service, Key, Options) ->
    case proplists:get_all_values(node, Options) of
        [] -> [ get_bucket1(Site, Service, Key, sha, Options) ];
        Nodes -> [ get_bucket1(Site, Service, Key, Node, Options) || Node <- Nodes ]
    end.

    get_bucket1(Site, Service, Key, Node, Options) ->
        case Node of
            random ->
                random;
            local_random ->
                local_random;
            [ N | _ ] = NodeList when is_atom(N) ->
                {node, NodeList};
            {key, K} ->
                get_bucket1(Site, Service, K, sha, Options);
            {key, K, M} ->
                get_bucket1(Site, Service, K, M, Options);
            Method ->
                HashValue = case Method of
                                sha -> zynamo_hash:hash(Key);
                                N when is_integer(N) -> N;
                                F when is_function(F) -> F(Site, Service, Key, Options);
                                {M,F} -> M:F(Site, Service, Key, Options)
                            end,
                {bucket, HashValue rem zynamo_ring:ring_buckets()}
        end.


%% @doc Fetch the ring for the command.
get_ring_range(Command, Options) ->
    Which = case proplists:get_value(ring, Options) of
        undefined ->
            case Command of
                #zynamo_command{command=put} -> future;
                _ -> past
            end;
        past -> past;
        future -> future
    end,
    zynamo_manager:get_ring_range(Which).


%% @doc Determine the preference list of nodes. Can be random, depending 
%% on the bucket nr or a preferred list of nodes from the Options.
collect_preference_nodes(Buckets, Ranges, ServiceNodes) ->
    Nodes = [
        collect_preference_nodes1(Bucket, Ranges, ServiceNodes)
        || Bucket <- Buckets
    ],
    unique_list(merge_preference_nodes(Nodes, [])).

    collect_preference_nodes1(local_random, _Ranges, ServiceNodes) ->
        Random = zynamo_random:randomize([ Node || {Node,_} <- ServiceNodes ]),
        {Local,Other} = lists:partition(fun(Node) -> node() =:= Node end, Random),
        Local ++ Other;
    collect_preference_nodes1(random, _Ranges, ServiceNodes) ->
        zynamo_random:randomize([ Node || {Node,_} <- ServiceNodes ]);
    collect_preference_nodes1({node, Nodes}, _Ranges, _ServiceNodes) ->
        Nodes;
    collect_preference_nodes1({bucket, Bucket}, Ranges, _ServiceNodes) ->
        {_, Nodes} = zynamo_hash:nodelist_bucket(Bucket, Ranges),
        Nodes.

    % Merge nodes, keep order
    merge_preference_nodes([], Acc) ->
        Acc;
    merge_preference_nodes([Nodes|Rest], Acc) ->
        Extra1 = lists:foldl(
            fun(Node, Extra) ->
                case lists:member(Node, Acc) of
                    true -> Extra;
                    false -> [Node|Extra]
                end
            end,
            [],
            Nodes),
        merge_preference_nodes(Rest, Acc ++ lists:reverse(Extra1)).


unique_list(L) ->
    unique_list(L, []).

unique_list([], Acc) ->
    lists:reverse(Acc);
unique_list([Member|Rest], Acc) ->
    case lists:member(Member, Acc) of
        true ->
            unique_list(Rest, Acc);
        false ->
            unique_list(Rest, [Member|Acc])
    end.
    


%% @doc Calculate the best coverage for a given N and the nodes running the service.
coverage(Site, Service, _N) ->
    % Lazy for now - take all nodes that run the service
    % Method:
    % - Make lists of nodes, with step N through the ring, starting at 1..N
    % - Check which list is covered by ServiceNodes
    % - Take most online, add nodes before/after for offline nodes (when alt node is not yet covered)
    % - Random select equal best fits to lower load when running queries
    {ok, ServicePidData} = zynamo_manager:locate_service(Site, Service),
    ServiceNodes = [ {node(Pid), Pid} || {Pid,_Data} <- ServicePidData ],
    [ Node || {Node,_Pid} <- ServiceNodes ].


do_list(Site, Service, Nodes, Receiver, Options) ->
    ListArgs = #zynamo_list_args{
        value=proplists:get_value(return_value, Options, false), 
        version=not proplists:get_value(no_return_version, Options, false),
        offset=0,
        limit=1000
    },
    ServiceArgs = {Site, Service, ListArgs},
    States = [ list_next({Node, [], 0}, ServiceArgs) || Node <- Nodes ],
    step_lists(States, Receiver, ServiceArgs).


    step_lists(States, Receiver, ServiceArgs) ->
        Min = find_lowest(States, undefined),
        Value = resolve_version([ {K,Vers,Val} || {{K,Vers,Val},_} <- States, K =:= Min ]),

        %% @TODO combine the do_command calls into a single parallel do_command call.
        States1 = [ step_lowest(State, Min, ServiceArgs) || State <- States ],

        case lists:any(fun({'$end_of_table',_}) -> false;
                          ({{error, _}, _}) -> false;
                          (_) -> true
                       end,
                       States1)
        of
            true -> step_lists(States1, list_receiver(Value, Receiver), ServiceArgs);
            false -> list_finalize(list_receiver(Value, Receiver))
        end.

    find_lowest([], Min) -> 
        Min;
    find_lowest([{'$end_of_table', _}|T], Min) ->
        find_lowest(T, Min);
    find_lowest([{{error, _}, _}|T], Min) ->
        find_lowest(T, Min);
    find_lowest([{{K,_Vers,_Val}, _}|T], undefined) ->
        find_lowest(T, K);
    find_lowest([{{K,_Vers,_Val}, _}|T], Min) when K < Min ->
        find_lowest(T, K);
    find_lowest([_|T], Min) ->
        find_lowest(T, Min).

    step_lowest({{K,_,_},S}, K, ServiceArgs) -> list_next(S, ServiceArgs);
    step_lowest(S, _K, _ServiceArgs) -> S.

    %% @doc A function that requests data from the given node. Sending them one by one to the caller.
    list_next({_Node, {error, Reason}, _Offset} = State, _ServiceArgs) ->
        {{error, Reason}, State};
    list_next({_Node, '$end_of_table', _Offset} = State, _ServiceArgs) ->
        {'$end_of_table', State};
    list_next({Node, [], Offset}, {Site, Service, ListArgs} = ServiceArgs) ->
        Command = #zynamo_command{
            command=list,
            value=ListArgs#zynamo_list_args{offset=Offset}
        },
        Options = [
            {node, [Node]},
            {n, 1},
            {quorum, 1},
            no_handoff
        ],
        case do_command(Site, Service, undefined, Command, Options) of
            {error, Reason} -> 
                {{error, Reason}, {Node, {error, Reason}, Offset}};
            [{_Node, _Handoff, {ok, []}}] ->
                list_next({Node, '$end_of_table', Offset}, ServiceArgs);
            [{_Node, _Handoff, {ok, List}}] ->
                list_next({Node, List, Offset}, ServiceArgs)
        end;
    list_next({Node, [KV|Rest], Offset}, _ServiceArgs) ->
        {KV, {Node, Rest, Offset+1}}.

    % TODO: use zynamo_version to determine which value is the most recent (or multiple if conflict)
    resolve_version(Vs) ->
        hd(Vs).

    list_receiver(V, Acc) when is_list(Acc) ->
        [V|Acc];
    list_receiver(V, {Pid, Ref}) when is_pid(Pid) ->
        Pid ! {Ref, V};
    list_receiver(V, {Pid, Ref}) when is_pid(Pid) ->
        Pid ! {Ref, V};
    list_receiver(V, F) when is_function(F) ->
        F(V);
    list_receiver(V, {M,F}) when is_atom(M), is_atom(F) ->
        M:F(V).

    list_finalize(Acc) when is_list(Acc) ->
        {ok, lists:reverse(Acc)};
    list_finalize({Pid, Ref}) when is_pid(Pid) ->
        Pid ! {Ref, '$end_of_table'},
        {ok, {Pid, Ref}};
    list_finalize(F) when is_function(F) ->
        {ok, F('$end_of_table')};
    list_finalize({M, F}) when is_atom(M), is_atom(F) ->
        {ok, M:F('$end_of_table')}.
