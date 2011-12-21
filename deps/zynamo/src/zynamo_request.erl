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

    put/5,
    put/6,

    list/3,
    list/4
]).

-include("zynamo.hrl").

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


%% @doc List all key/values on the servers, tryes to deduplicate. 
%% Server selection is 'coverage', 'all', {node, [node()]} or {hash, [Key]}.
%% The receiver is a Pid which will be sent messages {list, Reference, data, {Key, Value}}.
%% The final message is {list, Reference, eoi, Stats}.
%% The error message is {list, Reference, error, Reason}.
list(Site, Service, Receiver) ->
    list(Site, Service, Receiver, []).

-spec list(atom(), atom(), {pid, pid(), reference()}, list()) -> ok | {error, Reason :: term()}.
list(Site, Service, Receiver, Options) ->
    % TODO
    {error, not_implemented}.




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
            {ok, _Pid} = zynamo_request_fsm_sup:start_fsm([self(), Command1, PreferenceNodes, ServiceNodes, Options]),
            wait_for_result(Ref, Options)
    end.
    
    bucket_nr(Key, []) -> zynamo_hash:hash(Key) rem zynamo_ring:ring_buckets();
    bucket_nr(_Key, [{bucket,Nr}|_]) -> Nr;
    bucket_nr(Key, [_|Rest]) -> bucket_nr(Key, Rest).


%% @doc Wait for a result from the FSM, timeout after a configured time.
wait_for_result(Ref, Options) ->
    Timeout = proplists:get_value(timeout, Options, ?ZYNAMO_REQUEST_TIMEOUT),
    receive 
        {result, Ref, Result, _Stats} -> Result
    after Timeout ->
        {error, timeout}
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
    merge_preference_nodes(Nodes, []).

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

