%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc Hash ring management for Zotonic

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

%% The hash ring of a zotonic system consists of nodes that are responsible for a part of the ring.
%% Keys are hashed to buckets and the buckets have a primary node that is responsible for storing
%% values hashed to the bucket.
%% 
%% When a node is added it will claim a range from the ring.  This range is calculated depending on 
%% the number of nodes in the ring.  All nodes have an equal weight.

%% Adding a new node:
%% 1. Ping random nodes in the network in the known nodes list till we get a pong
%% 2. When receiving a pong: fetch ring from any node in nodes()
%% 3. Add the current node to the fetched ring.
%% 4. Ping all ring-nodes a {sync, NewRing}

%% Removing a node:
%% 1. Ping all nodes() a {bye, node(), timestamp()}

%% Gossiping the ring state:
%% All nodes send randomly to another ring-node a {ring, node(), Ring} message.


-module(zynamo_ring).
-author("Marc  <marc@worrell.nl>").

-export([
    new/0,
    new/1,
    resume/1,
    hello/1,
    bye/1,
    resync/1,
    ring_buckets/0,
    is_equal/2,
    get_node/1,
    set_node_state/3,
    get_node_state/1,
    get_node_state/2,
    set_nodes_down/1,
    is_node_up/2,
    is_node_active/2,
    is_node_active/1,
    is_member/2,
    ping_nodes/1,
    nodes/1,
    get_local_service/3,
    set_local_service/5,
    delete_local_service/3,
    list_services/1,
    list_services/2,
    list_known_services/1,
    list_node_services/2,
    locate_service/3,
    sync/2,
    ranges/2,
    random_other_node/1,
    random_other_ring_nodes/1,
    empty_ring_services/1
]).

%% @doc The version of the ring data structure.
-define(ZYNAMO_VERSION, 1).

%% @doc Number of segments in our ring
-define(RING_BUCKETS, 1024).

%% @doc Default replication value
-define(DEFAULT_N, 3).

-type node_version() :: {integer(),integer(),integer()}.  % now() on the node

%% @doc A remembered bye message, needed to suppress 'hello' messages that arrive late
%%      or remove nodes from gossiped ring states for which we have a 'bye'
-record(ring_bye, {
                node    :: node(), 
                version :: node_version()
        }).

%% @doc A node in our ring.
-record(ring_node, {
                zynamo_version = ?ZYNAMO_VERSION :: pos_integer(),
                node       :: node(), 
                state      :: 'up'|'down', 
                membership :: 'ok'|'joining'|'joined'|'leaving'|'left', 
                version    :: node_version(),
                services   :: {{atom(), atom()}, pid()}
        }).

%% @doc The gossiped ring, with active nodes and received byes.
-record(ring, {
                zynamo_version = ?ZYNAMO_VERSION :: pos_integer(),
                nodes=[] :: [#ring_node{}], 
                byes=[]  :: [#ring_bye{}]
        }).

-type ring() :: #ring{}.
-export_type([ring/0]).

%% @doc Return a new ring, only containing the current node.
-spec new() -> ring().
new() ->
    new(node()).

-spec new(node()) -> ring().
new(Node) ->
    #ring{
        nodes=[ #ring_node{node=Node, state=up, membership=ok, version=now(), services=[]} ],
        byes=[]
    }.

%% @doc Resume a previously saved ring state, remove the local services.
-spec resume(ring()) -> ring().
resume(#ring{} = Ring) ->
    Me = node(),
    Nodes = [
        case Node#ring_node.node of
            Me -> Node#ring_node{services=[], version=now()};
            _ -> Node
        end
        || Node <- Ring#ring.nodes
    ],
    Ring#ring{nodes=Nodes}.


%% @doc Touch the ring, forcing a gossip
-spec resync(ring()) -> ring().
resync(#ring{} = Ring) ->
    Me = node(),
    Nodes = [
        Node#ring_node{
            version=case Node#ring_node.node of
                        Me -> now();
                        _  -> {0,0,0}
                    end
        }
        || Node <- Ring#ring.nodes
    ],
    Ring#ring{nodes=Nodes}.


%% @doc Return the number of buckets in a ring
-spec ring_buckets() -> pos_integer().
ring_buckets() ->
    ?RING_BUCKETS.


%% @doc Check if two rings are equal, including their version numbers.
-spec is_equal(ring(), ring()) -> boolean().
is_equal(A, B) ->
    A#ring.nodes == B#ring.nodes andalso A#ring.byes == B#ring.byes.


%% @doc Recalculate all ranges in a ring, based on an even distribution.
-spec ranges('past'|'future', ring()) -> [ {integer(),integer(),node()} ].
ranges(When, #ring{nodes=Nodes}) ->
    Nodes1 = lists:filter(fun(N) -> is_when(When, N#ring_node.membership) end, Nodes),
    Size = ?RING_BUCKETS / length(Nodes1),
    ranges(Nodes1, 1, Size, []).

    ranges([], _N, _Size, Acc) ->
        lists:reverse(Acc);
    ranges([R|T], N, Size, Acc) ->
        Range = {round((N-1)*Size), round(N*Size)-1, R#ring_node.node},
        ranges(T, N+1, Size, [Range|Acc]).

    % is_when separates the ring in a future and past ring, depending on the status of the nodes.
    is_when(_, ok) -> true;
    is_when(past, leaving) -> true;
    is_when(past, left) -> true;
    is_when(future, joining) -> true;
    is_when(future, joined) -> true;
    is_when(_, _) -> false.


%% @doc Return the node() of a ring node.
get_node(#ring_node{node=Node}) ->
    Node.

%% @doc Register a down node in our ring state
set_node_state(Node, NewState, #ring{nodes=Nodes} = Ring) ->
    case lists:keyfind(Node, #ring_node.node, Nodes) of
        #ring_node{state=NewState} ->
            Ring;
        #ring_node{} = RN ->
            Ring#ring{nodes=lists:keyreplace(Node, #ring_node.node, Nodes, RN#ring_node{state=NewState})};
        false ->
            Ring
    end.

%% @doc Get the up/down state of a node in the ring
get_node_state(#ring_node{state=State}) ->
    State.

%% @doc Check if a node is registered as up or down
get_node_state(#ring_node{node=Node}, Ring) ->
    get_node_state(Node, Ring);
get_node_state(Node, #ring{nodes=Nodes}) ->
    case lists:keyfind(Node, #ring_node.node, Nodes) of
        #ring_node{state=State} -> State;
        false -> undefined
    end.

%% @doc Set all nodes, except the local node, to 'down'. Forget about all services.
%%      Used on initialization.
set_nodes_down(#ring{nodes=Nodes} = Ring) ->
    Me = node(),
    Nodes1 = [
        N#ring_node{
            state=case N#ring_node.node of Me -> up; _ -> down end,
            services=[]
        } 
        || N <- Nodes
    ],
    Ring#ring{nodes=Nodes1}.


%% @doc Check if a node is up
is_node_up(#ring_node{node=Node}, Ring) ->
    is_node_up(Node, Ring);
is_node_up(Node, Ring) ->
    get_node_state(Node, Ring) =:= 'up'.

%% @doc Check if a node is up and available for requests
is_node_active(#ring_node{node=Node}, Ring) ->
    is_node_active(Node, Ring);
is_node_active(Node, #ring{nodes=Nodes}) ->
    case lists:keyfind(Node, #ring_node.node, Nodes) of
        #ring_node{} = RingNode -> is_node_active(RingNode);
        false -> false
    end.

is_node_active(#ring_node{state='up', membership='ok'}) -> true;
is_node_active(#ring_node{state='up', membership='joined'}) -> true;
is_node_active(#ring_node{state='up', membership='joining'}) -> true;
is_node_active(_) -> false.
    

%% Check if a node is a member of our ring
is_member(Node, #ring{nodes=Nodes}) ->
    case lists:keyfind(Node, #ring_node.node, Nodes) of
        #ring_node{} -> true;
        false -> false
    end.

%% @doc Ping all nodes in the ring, used when restarting a node (see zynamo_manager)
ping_nodes(Ring) -> 
    [ net_adm:ping(N) || #ring_node{node=N} <- Ring#ring.nodes ].

%% @doc Return a list of all nodes in the ring (down and up)
-spec nodes(ring()) -> [ node() ].
nodes(Ring) ->
    [ N || #ring_node{node=N} <- Ring#ring.nodes ].


%% @doc A hello for the current node, rejoining the ring
hello(Ring) ->
    hello(#ring_node{node=node(), version=now(), services=[]}, Ring).
    
%% @doc Process a received {hello, node(), Version} message from another node.
-spec hello(#ring_node{}, ring()) -> {'nochange'|'changed', #ring{}}.
hello(#ring_node{node=Node, version=Version} = RingNode, #ring{byes=Byes} = Ring) ->
    case lists:keyfind(Node, #ring_bye.node, Byes) of
        #ring_bye{version=ByeVersion} ->
            case ByeVersion > Version of
                true ->
                    {nochange, Ring};
                false ->
                    add(RingNode, Ring#ring{byes=lists:keydelete(Node, #ring_bye.node, Byes)})
            end;
        false ->
            add(RingNode, Ring)
    end.

    %% @doc Add a node.
    add(#ring_node{node=Node, version=Version, membership=Membership, services=Services}, #ring{nodes=Nodes} = Ring) ->
        case lists:keyfind(Node, #ring_node.node, Nodes) of
            #ring_node{version=RingVersion} = RN->
                case RingVersion >= Version of
                    true -> 
                        {nochange, Ring};
                    false ->
                        {changed, Ring#ring{nodes=lists:keyreplace(
                                                        Node, #ring_node.node, Nodes, 
                                                        RN#ring_node{version=Version, membership=Membership, services=Services})}}
                end;
            false ->
                Nodes1 = lists:sort([#ring_node{node=Node, state=up, membership=Membership, version=Version, services=Services} | Nodes]),
                {changed, Ring#ring{nodes=Nodes1}}
        end.

%% @doc The current node leaves the ring
-spec bye(ring()) -> {'changed'|'nochange', #ring{}}.
bye(Ring) ->
    bye(node(), now(), Ring).

%% @doc Remove a node from the ring, register a bye with the given version
%%      Drop the bye if it is older than the latest recorded hello.
-spec bye(node(), node_version(), ring()) -> {'changed'|'nochange', #ring{}}.
bye(Node, Version, #ring{nodes=Nodes, byes=Byes} = Ring) ->
    case lists:keyfind(Node, #ring_bye.node, Byes) of
        #ring_bye{version=RingVersion} = Bye->
            case RingVersion >= Version of
                true ->
                    {nochange, Ring};
                false ->
                    {changed, Ring#ring{byes=lists:keyreplace(Node, #ring_bye.node, Byes, Bye#ring_bye{version=Version})}}
            end;
        false ->
            case lists:keyfind(Node, #ring_node.node, Nodes) of
                #ring_node{version=RingVersion} ->
                    case RingVersion >= Version of
                        true ->
                            {nochange, Ring};
                        false ->
                            {changed, 
                                Ring#ring{nodes=lists:keydelete(Node, #ring_node.node, Nodes), 
                                      byes=lists:sort([#ring_bye{node=Node, version=Version}|Byes])}}
                    end;
                false ->
                    {changed, Ring#ring{byes=lists:sort([#ring_bye{node=Node, version=Version}|Byes])}}
            end
    end.


%% @doc Find a service record for the current node
-spec get_local_service(atom(), atom(), ring()) -> false | {{atom(), atom()}, pid(), term()}.
get_local_service(Site, Service, Ring) ->
    case lists:keyfind(node(), #ring_node.node, Ring#ring.nodes) of
        false -> false;
        Node -> lists:keyfind({Site, Service}, 1, Node#ring_node.services)
    end.

%% @doc Add or update a service for the current node and its service gossip state
-spec set_local_service(atom(), atom(), pid(), term(), ring()) -> {ok, ring()} | {error, node_down}.
set_local_service(Site, Service, Pid, GossipState, Ring) ->
    case is_node_up(node(), Ring) of
        true ->
            Me = node(),
            Nodes1 = [
                case Node#ring_node.node of
                    Me ->
                        Services1 = lists:sort([
                                        {{Site, Service}, Pid, GossipState}
                                        | lists:keydelete({Site, Service}, 1, Node#ring_node.services)
                                    ]),
                        case Services1 == Node#ring_node.services of
                            true -> 
                                Node;
                            false ->
                                Node#ring_node{version=now(), services=Services1}
                        end;
                    _ ->
                        Node
                end
                || Node <- Ring#ring.nodes
            ],
            {ok, Ring#ring{nodes=Nodes1}};
        false ->
            {error, node_down}
    end.


%% @doc Delete a service from the current node
-spec delete_local_service(atom(), atom(), ring()) -> {ok, ring()} | {error, node_down}.
delete_local_service(Site, Service, Ring) ->
    case is_node_up(node(), Ring) of
        true ->
            Me = node(),
            Nodes1 = [
                case Node#ring_node.node of
                    Me ->
                        case lists:keyfind({Site, Service}, 1, Node#ring_node.services) of
                            false -> 
                                Node;
                            _ ->
                                Services1 = lists:keydelete({Site, Service}, 1, Node#ring_node.services),
                                Node#ring_node{version=now(), services=Services1}
                        end;
                    _ ->
                        Node
                end
                || Node <- Ring#ring.nodes
            ],
            {ok, Ring#ring{nodes=Nodes1}};
        false ->
            {error, node_down}
    end.


%% @doc Get a list of all known services on all nodes, up or down.
-spec list_known_services(ring()) -> [ {atom(), atom()} ].
list_known_services(Ring) ->
    AllServices = lists:flatten([[Service || {Service, _, _} <- Node#ring_node.services] || Node <- Ring#ring.nodes]),
    lists:sort(sets:to_list(sets:from_list(AllServices))).
                                   

%% @doc Get a list of all services for all sites available on active nodes.
-spec list_services(ring()) -> [ {atom(), atom()} ].
list_services(Ring) ->
    Services = [
        case is_node_active(Node) of
            true -> [ Service || {Service, _, _} <- Node#ring_node.services ];
            false -> []
        end
        || Node <- Ring#ring.nodes
    ],
    lists:usort(lists:flatten(Services)).

%% @doc Get a list of all services available on active nodes for a certain site or zotonic
-spec list_services(atom(), ring()) -> [ atom() ].
list_services(Site, Ring) ->
    Services = [
        case is_node_active(Node) of
            true -> [ Service || {{ServiceSite, Service}, _, _} <- Node#ring_node.services, ServiceSite =:= Site ];
            false -> []
        end
        || Node <- Ring#ring.nodes
    ],
    lists:usort(lists:flatten(Services)).

%% @doc Get a list of all services for all sites available on active nodes.
-spec list_node_services(atom(), ring()) -> [ {atom(), atom()} ].
list_node_services(Node, Ring) ->
    Services = [
                [ Service || {Service, _, _} <- N#ring_node.services ]
        || N <- Ring#ring.nodes, N#ring_node.node =:= Node
    ],
    lists:usort(lists:flatten(Services)).


%% @doc Return the list of processes running a service, keep in ring order. Only list active nodes.
-spec locate_service(atom(), atom(), ring()) -> [ {pid(), term()} ].
locate_service(Site, Service, Ring) ->
    lists:foldl(
        fun(Node, Acc) ->
            case is_node_active(Node) of
                true ->
                    case lists:keyfind({Site, Service}, 1, Node#ring_node.services) of
                        false -> 
                            Acc;
                        {{Site, Service}, Pid, GossipState} ->
                            [{Pid,GossipState}|Acc]
                    end;
                false ->
                    Acc
            end
        end,
        [],
        Ring#ring.nodes).


%% @doc Sync two views of the ring memberships. This does not merge the up-state of the nodes.
%%      This is a simple brute force merge which works well with low numbers of nodes.
-spec sync(ring(), ring()) -> {'changed'|'nochange', ring()}.
sync(Ring, Ring) ->
    {nochange, Ring};
sync(Other, Ring) ->
    {IsChanged, Ring1} = lists:foldl(fun(RingNode, {IsC, R}) ->
                                    {Change, R1} = hello(RingNode, R),
                                    {IsC or (Change =:= changed), R1}
                                end, 
                                {false, Ring},
                                Other#ring.nodes),
    case lists:foldl(fun(#ring_bye{node=Node, version=Version}, {IsC, R}) ->
                        {Change, R1} = bye(Node, Version, R),
                        {IsC or (Change =:= changed), R1}
                     end, 
                     {IsChanged, Ring1},
                     Other#ring.byes) of
        {true, R} -> {changed, R};
        {false, R} -> {nochange, R}
    end.


%% @doc Return a random node from the ring that is not the current node.
-spec random_other_node(ring()) -> 'no_node' | #ring_node{}.
random_other_node(#ring{nodes=Nodes} = Ring) ->
    Self = node(),
    case Nodes of
        [#ring_node{node=Self}] -> 
            no_node;
        _ ->
            #ring_node{node=Node} = lists:nth(random:uniform(length(Nodes)), Nodes),
            case Node of
                Self -> random_other_node(Ring);
                _Other -> Node
            end
    end.

-spec random_other_ring_nodes(ring()) -> [ #ring_node{} ].
random_other_ring_nodes(#ring{nodes=Nodes}) ->
    Nodes1 = lists:filter(fun(#ring_node{node=N}) -> N /= node() end, Nodes),
    zynamo_random:randomize(Nodes1).


%% @doc Return a ring with all services removed
empty_ring_services(Ring) ->
    Ring#ring{byes=[], nodes=[Node#ring_node{services=[]} || Node <- Ring#ring.nodes]}.

    
