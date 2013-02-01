%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%%
%% @doc Ring manager, records our current view of the ring.

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

-module(zynamo_manager).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
    nodes/0,
    get_ring/0,
    get_ring_range/1,
    get_ring_range/2,
    sync_ring/2,
    save/0,
    leave/0,
    join/0,
    resync/0,
    nodeup/1,
    nodedown/1,
    is_node_active/1,
    set_service/4,
    list_services/0,
    list_services/1,
    list_node_services/1,
    locate_service/2,
    get_service_pid/3
]).

-type ring() :: zynamo_ring:ring().


%% @doc The ring state. The complete ring and the buckets per node.
%%      The 'past' bucket list is the state without joining nodes
%%      The 'future' bucket list is the state without leaving nodes
-record(state, {
            ring :: ring(),
            service_monitors = [] :: [{{atom(), atom()}, reference(), pid(), term()}],
            past = [] :: [{integer(), integer(), node()}],
            future = [] :: [{integer(), integer(), node()}]
    }).


%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() -> 
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%% @doc List all nodes in the current ring
-spec nodes() -> {ok, [node()]} | {error, term()}.
nodes() ->
    gen_server:call(?MODULE, nodes, infinity).

%% @doc Fetch the current ring
-spec get_ring() -> {ok, ring()} | {error, term()}.
get_ring() ->
    gen_server:call(?MODULE, get_ring, infinity).

%% @doc Sync the ring with a ring from another node
%% @type sync_ring(node(), ring()) -> no_change | {ok, ring()}
-spec sync_ring(node(), ring()) -> {ok, ring()} | no_change | {error, term()}.
sync_ring(FromNode, Ring) ->
    gen_server:call(?MODULE, {do_sync_ring, FromNode, Ring}, infinity).


%% @doc Fetch a ring and its ranges
-spec get_ring_range(past | future) -> {ok, list( {non_neg_integer(), non_neg_integer(), node()} )} | {error, term()}.
get_ring_range(Which) ->
    gen_server:call(?MODULE, {get_ring_range, Which}, infinity).


-spec get_ring_range(past | future, node()) -> {ok, {non_neg_integer(), non_neg_integer()}} | {error, term()}.
get_ring_range(Which, Node) ->
    case get_ring_range(Which) of
        {ok, Ranges} ->
            case lists:keyfind(Node, 3, Ranges) of
                {L,H,_Node} -> {ok, {L, H}};
                false -> {error, not_found}
            end;
        Error ->
            Error
    end.

%% @doc Save the ring to disk, which will be used for a later reboot.
-spec save() -> ok.
save() ->
    gen_server:cast(?MODULE, save).

%% @doc Leave the ring
-spec leave() -> ok.
leave() ->
    gen_server:cast(?MODULE, leave).

%% @doc Join the ring
-spec join() -> ok.
join() ->
    gen_server:cast(?MODULE, join).

%% @doc Resync our ring state by forcing a gossip
-spec resync() -> ok.
resync() ->
    gen_server:cast(?MODULE, resync).

%% @doc Set the node's state to 'up'
-spec nodeup(node()) -> ok.
nodeup(Node) ->
    gen_server:cast(?MODULE, {nodeup, Node}).

%% @doc Set the node's state to 'down'
-spec nodedown(node()) -> ok.
nodedown(Node) ->
    gen_server:cast(?MODULE, {nodedown, Node}).

%% @doc Check if the node is marked as 'up' and available
-spec is_node_active(node()) -> {ok, boolean()} | {error, term()}.
is_node_active(Node) ->
    gen_server:call(?MODULE, {is_node_active, Node}, infinity).

%% @doc Add a service, track its availability
-spec set_service(atom(), atom(), pid(), term()) -> ok.
set_service(Site, Service, Pid, GossipState) ->
    gen_server:cast(?MODULE, {set_service, Site, Service, Pid, GossipState}).

%% @doc Return the list of available services on the ring
-spec list_services() -> {ok, [{atom(), atom()}]} | {error, term()}.
list_services() ->
    gen_server:call(?MODULE, list_services, infinity).

%% @doc Return the list of available services for a site on the ring
-spec list_services(atom()) -> {ok, [atom()]} | {error, term()}.
list_services(Site) ->
    gen_server:call(?MODULE, {list_services, Site}, infinity).

%% @doc Return the list of available services on a node
-spec list_node_services(node()) -> {ok, [{atom(),atom()}]} | {error, term()}.
list_node_services(Node) ->
    gen_server:call(?MODULE, {list_node_services, Node}, infinity).

%% @doc Return a list of (random) available service Pids, local Pid first.
-spec locate_service(atom(), atom()) -> {ok, [ {pid(), term()} ]} | {error, term()}.
locate_service(Site, Service) ->
    gen_server:call(?MODULE, {locate_service, Site, Service}, infinity).

%% @doc Return the pid of a service on a specific node
-spec get_service_pid(atom(), atom(), node()) -> {ok, pid()} | {error, term()}.
get_service_pid(Site, Service, Node) ->
    {ok, Pids} = zynamo_manager:locate_service(Site, Service),
    case [ Pid || {Pid,_} <- Pids, node(Pid) =:= Node ] of
        [Pid] -> {ok, Pid};
        [] -> {error, service_not_running}
    end.



%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
-spec init(list()) -> {ok, #state{}} | {ok, #state{}, integer()} | ignore | {stop, term()}.
init(_Args) ->
    % process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    % Try to start with the last known ring state
    Ring = case do_read() of
               {ok, R} -> zynamo_ring:resume(zynamo_ring:set_nodes_down(R));
               {error, _Reason} -> zynamo_ring:new()
           end,
    zynamo_event:up(),
    service_events(zynamo_ring:new(), Ring),
    {ok, new_ring(#state{}, Ring)}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
handle_call({do_sync_ring, FromNode, OtherRing}, _From, #state{ring=MyRing} = State) ->
    {Change, NewRing} = zynamo_ring:sync(OtherRing, MyRing),
    do_sync_node_state(FromNode, OtherRing, NewRing),
    case Change of
        changed ->
            save(),
            zynamo_event:changed(),
            node_events(MyRing, NewRing),
            service_events(MyRing, NewRing);
        nochange ->
            nop
    end,
    State1 = new_ring(State, NewRing),
    case zynamo_ring:is_equal(OtherRing, NewRing) of
        false -> {reply, {ok, NewRing}, State1};
        true -> {reply, ok, State1}
    end;

handle_call(nodes, _From, #state{ring=Ring} = State) ->
    {reply, {ok, zynamo_ring:nodes(Ring)}, State};

handle_call(get_ring, _From, #state{ring=Ring} = State) ->
    {reply, {ok, Ring}, State};

handle_call({get_ring_range, past}, _From, #state{past=Past} = State) ->
    {reply, {ok, Past}, State};

handle_call({get_ring_range, future}, _From, #state{past=Future} = State) ->
    {reply, {ok, Future}, State};

handle_call({is_node_active, Node}, _From, #state{ring=Ring} = State) ->
    {reply, {ok, zynamo_ring:is_node_active(Node, Ring)}, State};

handle_call(list_services, _From, #state{ring=Ring} = State) ->
    {reply, {ok, zynamo_ring:list_services(Ring)}, State};

handle_call({list_services, Site}, _From, #state{ring=Ring} = State) ->
    {reply, {ok, zynamo_ring:list_services(Site, Ring)}, State};

handle_call({list_node_services, Node}, _From, #state{ring=Ring} = State) ->
    {reply, {ok, zynamo_ring:list_node_services(Node, Ring)}, State};

handle_call({locate_service, Site, Service}, _From, #state{ring=Ring} = State) ->
    {reply, {ok, zynamo_ring:locate_service(Site, Service, Ring)}, State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
handle_cast(save, #state{ring=Ring} = State) ->
    do_save(Ring),
    {noreply, State};

handle_cast(resync, #state{ring=Ring} = State) ->
    zynamo_gossip:push_ring(),
    {noreply, zynamo_ring:resync(Ring), State};

%% @doc Leave the ring.
handle_cast(leave, #state{ring=Ring} = State) ->
    case zynamo_ring:is_member(node(), Ring) of
        true ->
            {_Changed, NewRing} = zynamo_ring:bye(Ring),
            {noreply, ring_changed(Ring, NewRing, State)};
        false ->
            {noreply, State}
    end;

%% @doc Take part in the ring again, overruling any earlier 'bye'
handle_cast(join, #state{ring=Ring, service_monitors=Monitors} = State) ->
    case zynamo_ring:is_member(node(), Ring) of
        false ->
            {Changed, NewRing} = zynamo_ring:hello(Ring),
            % Re-publish all local services
            NewRing1 = case Changed of
                        changed ->
                            lists:foldl(fun({{Site, Service}, _MRef, Pid, GossipState}, R) ->
                                                case zynamo_ring:set_local_service(Site, Service, Pid, GossipState, R) of
                                                    {ok, NewRing2} -> NewRing2;
                                                    {error, _} -> R
                                                end
                                        end,
                                        NewRing,
                                        Monitors);
                        nochange ->
                            NewRing
                       end,
            % Signal our ring change
            {noreply, ring_changed(Ring, NewRing1, State)};
        true ->
            {noreply, State}
    end;

%% @doc Set the state of the node to 'up', typically done when we received
%%      gossip from the node.
handle_cast({nodeup, Node}, State) ->
    {noreply, do_nodeup(Node, State)};

%% @doc Set the state of the node to 'down'.
handle_cast({nodedown, Node}, State) ->
    {noreply, do_nodedown(Node, State)};

%% @doc Add a service to the services list of this node.
handle_cast({set_service, Site, Service, Pid, GossipState}, State) ->
    {noreply, do_set_service(Site, Service, Pid, GossipState, State)};


%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handle node up events, set the state of the node to 'up', 
%%      send the node our ring state.
handle_info({nodeup, Node}, State) ->
    {noreply, do_nodeup(Node, State)};

%% @doc Handle node down events, set the state of the node to 'down'
handle_info({nodedown, Node}, State) ->
    {noreply, do_nodedown(Node, State)};

%% @doc Handle a 'down' from a service
%% @todo Log the service down event
handle_info({'DOWN', MRef, process, Pid, _Reason}, #state{service_monitors=Monitors} = State) ->
    case lists:keyfind(MRef, 2, Monitors) of
        {{Site, Service, _GossipState}, MRef, Pid, _GossipState} ->
            % Delete the service, gossip the deletion
            {noreply, do_set_service(Site, Service, undefined, undefined, State)};
        {{_Site, _Service}, MRef, _OtherPid, _GossipState} ->
            % Monitor on a service that has been deleted, dispose the monitor and ignore
            Monitors1 = lists:keydelete(MRef, 2, Monitors),
            {noreply, State#state{service_monitors=Monitors1}};
        _ ->
            {noreply, State}
    end;
    
%% @doc Handling all non call/cast messages
handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc Tell all ring members that we are going down.
terminate(_Reason, #state{ring=Ring}) ->
    [ gen_server:cast({?MODULE, Node}, {nodedown, node()}) || Node <- zynamo_ring:nodes(Ring) ],
    zynamo_event:down(),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

-spec do_save(ring()) -> ok.
do_save(Ring) ->
    {ok, Dev} = file:open(ring_file(), [write]),
    ok = file:write(Dev, <<
            "% THIS FILE IS AUTOMATICALLY GENERATED, DO NOT CHANGE WHILE RUNNING.",10,
            "%", 10,
            "% This file contains the latest ring state.",10,
            "% Delete this file to reset the ring to its defaults (when not running).",10,
            10>>),
    Data = io_lib:format("~p.~n", [zynamo_ring:empty_ring_services(Ring)]),
    ok = file:write(Dev, iolist_to_binary(Data)),
    ok = file:close(Dev).


-spec do_read() -> {ok, ring()} | {error, term()}.
do_read() ->
    case catch file:consult(ring_file()) of
        {ok, []} ->
            error_logger:info_msg("Ring state file is empty."),
            {error, empty};
        {ok, Rings} when is_list(Rings) ->
            {ok, hd(Rings)};
        {ok, _Content} ->
            error_logger:info_msg("Ring state file doesn't contain a list."),
            {error, illegal};
        {error, Reason} = Error ->
            error_logger:info_msg("Error reading ring state:~n~n~p~n", [Reason]),
            Error
    end.


do_nodeup(Node, #state{ring=Ring} = State) ->
    case zynamo_ring:get_node_state(Node, Ring) of
        down ->
            Ring1 = zynamo_ring:set_node_state(Node, up, Ring),
            zynamo_event:nodeup(Node),
            zynamo_gossip:gossip_ring(Node),
            service_events(Ring, Ring1),
            State#state{ring=Ring1};
        undefined ->
            zynamo_gossip:gossip_ring(Node),
            State;
        up ->
            State
    end.


do_nodedown(Node, #state{ring=Ring} = State) ->
    case zynamo_ring:get_node_state(Node, Ring) of
        up ->
            Ring1 = zynamo_ring:set_node_state(Node, down, Ring),
            zynamo_event:nodedown(Node),
            service_events(Ring, Ring1),
            State#state{ring=Ring1};
        undefined ->
            State;
        down ->
            State
    end.


do_sync_node_state(FromNode, OtherRing, MyRing) ->
    case zynamo_ring:get_node_state(FromNode, MyRing) of
        up -> nop;
        down -> nodeup(FromNode);
        undefined -> nop
    end,
    Me = node(),
    [ sync_node_state(N, OtherRing, MyRing) || N <- zynamo_ring:nodes(MyRing), N /= FromNode, N /= Me ].

    sync_node_state(Node, OtherRing, MyRing) ->
        S = zynamo_ring:get_node_state(Node, OtherRing),
        case zynamo_ring:get_node_state(Node, MyRing) of
            S -> 
                nop;
            up ->
                % Other node claims this node is down
                case net_adm:ping(Node) of
                    pong -> nop;
                    pang -> nodedown(Node)
                end;
            down ->
                % Other node claims this node is up
                case net_adm:ping(Node) of
                    pong -> zynamo_gossip:invite_gossip(Node);
                    pang -> nop
                end;
            undefined -> 
                nop
        end.


do_set_service(Site, Service, Pid, GossipState, #state{ring=Ring, service_monitors=Monitors} = State) ->
    case zynamo_ring:get_local_service(Site, Service, Ring) of
        false -> 
            case Pid of
                undefined -> State;
                _ -> update_service(Site, Service, Pid, GossipState, State)
            end;
        {{Site,Service}, Pid, GossipState} ->
            State;
        {{Site,Service}, Pid, _OldGossipState} ->
            NewRing = case zynamo_ring:set_local_service(Site, Service, Pid, GossipState, Ring) of
                          {ok, R} -> R;
                          {error, _} -> Ring
                      end,
            State#state{ring=NewRing};
        {{Site,Service}, OtherPid, _OldGossipState} ->
            Monitors1 = case lists:keyfind(OtherPid, 3, Monitors) of
                            {{Site, Service}, OtherMRef, OtherPid, _GossipState} ->
                                erlang:demonitor(OtherMRef),
                                lists:keydelete(OtherPid, 3, Monitors);
                            _NotMonitored ->
                                Monitors
                        end,
            update_service(Site, Service, Pid, GossipState, State#state{service_monitors=Monitors1})
    end.

    % Service is changed, force gossip to prevent referrals to old service
    update_service(Site, Service, undefined, _GossipState, #state{ring=Ring, service_monitors=Monitors} = State) ->
        Monitors1 = case lists:keyfind({Site, Service}, 1, Monitors) of
                        {{Site, Service}, OtherMRef, _OtherPid, _GossipState} ->
                            erlang:demonitor(OtherMRef),
                            lists:keydelete({Site, Service}, 1, Monitors);
                        false ->
                            Monitors
                    end,
        {ok, NewRing} = zynamo_ring:delete_local_service(Site, Service, Ring),
        zynamo_gossip:push_ring(),
        service_events(Ring, NewRing),
        State#state{ring=NewRing, service_monitors=Monitors1};
    update_service(Site, Service, Pid, GossipState, #state{ring=Ring, service_monitors=Monitors} = State) ->
        MRef = erlang:monitor(process, Pid),
    NewRing = case zynamo_ring:set_local_service(Site, Service, Pid, GossipState, Ring) of
                  {ok, Ring1} ->
                      zynamo_gossip:push_ring(),
                      service_events(Ring, Ring1),
                      Ring1;
                  {error, node_down} ->
                      Ring
              end,
    State#state{ring=NewRing, service_monitors=[ {{Site,Service}, MRef, Pid, GossipState} | Monitors ]}.


%% @doc Ring is changed, save the ring state, notify event listeners, gossip the new ring.
ring_changed(OldRing, NewRing, State) ->
    do_save(NewRing),
    zynamo_event:changed(),
    zynamo_gossip:push_ring(),
    node_events(OldRing, NewRing),
    service_events(OldRing, NewRing),
    new_ring(State, NewRing).

%% @doc Generate nodeup/nodedown events for the change in nodes.
node_events(OldRing, NewRing) ->
    AllNodes = lists:usort(zynamo_ring:nodes(OldRing) ++ zynamo_ring:nodes(NewRing)),
    [
        case {zynamo_ring:is_node_up(N, OldRing), zynamo_ring:is_node_up(N, NewRing)} of
            {false, true} -> zynamo_event:nodeup(N);
            {true, false} -> zynamo_event:nodedown(N);
            _ -> no_change
        end
        || N <- AllNodes
    ].
        
%% @doc Generate service up/down events for the services on the ring
service_events(OldRing, NewRing) ->
    NewServices = zynamo_ring:list_services(NewRing),
    case zynamo_ring:list_services(OldRing) of
        NewServices -> 
            ok;
        OldServices ->
            zynamo_event:servicechanged(NewServices),
            [ zynamo_event:servicedown(Site, Service) || {Site,Service} <- OldServices -- NewServices ],
            [ zynamo_event:serviceup(Site, Service) || {Site,Service} <- NewServices -- OldServices ],
            ok
    end.

%% @doc Set the new ring, recalc the past/future hash ring
new_ring(State, NewRing) ->
    State#state{
            ring=NewRing,
            past=zynamo_ring:ranges(past, NewRing),
            future=zynamo_ring:ranges(future, NewRing)
    }.


ring_file() ->
    case application:get_env(zynamo, ring_state_file) of
        {ok, Dir} -> Dir;
        undefined -> filename:join([code:lib_dir(zynamo, priv), "ring-"++atom_to_list(node())])
    end.


