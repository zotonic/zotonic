%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc Gossip the ring state.

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

%% Sends the ring state at random intervals to a random node in the ring.

-module(zynamo_gossip).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
    gossip_ring/0,
    gossip_ring/1,
    invite_gossip/0,
    invite_gossip/1,
    push_ring/0
]).

%% @doc The maximum interval (msec) between gossips
-define(GOSSIP_INTERVAL, 2000).


%%====================================================================
%% API
%%====================================================================

%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() -> 
    start_link([]).
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%% @doc Send the ring state to a random node
gossip_ring() ->
    gen_server:cast(?MODULE, gossip_ring).

%% @doc Send the ring state to a specific node
gossip_ring(Node) ->
    case node() of
        Node -> ok;
        _ -> gen_server:cast(?MODULE, {gossip_ring, Node})
    end.

%% @doc Push the ring state to all nodes
push_ring() ->
    gen_server:cast(?MODULE, push_ring).


%% @doc Try to reattach to the ring and gossip with all nodes 
%%      about our ring.
invite_gossip() ->
    spawn(fun() ->
        {ok, Ring} = zynamo_manager:get_ring(),
        zynamo_ring:ping_nodes(Ring),
        timer:sleep(2000),
        [ invite_gossip(Node) || Node <- nodes() ]
    end).

%% doc Invite other node to gossip to us
invite_gossip(Node) ->
    case node() of
        Node -> ok;
        _ -> gen_server:cast({?MODULE, Node}, {gossip_ring, node()})
    end.




%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(_Args) ->
    schedule_next_gossip(),
    invite_gossip(),
    {ok, []}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Send the ring state to a random other node.
handle_cast(gossip_ring, State) ->
    {ok, Ring} = zynamo_manager:get_ring(),
    do_gossip_ring(Ring),
    {noreply, State};

%% @doc Send our ring contents to a specific node.
handle_cast({gossip_ring, Node}, State) ->
    {ok, Ring} = zynamo_manager:get_ring(),
    gen_server:cast({?MODULE, Node}, {sync_ring, node(), true, Ring}),
    {noreply, State};

%% @doc At random intervals, send the ring state to a random node.
handle_cast(scheduled_gossip_ring, State) ->
    schedule_next_gossip(),
    {ok, Ring} = zynamo_manager:get_ring(),
    do_gossip_ring(Ring),
    {noreply, State};

%% @doc Receive a new ring state and sync our ring state with the
%%      received ring.  When our ring is changed then send it back to
%%      the sender so that they will agree on the ring state sooner.
handle_cast({sync_ring, FromNode, DoReply, Ring}, State) ->
    case zynamo_manager:sync_ring(FromNode, Ring) of
        ok -> 
            nop;
        {ok, NewRing} ->
            case DoReply of
                false -> nop;
                true -> gen_server:cast({?MODULE, FromNode}, {sync_ring, node(), false, NewRing})
            end
    end,
    {noreply, State};

%% @doc Push our state of the ring to all servers, used when joining or leaving the ring.
handle_cast(push_ring, State) ->
    {ok, Ring} = zynamo_manager:get_ring(),
    [ gen_server:cast({?MODULE, Node}, {sync_ring, node(), true, Ring}) || Node <- nodes() ],
    {noreply, State};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc At random intervals we sync our ring with a random node on the ring.
schedule_next_gossip() ->
    timer:apply_after(random:uniform(?GOSSIP_INTERVAL), gen_server, cast, [?MODULE, scheduled_gossip_ring]).

%% @doc Sync our ring to another random node.
do_gossip_ring(Ring) ->
    case zynamo_ring:random_other_node(Ring) of
        no_node -> ok;
        Node -> gen_server:cast({?MODULE, Node}, {sync_ring, node(), true, Ring})
    end.
