%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Simple implementation of an observer/notifier. Relays events to observers of that event.
%% Also implements map and fold operations over the observers.

%% Copyright 2009 Marc Worrell
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

-module(z_notifier).

-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    start_tests/0,
    observe/3,
    observe/4,
    detach/3,
    detach_all/2,
    get_observers/2,
    notify/2, 
    notify1/2, 
    first/2, 
    map/2, 
    foldl/3, 
    foldr/3
]).

%% internal
-export([notify_observer/4]).

-include_lib("zotonic.hrl").

-define(TIMEOUT, 60000).

-define(TIMER_INTERVAL, [ {1, tick_1s}, 
                          {60, tick_1m}, 
                          {3600, tick_1h},
                          {7200, tick_2h},
                          {43200, tick_12h},
                          {86400, tick_24h} ]).

-record(state, {observers, timers, context}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(SiteProps) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the notification server
start_link(SiteProps) when is_list(SiteProps) ->
    {host, Host} = proplists:lookup(host, SiteProps),
    Name = z_utils:name_for_host(?MODULE, Host),

    %% If the notifier crashes the supervisor gets ownership
    %% of the observer table. When it restarts the notifier
    %% it will give ownership back to the notifier.
    ObserverTable = ensure_observer_table(Host),

    case gen_server:start_link({local, Name}, ?MODULE, SiteProps, []) of
        {ok, P} ->
            ets:give_away(ObserverTable, P, observer_table),
            {ok, P};
        {already_started, P} ->
            ets:give_away(ObserverTable, P, observer_table),
            {already_started, P};
        R -> R
    end.

% Make sure the observer table exists.
%
ensure_observer_table(Name) ->
    Table = observer_table_name(Name),
    case ets:info(Table) of
        undefined -> ets:new(Table, [named_table, set, {keypos, 1}, protected, {heir, self(), []}]);
        _ -> Table
    end.

% Return the name of the observer table
%
observer_table_name(Context) when is_record(Context, context) ->
    observer_table_name(Context#context.host);
observer_table_name(Name) when is_atom(Name) ->
    z_utils:name_for_host(observers, Name).

%% @doc Start a notifier server for unit testing
start_tests() ->
    io:format("Starting notifier server.~n"),
    start_link([{host, test}]).


%%====================================================================
%% API for subscription
%%====================================================================

%% @doc Subscribe to an event. Observer is a {M,F} or pid()
observe(Event, {Module, Function}, Context) ->
    observe(Event, {Module, Function}, z_module_manager:prio(Module), Context);
observe(Event, Observer, Context) ->
    observe(Event, Observer, ?NOTIFIER_DEFAULT_PRIORITY, Context).

%% @doc Subscribe to an event. Observer is a {M,F} or pid()
observe(Event, Observer, Priority, #context{notifier=Notifier}) ->
    gen_server:cast(Notifier, {'observe', Event, Observer, Priority});
observe(Event, Observer, Priority, Host) when is_atom(Host) ->
    Notifier = z_utils:name_for_host(?MODULE, Host),
    gen_server:cast(Notifier, {'observe', Event, Observer, Priority}).


%% @doc Detach all observers and delete the event
detach_all(Event, #context{notifier=Notifier}) ->
    gen_server:cast(Notifier, {'detach_all', Event}).

%% @doc Unsubscribe from an event. Observer is a {M,F} or pid()
detach(Event, Observer, #context{notifier=Notifier}) ->
    gen_server:cast(Notifier, {'detach', Event, Observer});
detach(Event, Observer, Host) when is_atom(Host) ->
    Notifier = z_utils:name_for_host(?MODULE, Host),
    gen_server:cast(Notifier, {'detach', Event, Observer}).

%% @doc Return all observers for a particular event
get_observers(Msg, Context) when is_tuple(Msg) ->
    get_observers(element(1, Msg), Context);
get_observers(Event, #context{host=Host}) ->
    Table = observer_table_name(Host),
    case ets:lookup(Table, Event) of
        [] -> [];
        [{Event, Observers}] -> Observers
    end.


%%====================================================================
%% API for notification
%% Calls are done in the calling process, to prevent copying of 
%% possibly large contexts for small notifications.
%%====================================================================

%% @doc Cast the event to all observers. The prototype of the observer is: f(Msg, Context) -> void
notify(Msg, Context) ->
    Observers = get_observers(Msg, Context),
    AsyncContext = z_context:prune_for_async(Context),
    F = fun() ->
        lists:foreach(fun(Obs) -> notify_observer(Msg, Obs, false, AsyncContext) end, Observers)
    end,
    spawn(F),
    ok.

%% @doc Cast the event to the first observer. The prototype of the observer is: f(Msg, Context) -> void
notify1(Msg, Context) ->
    Observers = get_observers(Msg, Context),
    AsyncContext = z_context:prune_for_async(Context),
    case Observers of
        [Obs|_] -> 
            F = fun() -> notify_observer(Msg, Obs, false, AsyncContext) end,
            spawn(F);
        [] -> ok
    end.


%% @doc Call all observers till one returns something else than undefined. The prototype of the observer is: f(Msg, Context)
first(Msg, Context) ->
    Observers = get_observers(Msg, Context),
    first1(Observers, Msg, Context).

    first1([], _Msg, _Context) ->
        undefined;
    first1([Obs|Rest], Msg, Context) ->
        case notify_observer(Msg, Obs, true, Context) of
            Continue when Continue =:= undefined; Continue =:= continue -> 
                first1(Rest, Msg, Context);
            {continue, Msg1} ->
                first1(Rest, Msg1, Context);
            Result ->
                Result
        end.


%% @doc Call all observers, return the list of answers. The prototype of the observer is: f(Msg, Context)
map(Msg, Context) ->
    Observers = get_observers(Msg, Context),
    lists:map(fun(Obs) -> notify_observer(Msg, Obs, true, Context) end, Observers).


%% @doc Do a fold over all observers, prio 1 observers first. The prototype of the observer is: f(Msg, Acc, Context)
foldl(Msg, Acc0, Context) ->
    Observers = get_observers(Msg, Context),
    lists:foldl(
            fun(Obs, Acc) -> 
                notify_observer_fold(Msg, Obs, Acc, Context) 
            end, 
            Acc0,
            Observers).

%% @doc Do a fold over all observers, prio 1 observers last
foldr(Msg, Acc0, Context) ->
    Observers = get_observers(Msg, Context),
    lists:foldr(
            fun(Obs, Acc) -> 
                notify_observer_fold(Msg, Obs, Acc, Context) 
            end, 
            Acc0,
            Observers).



%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server, creates a new observer list
init(Args) ->
    {host, Host} = proplists:lookup(host, Args),
    Timers = [ timer:send_interval(Time * 1000, {tick, Msg}) || {Time, Msg} <- ?TIMER_INTERVAL ],
    State = #state{observers=undefined, timers=Timers, context=z_context:new(Host)},
    {ok, State}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Add an observer to an event
handle_cast({'observe', Event, Observer, Priority}, State) ->
    Event1 = case is_tuple(Event) of true -> element(1,Event); false -> Event end,
        
    PObs = {Priority, Observer},
    UpdatedObservers = case ets:lookup(State#state.observers, Event1) of 
        [] -> 
            [PObs];
        [{Event1, Observers}] ->
            % Prevent double observers, remove old observe first
            OtherObservers = lists:filter(fun({_Prio,Obs}) -> Obs /= Observer end, Observers),
            lists:sort([PObs | OtherObservers])
    end,
    ets:insert(State#state.observers, {Event1, UpdatedObservers}),

    {noreply, State};

%% @doc Detach an observer from an event
handle_cast({'detach', Event, Observer}, State) ->
	Event1 = case is_tuple(Event) of true -> element(1,Event); false -> Event end,

    case ets:lookup(State#state.observers, Event1) of 
        [] -> ok;
        [{Event1, Observers}] -> 
            UpdatedObservers = lists:filter(fun({_Prio,Obs}) -> Obs /= Observer end, Observers),
            ets:insert(State#state.observers, {Event1, UpdatedObservers})
    end,

    {noreply, State};


%% @doc Detach all observer from an event
handle_cast({'detach_all', Event}, State) ->
	Event1 = case is_tuple(Event) of true -> element(1,Event); false -> Event end,
    ets:delete(State#state.observers, Event1),  
    {noreply, State};


%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handle timer ticks
handle_info({tick, Msg}, State) ->
    spawn(fun() -> ?MODULE:notify(Msg, State#state.context) end),
    z_utils:flush_message({tick, Msg}),
    {noreply, State};

%% @doc Handle ets table transfers
handle_info({'ETS-TRANSFER', Table, _FromPid, observer_table}, State) ->
    {noreply, State#state{observers=Table}};

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.


%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(normal, State) ->
    cancel_timers(State#state.timers),
    ets:delete(State#state.observers),
    ok;
terminate(_Reason, State) ->
    cancel_timers(State#state.timers),
    ok.


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

cancel_timers(Timers) ->
    [ timer:cancel(TRef)  || {ok, TRef} <- Timers ].


%% @doc Notify an observer of an event
notify_observer(Msg, {_Prio, Fun}, _IsCall, Context) when is_function(Fun) ->
    Fun(Msg, Context);
notify_observer(Msg, {_Prio, Pid}, IsCall, Context) when is_pid(Pid) ->
    try
        case IsCall of
            true ->
                gen_server:call(Pid, {Msg, Context}, ?TIMEOUT);
            false ->
                gen_server:cast(Pid, {Msg, Context})
        end
    catch M:E ->
        ?ERROR("Error notifying %p with event %p. Detaching pid.", [Pid, Msg]),
        detach(msg_event(Msg), Pid, Context),
        {error, {notify_observer, Pid, Msg, M, E}}
    end;
notify_observer(Msg, {_Prio, {M,F}}, _IsCall, Context) ->
    M:F(Msg, Context);
notify_observer(Msg, {_Prio, {M,F,[Pid]}}, _IsCall, Context) when is_pid(Pid) ->
    try
        M:F(Pid, Msg, Context)
    catch EM:E ->
        ?ERROR("Error notifying %p with event %p. Detaching pid.", [{M,F,Pid}, Msg]),
        detach(msg_event(Msg), {M,F,[Pid]}, Context),
        {error, {notify_observer, Pid, Msg, EM, E}}
    end;
notify_observer(Msg, {_Prio, {M,F,Args}}, _IsCall, Context) ->
    erlang:apply(M, F, Args++[Msg, Context]).


%% @doc Notify an observer of an event, used in fold operations.  The receiving function should accept the message, the
%% accumulator and the context.
notify_observer_fold(Msg, {_Prio, Fun}, Acc, Context) when is_function(Fun) ->
    Fun(Msg, Acc, Context);
notify_observer_fold(Msg, {_Prio, Pid}, Acc, Context) when is_pid(Pid) ->
    try
        gen_server:call(Pid, {Msg, Acc, Context}, ?TIMEOUT)
    catch M:E ->
        ?ERROR("Error notifying %p with event %p. Detaching pid.", [Pid, Msg]),
        detach(msg_event(Msg), Pid, Context),
        {error, {notify_observer_fold, Pid, Msg, M, E}}
    end;
notify_observer_fold(Msg, {_Prio, {M,F}}, Acc, Context) ->
    M:F(Msg, Acc, Context);
notify_observer_fold(Msg, {_Prio, {M,F,[Pid]}}, Acc, Context) when is_pid(Pid) ->
    try
        M:F(Pid, Msg, Acc, Context)
    catch EM:E ->
        ?ERROR("Error notifying %p with event %p. Detaching pid.", [{M,F,Pid}, Msg]),
        detach(msg_event(Msg), {M,F,[Pid]}, Context),
        {error, {notify_observer, Pid, Msg, EM, E}}
    end;
notify_observer_fold(Msg, {_Prio, {M,F,Args}}, Acc, Context) ->
    erlang:apply(M, F, Args++[Msg, Acc, Context]).



msg_event(E) when is_atom(E) -> E;
msg_event(Msg) -> element(1, Msg).

