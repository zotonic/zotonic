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
    notify_sync/2,
    notify1/2,
    first/2,
    map/2,
    foldl/3,
    foldr/3,
    await/2,
    await/3,
    await_exact/2,
    await_exact/3
]).

%% internal
-export([notify_observer/4]).

-include_lib("zotonic.hrl").

-define(TIMEOUT, 60000).

-define(TIMER_INTERVAL, [ {1, tick_1s},
                          {60, tick_1m},
                          {600, tick_10m},
                          {3600, tick_1h},
                          {7200, tick_2h},
                          {21600, tick_6h},
                          {43200, tick_12h},
                          {86400, tick_24h} ]).

-record(state, {observers :: undefined | list(), timers :: list(), site :: atom()}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(SiteProps) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the notification server
start_link(SiteProps) when is_list(SiteProps) ->
    {site, Site} = proplists:lookup(site, SiteProps),
    Name = z_utils:name_for_site(?MODULE, Site),

    %% If the notifier crashes the supervisor gets ownership
    %% of the observer table. When it restarts the notifier
    %% it will give ownership back to the notifier.
    ObserverTable = ensure_observer_table(Site),

    case gen_server:start_link({local, Name}, ?MODULE, SiteProps, []) of
        {ok, P} ->
            ets:give_away(ObserverTable, P, observer_table),
            {ok, P};
        {error, {already_started, P}} ->
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
    observer_table_name(z_context:site(Context));
observer_table_name(Name) when is_atom(Name) ->
    z_utils:name_for_site(observers, Name).

%% @doc Start a notifier server for unit testing
start_tests() ->
    io:format("Starting notifier server.~n"),
    start_link([{site, test}]).


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
    gen_server:call(Notifier, {'observe', Event, Observer, Priority});
observe(Event, Observer, Priority, Site) when is_atom(Site) ->
    Notifier = z_utils:name_for_site(?MODULE, Site),
    gen_server:call(Notifier, {'observe', Event, Observer, Priority}).


%% @doc Detach all observers and delete the event
detach_all(Event, #context{notifier=Notifier}) ->
    gen_server:call(Notifier, {'detach_all', Event}).

%% @doc Unsubscribe from an event. Observer is a {M,F} or pid()
detach(Event, Observer, #context{notifier=Notifier}) ->
    gen_server:call(Notifier, {'detach', Event, Observer});
detach(Event, Observer, Site) when is_atom(Site) ->
    Notifier = z_utils:name_for_site(?MODULE, Site),
    gen_server:call(Notifier, {'detach', Event, Observer}).

%% @doc Return all observers for a particular event
get_observers(Msg, Context) when is_tuple(Msg) ->
    get_observers(element(1, Msg), Context);
get_observers(Event, #context{site=Site}) ->
    Table = observer_table_name(Site),
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
    case get_observers(Msg, Context) of
        [] -> ok;
        Observers ->
            AsyncContext = z_context:prune_for_async(Context),
            F = fun() ->
                    lists:foreach(fun(Obs) -> notify_observer(Msg, Obs, false, AsyncContext) end, Observers)
            end,
            spawn(F),
            ok
    end.

%% @doc Cast the event to all observers. The prototype of the observer is: f(Msg, Context) -> void
notify_sync(Msg, Context) ->
    case get_observers(Msg, Context) of
        [] ->
            ok;
        Observers ->
            lists:foreach(fun(Obs) -> notify_observer(Msg, Obs, false, Context) end, Observers)
    end.

%% @doc Cast the event to the first observer. The prototype of the observer is: f(Msg, Context) -> void
notify1(Msg, Context) ->
    case get_observers(Msg, Context) of
        [] -> ok;
        [Obs|_] ->
            AsyncContext = z_context:prune_for_async(Context),
            F = fun() -> notify_observer(Msg, Obs, false, AsyncContext) end,
            spawn(F)
    end.


%% @doc Call all observers till one returns something else than undefined.
%% The prototype of the observer is: f(Msg, Context)
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


%% @doc Call all observers, return the list of answers. The prototype of the
%% observer is: f(Msg, Context)
map(Msg, Context) ->
    Observers = get_observers(Msg, Context),
    lists:map(fun(Obs) -> notify_observer(Msg, Obs, true, Context) end, Observers).


%% @doc Do a fold over all observers, prio 1 observers first. The prototype of
%% the observer is: f(Msg, Acc, Context)
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


%% @doc Subscribe once to a notification, detach after receiving the notification.
-spec await(tuple()|atom(), #context{}) ->
        {ok, tuple()|atom()} |
        {ok, {pid(), reference()}, tuple()|atom()} |
        {error, timeout}.
await(Msg, Context) ->
    await(Msg, 5000, Context).

-spec await(tuple()|atom(), pos_integer(), #context{}) ->
        {ok, tuple()|atom()} |
        {ok, {pid(), reference()}, tuple()|atom()} |
        {error, timeout}.
await(Msg, Timeout, Context) when is_atom(Msg) ->
    observe(Msg, self(), Context),
    await_1(Msg, Timeout, Context);
await(Msg, Timeout, Context) when is_tuple(Msg) ->
    observe(Msg, self(), Context),
    await_1(element(1, Msg), Timeout, Context).

await_1(Msg, Timeout, Context) ->
    Result = await_receive(Msg, Timeout),
    detach(Msg, self(), Context),
    Result.

await_receive(Msg, Timeout) when is_atom(Msg) ->
    receive
        Msg -> {ok, Msg};
        M when is_tuple(M), element(1, M) =:= Msg -> {ok, M};
        {'$gen_cast', Msg} -> {ok, Msg};
        {'$gen_cast', M} when is_tuple(M), element(1, M) =:= Msg -> {ok, M};
        {'$gen_call', From, Msg} -> {ok, From, Msg};
        {'$gen_call', From, M} when is_tuple(M), element(1, M) =:= Msg -> {ok, From, M}
    after Timeout ->
        {error, timeout}
    end.

-spec await_exact(tuple()|atom(), #context{}) ->
        {ok, tuple()|atom()} |
        {ok, {pid(), reference()}, tuple()|atom()} |
        {error, timeout}.
await_exact(Msg, Context) ->
    await_exact(Msg, 5000, Context).

-spec await_exact(tuple()|atom(), pos_integer(), #context{}) ->
        {ok, tuple()|atom()} |
        {ok, {pid(), reference()}, tuple()|atom()} |
        {error, timeout}.
await_exact(Msg, Timeout, Context) ->
    observe(Msg, self(), Context),
    Result = receive
        Msg -> {ok, Msg};
        {'$gen_cast', Msg} -> {ok, Msg};
        {'$gen_call', From, Msg} -> {ok, From, Msg}
    after Timeout ->
        {error, timeout}
    end,
    detach(Msg, self(), Context),
    Result.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server, creates a new observer list
init(Args) ->
    {site, Site} = proplists:lookup(site, Args),
    lager:md([
        {site, Site},
        {module, ?MODULE}
      ]),
    Timers = [ timer:send_interval(Time * 1000, {tick, Msg}) || {Time, Msg} <- ?TIMER_INTERVAL ],
    State = #state{observers=undefined, timers=Timers, site=Site},
    {ok, State}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Add an observer to an event
handle_call({'observe', Event, Observer, Priority}, _From, State) ->
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
    {reply, ok, State};

%% @doc Detach an observer from an event
handle_call({'detach', Event, Observer}, _From, State) ->
    Event1 = case is_tuple(Event) of true -> element(1,Event); false -> Event end,
    case ets:lookup(State#state.observers, Event1) of
        [] -> ok;
        [{Event1, Observers}] ->
            UpdatedObservers = lists:filter(fun({_Prio,Obs}) -> Obs /= Observer end, Observers),
            ets:insert(State#state.observers, {Event1, UpdatedObservers})
    end,
    {reply, ok, State};

%% @doc Detach all observer from an event
handle_call({'detach_all', Event}, _From, State) ->
    Event1 = case is_tuple(Event) of true -> element(1,Event); false -> Event end,
    ets:delete(State#state.observers, Event1),
    {reply, ok, State};


%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handle timer ticks
handle_info({tick, Msg}, #state{site=Site} = State) ->
    case catch z_context:new(Site) of
        #context{} = Context ->
            spawn(fun() ->
                    ?MODULE:notify(Msg, Context)
                  end);
        _ ->
            % z_trans_server not running, skip this tick
            skip
    end,
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
    catch EM:E ->
        case z_utils:is_process_alive(Pid) of
            false ->
                lager:error("Error notifying ~p with event ~p. Error ~p:~p. Detaching pid. (~p)",
                            [Pid, Msg, EM, E, erlang:get_stacktrace()]),
                detach(msg_event(Msg), Pid, Context);
            true ->
                % Assume transient error
                nop
        end,
        {error, {notify_observer, Pid, Msg, EM, E}}
    end;
notify_observer(Msg, {_Prio, {M,F}}, _IsCall, Context) ->
    M:F(Msg, Context);
notify_observer(Msg, {_Prio, {M,F,[Pid]}}, _IsCall, Context) when is_pid(Pid) ->
    try
        M:F(Pid, Msg, Context)
    catch EM:E ->
        case z_utils:is_process_alive(Pid) of
            false ->
                lager:error("Error notifying ~p with event ~p. Error ~p:~p. Detaching pid. (~p)",
                            [{M,F,Pid}, Msg, EM, E, erlang:get_stacktrace()]),
                detach(msg_event(Msg), {M,F,[Pid]}, Context);
            true ->
                % Assume transient error
                nop
        end,
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
    catch EM:E ->
        case z_utils:is_process_alive(Pid) of
            false ->
                lager:error("Error folding ~p with event ~p. Error ~p:~p. Detaching pid. (~p)",
                            [Pid, Msg, EM, E, erlang:get_stacktrace()]),
                detach(msg_event(Msg), Pid, Context);
            true ->
                % Assume transient error
                lager:error("Error folding ~p with event ~p. Error ~p:~p (~p)",
                            [Pid, Msg, EM, E, erlang:get_stacktrace()])
        end,
        Acc
    end;
notify_observer_fold(Msg, {_Prio, {M,F}}, Acc, Context) ->
    M:F(Msg, Acc, Context);
notify_observer_fold(Msg, {_Prio, {M,F,[Pid]}}, Acc, Context) when is_pid(Pid) ->
    try
        M:F(Pid, Msg, Acc, Context)
    catch EM:E ->
        case z_utils:is_process_alive(Pid) of
            false ->
                lager:error("Error folding ~p with event ~p. Error ~p:~p. Detaching pid. (~p)",
                            [{M,F,Pid}, Msg, EM, E, erlang:get_stacktrace()]),
                detach(msg_event(Msg), {M,F,[Pid]}, Context);
            true ->
                % Assume transient error
                lager:error("Error folding ~p with event ~p. Error ~p:~p (~p)",
                            [{M,F,Pid}, Msg, EM, E, erlang:get_stacktrace()])
        end,
        Acc
    end;
notify_observer_fold(Msg, {_Prio, {M,F,Args}}, Acc, Context) ->
    erlang:apply(M, F, Args++[Msg, Acc, Context]).



msg_event(E) when is_atom(E) -> E;
msg_event(Msg) -> element(1, Msg).

