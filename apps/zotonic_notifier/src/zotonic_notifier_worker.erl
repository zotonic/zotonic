%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%%
%% @doc Simple implementation of an observer/notifier. Relays events to observers of that event.
%% Also implements map and fold operations over the observers.

%% Copyright 2009-2017 Marc Worrell
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

-module(zotonic_notifier_worker).

-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    start_tests/0,

    observe/5,

    detach/3,
    detach_all/2,

    get_observers/1,
    get_observers/2,

    notify_sync/4,
    notify_async/4,

    notify1/4,
    first/4,
    map/4,

    foldl/5,
    foldr/5,

    await/4,
    await_exact/4
]).

%% internal
-export([notify_observer/4]).

-define(TIMEOUT, 60000).

-record(state, {
    name :: atom(),
    observers :: ets:tab(),  % { term(), [ {prio, zotonic_notifier:observer(), pid()} ] }
    monitors :: map(),       % pid() => reference()
    pid2event :: map()       % pid() => [ term() ]
}).


%%====================================================================
%% API
%%====================================================================
%% @doc Starts the notification server
-spec start_link(Name :: atom()) -> {ok, pid()} | {error, term()}.
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, Name, []).

%% @doc Start a notifier server for unit testing
-spec start_tests() -> {ok, pid()} | {error, term()}.
start_tests() ->
    io:format("Starting notifier server.~n"),
    start_link(test).


%%====================================================================
%% API for subscription
%%====================================================================

%% @doc Subscribe to an event. Observer is a {M,F} or pid()
-spec observe(zotonic_notifier:notifier(), zotonic_notifier:event(), zotonic_notifier:observer(),
              pid(), integer()) -> ok | {error, term()}.
observe(Notifier, Event, Observer, OwnerPid, Prio) ->
    gen_server:call(Notifier, {observe, Event, Observer, OwnerPid, Prio}, infinity).


%% @doc Detach all observers for the owner
-spec detach_all(zotonic_notifier:notifier(), pid()) -> ok | {error, term()}.
detach_all(Notifier, OwnerPid) when is_pid(OwnerPid) ->
    gen_server:call(Notifier, {detach_all, OwnerPid}, infinity).

%% @doc Unsubscribe an owner-pid from an event.
-spec detach(zotonic_notifier:notifier(), zotonic_notifier:event(), pid()) -> ok | {error, term()}.
detach(Notifier, Event, OwnerPid) ->
    gen_server:call(Notifier, {detach, Event, OwnerPid}, infinity).

%% @doc Return all observers
-spec get_observers(zotonic_notifier:notifier()) -> list().
get_observers(Notifier) ->
    Table = observer_table_name(Notifier),
    lists:flatten( ets:match(Table, '$1') ).

%% @doc Return all observers for a particular event
-spec get_observers(zotonic_notifier:notifier(), zotonic_notifier:event()) -> list().
get_observers(Notifier, Event) ->
    Table = observer_table_name(Notifier),
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
notify_sync(Notifier, Event, Msg, ContextArg) ->
    case get_observers(Notifier, Event) of
        [] ->
            ok;
        Observers ->
            lists:foreach(
                fun(Obs) ->
                    notify_observer(Msg, Obs, false, ContextArg)
                end,
                Observers)
    end.

%% @doc Cast the event to all observers. The prototype of the observer is: f(Msg, Context) -> void
notify_async(Notifier, Event, Msg, ContextArg) ->
    case get_observers(Notifier, Event) of
        [] ->
            ok;
        Observers ->
            case lists:all(fun is_pid_observer/1, Observers) of
                true ->
                    lists:foreach(
                        fun(Obs) ->
                            notify_observer(Msg, Obs, false, ContextArg)
                        end,
                        Observers);
                false ->
                    MD = logger:get_process_metadata(),
                    proc_lib:spawn(
                        fun() ->
                            set_process_metadata(MD),
                            lists:foreach(
                                fun(Obs) ->
                                    notify_observer(Msg, Obs, false, ContextArg)
                                end,
                                Observers)
                        end),
                    ok
            end
    end.

%% @doc Cast the event to the first observer. The prototype of the observer is: f(Msg, Context) -> void
notify1(Notifier, Event, Msg, ContextArg) ->
    case get_observers(Notifier, Event) of
        [] -> ok;
        [ {_, Pid, _} = Obs | _ ] when is_pid(Pid) ->
            notify_observer(Msg, Obs, false, ContextArg);
        [ Obs | _ ] ->
            MD = logger:get_process_metadata(),
            proc_lib:spawn(
                fun() ->
                    set_process_metadata(MD),
                    notify_observer(Msg, Obs, false, ContextArg)
                end)
    end.

set_process_metadata(undefined) -> ok;
set_process_metadata(MD) -> logger:set_process_metadata(MD).

%% @doc Call all observers till one returns something else than undefined.
%% The prototype of the observer is: f(Msg, Context)
first(Notifier, Event, Msg, ContextArg) ->
    Observers = get_observers(Notifier, Event),
    first_1(Observers, Msg, ContextArg).

first_1([], _Msg, _ContextArgs) ->
    undefined;
first_1([Obs|Rest], Msg, ContextArg) ->
    case notify_observer(Msg, Obs, true, ContextArg) of
        undefined -> first_1(Rest, Msg, ContextArg);
        continue -> first_1(Rest, Msg, ContextArg);
        {continue, Msg1} -> first_1(Rest, Msg1, ContextArg);
        Result -> Result
    end.


%% @doc Call all observers, return the list of answers. The prototype of the
%% observer is: f(Msg, ContextArg)
map(Notifier, Event, Msg, ContextArg) ->
    Observers = get_observers(Notifier, Event),
    lists:map(
        fun(Obs) ->
            notify_observer(Msg, Obs, true, ContextArg)
        end,
        Observers).


%% @doc Do a fold over all observers, prio 1 observers first. The prototype of
%% the observer is: f(Msg, Acc, ContextArg)
foldl(Notifier, Event, Msg, Acc0, ContextArg) ->
    Observers = get_observers(Notifier, Event),
    lists:foldl(
            fun(Obs, Acc) ->
                notify_observer_fold(Msg, Obs, Acc, ContextArg)
            end,
            Acc0,
            Observers).

%% @doc Do a fold over all observers, prio 1 observers last
foldr(Notifier, Event, Msg, Acc0, ContextArg) ->
    Observers = get_observers(Notifier, Event),
    lists:foldr(
            fun(Obs, Acc) ->
                notify_observer_fold(Msg, Obs, Acc, ContextArg)
            end,
            Acc0,
            Observers).

%% @doc Subscribe once to a notification, detach after receiving the notification.
-spec await(zotonic_notifier:notifier(), zotonic_notifier:event(), atom()|tuple(), pos_integer()) ->
        {ok, tuple()|atom()} |
        {ok, {pid(), reference()}, tuple()|atom()} |
        {error, timeout}.
await(Notifier, Event, Msg, Timeout) ->
    observe(Notifier, Event, self(), self(), 1),
    await_1(Notifier, Event, Msg, Timeout).

await_1(Notifier, Event, Msg, Timeout) ->
    Result = await_receive(Msg, Timeout),
    detach(Notifier, Event, self()),
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

-spec await_exact(zotonic_notifier:notifier(), zotonic_notifier:event(), term(), pos_integer()) ->
        {ok, term()} |
        {ok, {pid(), reference()}, term()} |
        {error, timeout}.
await_exact(Notifier, Event, Msg, Timeout) ->
    observe(Notifier, Event, self(), self(), 1),
    Result = receive
        Msg -> {ok, Msg};
        {'$gen_cast', Msg} -> {ok, Msg};
        {'$gen_call', From, Msg} -> {ok, From, Msg}
    after Timeout ->
        {error, timeout}
    end,
    detach(Notifier, Event, self()),
    Result.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server, creates a new observer list
-spec init(atom()) -> {ok, #state{}}.
init(Name) ->
    logger:set_process_metadata(#{
        name => Name,
        module => ?MODULE
    }),
    Table = observer_table_name(Name),
    ets:new(Table, [ named_table, set, {keypos, 1}, protected ]),
    State = #state{
        name = Name,
        observers = Table,
        monitors = #{},
        pid2event = #{}
    },
    {ok, State}.


%% @doc Add an observer to an event
handle_call({observe, Event, Observer, OwnerPid, Prio}, _From, State) ->
    do_add_observer(State#state.observers, Event, {Prio, Observer, OwnerPid}),
    State1 = do_add_monitor(OwnerPid, State),
    State2 = do_add_pid2event(OwnerPid, Event, State1),
    {reply, ok, State2};

%% @doc Detach an observer from an event
handle_call({detach, Event, OwnerPid}, _From, State) ->
    do_detach_observer(State#state.observers, Event, OwnerPid),
    State1 = do_remove_pid2event(OwnerPid, Event, State),
    {reply, ok, State1};

%% @doc Detach all observers owned by a pid
handle_call({detach_all, OwnerPid}, _From, State) ->
    State1 = do_detach_all(OwnerPid, State),
    {reply, ok, State1};


%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @doc Handling all non call/cast messages
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    State1 = do_detach_all(Pid, State),
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc Convert process state when code is changed
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

is_pid_observer({_Prio, Observer, _OwnerPid}) ->
    is_pid(Observer).

do_detach_all(OwnerPid, #state{ observers = Table, pid2event = Ps, monitors = Ms } = State) ->
    case maps:find(OwnerPid, Ps) of
        {ok, Es} ->
            lists:foreach(
                fun(Event) ->
                    do_detach_observer(Table, Event, OwnerPid)
                end,
                Es);
        error ->
            ok
    end,
    State#state{
        pid2event = maps:remove(OwnerPid, Ps),
        monitors = maps:remove(OwnerPid, Ms)
    }.

do_detach_observer(Table, Event, OwnerPid) ->
    case ets:lookup(Table, Event) of
        [] ->
            ok;
        [{Event, Observers}] ->
            UpdatedObservers = lists:filter(
                fun({_Prio, _Obs, Pid}) ->
                    Pid =/= OwnerPid
                end,
                Observers),
            ets:insert(Table, {Event, UpdatedObservers})
    end.

do_remove_pid2event(OwnerPid, Event, #state{ pid2event = P2E } = State) ->
    case maps:find(OwnerPid, P2E) of
        {ok, [ Event ]} ->
            State#state{ pid2event = maps:remove(OwnerPid, P2E) };
        {ok, Es} ->
            Es1 = [ E || E <- Es, E =/= Event ],
            State#state{ pid2event = P2E#{ OwnerPid => Es1 } };
        error ->
            State
    end.

do_add_observer(Table, Event, {_Prio, Observer, OwnerPid} = PObs) ->
    UpdatedObservers = case ets:lookup(Table, Event) of
        [] ->
            [ PObs ];
        [{Event, Observers}] ->
            % Prevent double observers, remove old observe first
            OtherObservers = lists:filter(
                fun({_, Obs, Pid}) ->
                    Obs =/= Observer orelse Pid =/= OwnerPid
                end,
                Observers),
            lists:sort([ PObs | OtherObservers ])
    end,
    ets:insert(Table, {Event, UpdatedObservers}).

do_add_pid2event(Pid, Event, #state{ pid2event = P2E } = State) ->
    case maps:find(Pid, P2E) of
        {ok, Es} ->
            case lists:member(Event, Es) of
                true ->
                    State;
                false ->
                    P2E1 = P2E#{ Pid => [ Event | Es ] },
                    State#state{ pid2event = P2E1 }
            end;
        error ->
            P2E1 = P2E#{ Pid => [ Event ] },
            State#state{ pid2event = P2E1 }
    end.

do_add_monitor(Pid, #state{ monitors = Monitors } = State) ->
    case maps:find(Pid, Monitors) of
        {ok, _} ->
            State;
        error ->
            MRef = erlang:monitor(process, Pid),
            State#state{ monitors = Monitors#{ Pid => MRef } }
    end.


% Return the name of the observer table
%
observer_table_name(zotonic_notifier) ->
    'observers$zotonic_notifier';
observer_table_name(Name) when is_atom(Name) ->
    list_to_atom("observers$" ++ atom_to_list(Name)).


%% @doc Notify an observer of an event
notify_observer(Msg, {_Prio, Pid, _OwnerPid}, true, ContextArg) when is_pid(Pid) ->
    try
        gen_server:call(Pid, {Msg, ContextArg}, ?TIMEOUT)
    catch
        EM:E:Trace ->
            ?LOG_ERROR(#{
                text => <<"Error notifying observer">>,
                in => zotonic_notifier,
                result => EM,
                reason => E,
                pid => Pid,
                event => Msg,
                stack => Trace
            }),
            {error, {notify_observer, Pid, Msg, EM, E}}
    end;
notify_observer(Msg, {_Prio, Pid, _OwnerPid}, false, ContextArg) when is_pid(Pid) ->
    gen_server:cast(Pid, {Msg, ContextArg});
notify_observer(Msg, {_Prio, {M, F}, _OwnerPid}, _IsCall, ContextArg) ->
    erlang:apply(M, F, [ Msg, ContextArg ]);
notify_observer(Msg, {_Prio, {M, F, As}, _OwnerPid}, _IsCall, ContextArg) ->
    erlang:apply(M, F, As ++ [ Msg, ContextArg ]).


%% @doc Notify an observer of an event, used in fold operations.  The receiving function should accept the message, the
%% accumulator and the context.
notify_observer_fold(Msg, {_Prio, Pid, _OwnerPid}, Acc, ContextArg) when is_pid(Pid) ->
    try
        gen_server:call(Pid, {Msg, Acc, ContextArg}, ?TIMEOUT)
    catch
        EM:E:Trace ->
            ?LOG_ERROR(#{
                text => <<"Error folding observers">>,
                in => zotonic_notifier,
                pid => Pid,
                result => EM,
                reason => E,
                message => Msg,
                stack => Trace
            }),
            Acc
    end;
notify_observer_fold(Msg, {_Prio, {M, F}, _OwnerPid}, Acc, ContextArg) ->
    erlang:apply(M, F, [ Msg, Acc, ContextArg ]);
notify_observer_fold(Msg, {_Prio, {M, F, As}, _OwnerPid}, Acc, ContextArg) ->
    erlang:apply(M, F, As ++ [ Msg, Acc, ContextArg ]).

