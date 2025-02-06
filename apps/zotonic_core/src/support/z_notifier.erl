%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2025 Marc Worrell
%% @doc Simple implementation of an observer/notifier. Relays events to observers of that event.
%% Also implements map and fold operations over the observers.
%% @end

%% Copyright 2009-2025 Marc Worrell
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

%% interface functions
-export([
    observe/3, observe/4, observe/5,
    detach/2, detach/3,
    detach_all/1, detach_all/2,
    get_observers/1,
    get_observers/2,
    notify/2,
    notify_sync/2,
    notify_queue/1,
    notify_queue_flush/1,
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

-type notification() :: atom()
                     | tuple().

-export_type([notification/0]).

-include("../../include/zotonic.hrl").

%% Default priority for notifiers. Normal module priorities are 1..1000.
-define(DEFAULT_PRIORITY, 500).

-define(DEFAULT_TIMEOUT, 5000).


%% @doc Subscribe to an event. Observer is a MFA or pid()
observe(Event, {Module, Function}, Context) ->
    observe(Event, {Module, Function, []}, Context);
observe(Event, Observer, Context) ->
    observe(Event, Observer, prio(Observer), Context).

%% @doc Subscribe to an event. Observer is a MFA or pid()
observe(Event, Observer, Priority, Context) when is_integer(Priority) ->
    observe(Event, Observer, self(), Priority, Context);
observe(Event, Observer, OwnerPid, Context) when is_pid(OwnerPid) ->
    observe(Event, Observer, OwnerPid, prio(Observer), Context).

-spec observe( zotonic_notifier:event(), zotonic_notifier:observer(), pid(), integer(), atom() | z:context() ) -> ok | {error, term()}.
observe(Event, Observer, OwnerPid, Priority, Site) when is_atom(Site), is_pid(OwnerPid) ->
    zotonic_notifier:observe({Site, msg_event(Event)}, Observer, OwnerPid, Priority);
observe(Event, Observer, OwnerPid, Priority, Context) ->
    observe(Event, Observer, OwnerPid, Priority, z_context:site(Context)).


%% @doc Detach all observers and delete the event
detach_all(Context) ->
    detach_all(self(), Context).

detach_all(OwnerPid, Site) when is_atom(Site), is_pid(OwnerPid) ->
    zotonic_notifier:detach_all(OwnerPid);
detach_all(OwnerPid, Context) ->
    detach_all(OwnerPid, z_context:site(Context)).

%% @doc Unsubscribe from an event.
detach(Event, Context) ->
    detach(Event, self(), Context).

detach(Event, OwnerPid, Site) when is_atom(Site), is_pid(OwnerPid) ->
    zotonic_notifier:detach({Site, msg_event(Event)}, OwnerPid);
detach(Event, OwnerPid, Context) ->
    detach(Event, OwnerPid, z_context:site(Context)).

%% @doc List all observers for all events for the site.
-spec get_observers( z:context() ) -> list( {atom(), list({integer(), zotonic_notifier:observer()})} ).
get_observers(Context) ->
    Site = z_context:site(Context),
    List = lists:filtermap(
        fun
            ({{S, Event}, Obs}) when S =:= Site ->
                {true, {Event, Obs}};
            (_) ->
                false
        end,
        zotonic_notifier:get_observers('_')),
    lists:sort(List).

%% @doc Return all observers for a particular event
get_observers(Event, Site) when is_atom(Site) ->
    zotonic_notifier:get_observers({Site, msg_event(Event)});
get_observers(Event, Context) ->
    get_observers(Event, z_context:site(Context)).


%%====================================================================
%% API for notification
%% Calls are done in the calling process, to prevent copying of
%% possibly large contexts for small notifications.
%%====================================================================

%% @doc Async cast the event to all observers. A process is forked in which all observers are
%% called. If there is a database transaction then the notification is delayed till the end of
%% the transaction. The prototype of the observer is: f(Msg, Context) -> void
-spec notify(Msg, Context) -> ok | {error, Reason} when
    Msg :: notification(),
    Context :: z:context(),
    Reason :: term().
notify(Msg, #context{dbc = undefined} = Context) ->
    zotonic_notifier:notify({z_context:site(Context), msg_event(Msg)}, Msg, Context);
notify(Msg, _Context) ->
    delay_notification({notify, Msg}).

%% @doc Sync cast the event to all observers. All observers are called in the current process.
%% This is ideal for frequent notifications or notifications that need to be handled before
%% processing can continue. The prototype of the observer is: f(Msg, Context) -> void
-spec notify_sync(Msg, Context) -> ok | {error, Reason} when
    Msg :: notification(),
    Context :: z:context(),
    Reason :: term().
notify_sync(Msg, Context) ->
    zotonic_notifier:notify_sync({z_context:site(Context), msg_event(Msg)}, Msg, Context).

%% @doc Async cast the event to the first observer. A process is forked in which the observer is
%% called. If there is a database transaction then the notification is delayed till the end of
%% the transaction. The prototype of the observer is: f(Msg, Context) -> void
-spec notify1(Msg, Context) -> ok | {error, Reason} when
    Msg :: notification(),
    Context :: z:context(),
    Reason :: term().
notify1(Msg, #context{dbc = undefined} = Context) ->
    zotonic_notifier:notify1({z_context:site(Context), msg_event(Msg)}, Msg, Context);
notify1(Msg, _Context) ->
    delay_notification({notify1, Msg}).

%% @doc Call all observers till one returns something else than undefined. If there are no
%% observers then undefined is returned.
%% The prototype of the observer is: f(Msg, Context)
-spec first(Msg, Context) -> Result when
    Msg :: notification(),
    Context :: z:context(),
    Result :: undefined | term().
first(Msg, Context) ->
    zotonic_notifier:first({z_context:site(Context), msg_event(Msg)}, Msg, Context).

%% @doc Call all observers, return the list of answers. The prototype of the
%% observer is: f(Msg, Context)
-spec map(Msg, Context) -> Result when
    Msg :: notification(),
    Context :: z:context(),
    Result :: list( term() ).
map(Msg, Context) ->
    zotonic_notifier:map({z_context:site(Context), msg_event(Msg)}, Msg, Context).


%% @doc Do a fold over all observers, prio 1 observers first. The prototype of
%% the observer is: f(Msg, Acc, Context)
-spec foldl(Msg, Acc, Context) -> Result when
    Msg :: notification(),
    Context :: z:context(),
    Acc :: term(),
    Result :: term().
foldl(Msg, Acc0, Context) ->
    zotonic_notifier:foldl({z_context:site(Context), msg_event(Msg)}, Msg, Acc0, Context).

%% @doc Do a fold over all observers, prio 1 observers last. The prototype of
%% the observer is: f(Msg, Acc, Context)
-spec foldr(Msg, Acc, Context) -> Result when
    Msg :: notification(),
    Context :: z:context(),
    Acc :: term(),
    Result :: term().
foldr(Msg, Acc0, Context) ->
    zotonic_notifier:foldr({z_context:site(Context), msg_event(Msg)}, Msg, Acc0, Context).

%% @doc Send all delayed notify or notify1 notifications. This is used to delay notifications
%% during a database transaction. When the transaction finishes all notifications are sent.
-spec notify_queue(Context) -> ok | {error, transaction} when
    Context :: z:context().
notify_queue(#context{dbc = undefined} = Context) ->
    case erlang:get(notify_queue) of
        undefined ->
            nop;
        Queue ->
            lists:foreach(
                fun
                    ({notify, Msg}) ->
                        notify(Msg, Context);
                    ({notify1, Msg}) ->
                        notify1(Msg, Context)
                end,
                lists:reverse(Queue)
            )
    end,
    erlang:erase(notify_queue),
    ok;
notify_queue(#context{dbc = _}) ->
    {error, transaction}.

%% @doc Erase queued notifications. Useful if delayed notifications should not be sent, for
%% example if the current database transaction rolled back. The queue is only flushed if there
%% isn't a database transaction.
-spec notify_queue_flush(Context) -> ok | {error, transaction} when
    Context :: z:context().
notify_queue_flush(#context{dbc = undefined}) ->
    erlang:erase(notify_queue),
    ok;
notify_queue_flush(#context{dbc = _}) ->
    {error, transaction}.

%% @doc Subscribe once to a notification, detach after receiving the notification.
%% After 5 seconds an error 'timeout' is returned.
-spec await(Notification, Context) ->
          {ok, notification()}
        | {ok, {pid(), reference()}, notification()}
        | {error, Reason} when
    Notification :: notification(),
    Context :: z:context(),
    Reason :: timeout.
await(Msg, Context) ->
    await(Msg, ?DEFAULT_TIMEOUT, Context).

%% @doc Subscribe once to a notification, detach after receiving the notification.
%% After the timeout (in msec) an error 'timeout' is returned.
-spec await(Notification, Timeout, Context) ->
          {ok, notification()}
        | {ok, {pid(), reference()}, notification()}
        | {error, Reason} when
    Notification :: notification(),
    Timeout :: pos_integer(),
    Context :: z:context(),
    Reason :: timeout.
await(Msg, Timeout, Context) ->
    zotonic_notifier:await({z_context:site(Context), msg_event(Msg)}, Msg, Timeout).

-spec await_exact(Notification, Context) ->
          {ok, notification()}
        | {ok, {pid(), reference()}, notification()}
        | {error, Reason} when
    Notification :: notification(),
    Context :: z:context(),
    Reason :: timeout.
await_exact(Msg, Context) ->
    await_exact(Msg, ?DEFAULT_TIMEOUT, Context).

-spec await_exact(Notification, Timeout, Context) ->
          {ok, notification()}
        | {ok, {pid(), reference()}, notification()}
        | {error, Reason} when
    Notification :: notification(),
    Timeout :: pos_integer(),
    Context :: z:context(),
    Reason :: timeout.
await_exact(Msg, Timeout, Context) ->
    zotonic_notifier:await_exact({z_context:site(Context), msg_event(Msg)}, Msg, Timeout).

%%====================================================================
%% support functions
%%====================================================================

-spec msg_event( atom() | tuple() ) -> atom().
msg_event(Event) when is_atom(Event) -> Event;
msg_event(Msg) when is_tuple(Msg) -> element(1, Msg).

delay_notification(Msg) ->
    case erlang:get(notify_queue) of
        undefined ->
            erlang:put(notify_queue, [Msg]);
        Queue ->
            erlang:put(notify_queue, [Msg | Queue])
    end.

prio({M, _F, _A}) -> z_module_manager:prio(M);
prio({M, _F}) -> z_module_manager:prio(M);
prio(Pid) when is_pid(Pid) -> ?DEFAULT_PRIORITY.

