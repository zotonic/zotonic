%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Extension system using notifications with fold, map and priorities.

%% Copyright 2017 Marc Worrell
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

-module(zotonic_notifier).

-behaviour(application).

-export([
    start/0,
    start/2,
    stop/1,

    start_notifier/1,
    stop_notifier/1,

    observe/2, observe/3, observe/4, observe/5,
    detach/2, detach/3,
    detach_all/1, detach_all/2,
    get_observers/1, get_observers/2,
    notify_sync/3, notify_sync/4,
    notify/3, notify/4,
    notify1/3, notify1/4,
    first/3, first/4,
    map/3, map/4,
    foldl/4,  foldl/5,
    foldr/4, foldr/5,
    await/1, await/2, await/3, await/4,
    await_exact/2, await_exact/3, await_exact/4
]).

-type observer() :: pid()
                  | {module(), atom()}
                  | {module(), atom(), list()}.

-type event() :: term().

-type notifier() :: atom() | pid().

-export_type([
    observer/0,
    event/0,
    notifier/0
]).

-include_lib("zotonic_notifier.hrl").

%%====================================================================
%% API
%%====================================================================

start() ->
    ensure_started(zotonic_notifier).

start(_StartType, _StartArgs) ->
    zotonic_notifier_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

-spec start_notifier(atom()) -> {ok, pid()} | {error, term()}.
start_notifier(Name) when is_atom(Name) ->
    case zotonic_notifier_sup:start_notifier(Name) of
        {error, {already_started, Pid}} -> {ok, Pid};
        Other -> Other
    end.

-spec stop_notifier(atom()) -> ok.
stop_notifier(Name) when is_atom(Name) ->
    zotonic_notifier_sup:stop_notifier(Name).

%% @doc Register an observer pid.
-spec observe(event(), pid()) -> ok | {error, term()}.
observe(Event, ObserverPid) when is_pid(ObserverPid) ->
    observe(Event, ObserverPid, ObserverPid).

%% @doc Register an observer with an owner pid.
-spec observe(event(), observer(), pid()) -> ok | {error, term()}.
observe(Event, Observer, OwnerPid) ->
    observe(?DEFAULT_NOTIFIER, Event, Observer, OwnerPid, prio(Observer)).

%% @doc Register an observer with the default notifier. Higher prio (lower nr) gets called earlier.
-spec observe(event(), observer(), pid(), integer()) -> ok | {error, term()}.
observe(Event, Observer, OwnerPid, Prio) ->
    observe(?DEFAULT_NOTIFIER, Event, Observer, OwnerPid, Prio).

%% @doc Register an observer. Higher prio (lower nr) gets called earlier.
-spec observe(notifier(), event(), observer(), pid(), integer()) -> ok | {error, term()}.
observe(Notifier, Event, Observer, OwnerPid, Prio) when is_pid(OwnerPid), is_integer(Prio) ->
    zotonic_notifier_worker:observe(Notifier, Event, Observer, OwnerPid, Prio).

%% @doc Unsubscribe an owner from an event
-spec detach(event(), pid()) -> ok | {error, term()}.
detach(Event, OwnerPid) when is_pid(OwnerPid) ->
    detach(?DEFAULT_NOTIFIER, Event, OwnerPid).

%% @doc Unsubscribe an owner from an event
-spec detach(notifier(), event(), pid()) -> ok | {error, term()}.
detach(Notifier, Event, OwnerPid) when is_pid(OwnerPid) ->
    zotonic_notifier_worker:detach(Notifier, Event, OwnerPid).

%% @doc Detach all observers owned by the pid
-spec detach_all(pid()) -> ok.
detach_all(OwnerPid) ->
    detach_all(?DEFAULT_NOTIFIER, OwnerPid).

%% @doc Detach all observers owned by the pid
-spec detach_all(notifier(), pid()) -> ok.
detach_all(Notifier, OwnerPid) when is_pid(OwnerPid) ->
    zotonic_notifier_worker:detach_all(Notifier, OwnerPid).

%% @doc Return the list of all observers for a specific event
-spec get_observers(event()) -> list().
get_observers(Event) ->
    get_observers(?DEFAULT_NOTIFIER, Event).

-spec get_observers(notifier(), event()) -> list().
get_observers(Notifier, Event) ->
    zotonic_notifier_worker:get_observers(Notifier, Event).


%% @doc Notify observers. Pids async, M:F sync.
-spec notify_sync(event(), term(), term()) -> ok | {error, term()}.
notify_sync(Event, Msg, ContextArg) ->
    notify_sync(?DEFAULT_NOTIFIER, Event, Msg, ContextArg).

%% @doc Notify observers. Pids async, M:F sync.
-spec notify_sync(notifier(), event(), term(), term()) -> ok | {error, term()}.
notify_sync(Notifier, Event, Msg, ContextArg) ->
    zotonic_notifier_worker:notify_sync(Notifier, Event, Msg, ContextArg).


%% @doc Notify observers async. Start separate process for the notification.
-spec notify(event(), term(), term()) -> ok | {error, term()}.
notify(Event, Msg, ContextArg) ->
    notify(?DEFAULT_NOTIFIER, Event, Msg, ContextArg).

%% @doc Notify observers async. Start separate process for the notification.
-spec notify(notifier(), event(), term(), term()) -> ok | {error, term()}.
notify(Notifier, Event, Msg, ContextArg) ->
    zotonic_notifier_worker:notify_async(Notifier, Event, Msg, ContextArg).


%% @doc Notify the first observer. Pids async, M:F sync.
-spec notify1(event(), term(), term()) -> ok | {error, term()}.
notify1(Event, Msg, ContextArg) ->
    notify1(?DEFAULT_NOTIFIER, Event, Msg, ContextArg).

%% @doc Notify the first observer. Pids async, M:F sync.
-spec notify1(notifier(), event(), term(), term()) -> ok | {error, term()}.
notify1(Notifier, Event, Msg, ContextArg) ->
    zotonic_notifier_worker:notify1(Notifier, Event, Msg, ContextArg).

%% @doc Return the result of the first observer returning something else than 'undefined'.
%%      Return 'undefined' if none.
-spec first(event(), term(), term()) -> term() | undefined.
first(Event, Msg, ContextArg) ->
    first(?DEFAULT_NOTIFIER, Event, Msg, ContextArg).

%% @doc Return the result of the first observer returning something else than 'undefined'.
%%      Return 'undefined' if none.
-spec first(notifier(), event(), term(), term()) -> term() | undefined.
first(Notifier, Event, Msg, ContextArg) ->
    zotonic_notifier_worker:first(Notifier, Event, Msg, ContextArg).


%% @doc Call all observers, return a list of all return values.
-spec map(event(), term(), term()) -> list( term() ).
map(Event, Msg, ContextArg) ->
    map(?DEFAULT_NOTIFIER, Event, Msg, ContextArg).

-spec map(notifier(), event(), term(), term()) -> list( term() ).
map(Notifier, Event, Msg, ContextArg) ->
    zotonic_notifier_worker:map(Notifier, Event, Msg, ContextArg).


-spec foldl(event(), term(), term(), term()) -> term().
foldl(Event, Msg, Value, ContextArg) ->
    foldl(?DEFAULT_NOTIFIER, Event, Msg, Value, ContextArg).

-spec foldl(notifier(), event(), term(), term(), term()) -> term().
foldl(Notifier, Event, Msg, Value, ContextArg) ->
    zotonic_notifier_worker:foldl(Notifier, Event, Msg, Value, ContextArg).

-spec foldr(event(), term(), term(), term()) -> term().
foldr(Event, Msg, Value, ContextArg) ->
    foldr(?DEFAULT_NOTIFIER, Event, Msg, Value, ContextArg).

-spec foldr(notifier(), event(), term(), term(), term()) -> term().
foldr(Notifier, Event, Msg, Value, ContextArg) ->
    zotonic_notifier_worker:foldr(Notifier, Event, Msg, Value, ContextArg).


-spec await(atom()|tuple()) ->
        {ok, tuple()|atom()} |
        {ok, {pid(), reference()}, tuple()|atom()} |
        {error, timeout}.
await(Event) ->
    await(?DEFAULT_NOTIFIER, Event, Event, 5000).

-spec await(term(), atom()|tuple()) ->
        {ok, tuple()|atom()} |
        {ok, {pid(), reference()}, tuple()|atom()} |
        {error, timeout}.
await(Event, Msg) ->
    await(?DEFAULT_NOTIFIER, Event, Msg, 5000).

-spec await(term(), atom()|tuple(), pos_integer()) ->
        {ok, tuple()|atom()} |
        {ok, {pid(), reference()}, tuple()|atom()} |
        {error, timeout}.
await(Event, Msg, Timeout) ->
    await(?DEFAULT_NOTIFIER, Event, Msg, Timeout).

-spec await(notifier(), term(), atom()|tuple(), pos_integer()) ->
        {ok, tuple()|atom()} |
        {ok, {pid(), reference()}, tuple()|atom()} |
        {error, timeout}.
await(Notifier, Event, Msg, Timeout) when is_atom(Event); is_tuple(Event) ->
    zotonic_notifier_worker:await(Notifier, Event, Msg, Timeout).


-spec await_exact(event(), term()) ->
        {ok, term()} |
        {ok, {pid(), reference()}, term()} |
        {error, timeout}.
await_exact(Event, Msg) ->
    await_exact(?DEFAULT_NOTIFIER, Event, Msg, 5000).

-spec await_exact(event(), term(), pos_integer()) ->
        {ok, term()} |
        {ok, {pid(), reference()}, term()} |
        {error, timeout}.
await_exact(Event, Msg, Timeout) ->
    await_exact(?DEFAULT_NOTIFIER, Event, Msg, Timeout).

-spec await_exact(notifier(), event(), term(), pos_integer()) ->
        {ok, term()} |
        {ok, {pid(), reference()}, term()} |
        {error, timeout}.
await_exact(Notifier, Event, Msg, Timeout) ->
    zotonic_notifier_worker:await_exact(Notifier, Event, Msg, Timeout).


%%====================================================================
%% Internal functions
%%====================================================================

-spec prio(observer()) -> integer().
prio({M, _F}) -> module_prio(M);
prio({M, _F, _A}) -> module_prio(M);
prio(Pid) when is_pid(Pid) -> ?NOTIFIER_DEFAULT_PRIORITY.

-spec module_prio(module()) -> integer().
module_prio(Module) ->
    try
        Info = erlang:get_module_info(Module, attributes),
        case proplists:get_value(mod_prio, Info) of
            [Prio] when is_integer(Prio) -> Prio;
            _ -> ?NOTIFIER_DEFAULT_PRIORITY
        end
    catch
        _M:_E -> ?NOTIFIER_DEFAULT_PRIORITY
    end.


-spec ensure_started(atom()) -> ok | {error, term()}.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {not_started, Dep}} ->
            case ensure_started(Dep) of
                ok -> ensure_started(App);
                {error, _} = Error -> Error
            end;
        {error, {already_started, App}} ->
            ok;
        {error, {Tag, Msg}} when is_list(Tag), is_list(Msg) ->
            {error, lists:flatten(io_lib:format("~s: ~s", [Tag, Msg]))};
        {error, {bad_return, {{M, F, Args}, Return}}} ->
            A = string:join([io_lib:format("~p", [A])|| A <- Args], ", "),
            {error, lists:flatten(
                        io_lib:format("~s failed to start due to a bad return value from call ~s:~s(~s):~n~p",
                                      [App, M, F, A, Return]))};
        {error, Reason} ->
            {error, Reason}
    end.
