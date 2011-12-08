%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010  Marc Worrell
%%
%% @doc Simple caching server with dependency checks and local in process memoization of lookups.

%% Copyright 2009-2010 Marc Worrell
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

-module(z_depcache).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% depcache exports
-export([set/3, set/4, set/5, get/2, get_wait/2, get/3, get_subkey/3, flush/2, flush/1, tick/1, size/1]).
-export([memo/2, memo/3, memo/4, memo/5]).
-export([in_process/0, in_process/1, flush_process_dict/0]).

%% internal export
-export([cleanup/4, cleanup/8]).

-include_lib("zotonic.hrl").

-record(state, {now, serial, meta_table, deps_table, data_table, wait_pids}).
-record(meta,  {key, expire, serial, depend}).
-record(depend,{key, serial}).

start_link(SiteProps) -> 
    Host = proplists:get_value(host, SiteProps),
    gen_server:start_link({local, z_utils:name_for_host(?MODULE, Host)}, ?MODULE, SiteProps, []).


-define(META_TABLE, z_depcache_meta).
-define(DEPS_TABLE, z_depcache_deps).
-define(DATA_TABLE, z_depcache_data).


%% Default max size of the stored data in the depcache before the gc kicks in.
-define(MEMORY_MAX, 100*1024*1024).

% Maximum time to wait for a get_wait/2 call before a timout failure (in secs).
-define(MAX_GET_WAIT, 30).

% Number of slots visited for each gc iteration
-define(CLEANUP_BATCH, 100).

% Number of entries we keep in the local process dictionary for fast lookups
-define(PROCESS_DICT_THRESHOLD, 10000).



memo(Function, #context{} = Context) ->
    memo(Function, undefined, ?HOUR, [], Context).

memo(Function, MaxAge, #context{} = Context) when is_tuple(Function) ->
    memo(Function, undefined, MaxAge, [], Context);

memo(F, Key, Context) when is_function(F) ->
    memo(F, Key, ?HOUR, [], Context).

memo(F, Key, MaxAge, #context{} = Context) ->
    memo(F, Key, MaxAge, [], Context).

memo(F, Key, MaxAge, Dep, #context{} = Context) ->
    Key1 = case Key of
        undefined -> memo_key(F);
        _ -> Key
    end,
    case ?MODULE:get_wait(Key1, Context) of
        {ok, Value} ->
            Value;
        undefined ->
            try
                Value = case F of
                    {M,F,A} -> erlang:apply(M,F,A);
                    {M,F} -> M:F();
                    F when is_function(F) -> F()
                end,
                case MaxAge of
                    0 -> memo_send_replies(Key, Value, Context);
                    _ -> set(Key, Value, MaxAge, Dep, Context)
                end,
                Value
            catch
                _: R ->  memo_send_errors(Key, R, Context)
            end
    end.

    %% @doc Calculate the key used for memo functions.
    memo_key({M,F,A}) -> 
        WithoutContext = lists:filter(fun(#context{}) -> false; (_) -> true end, A),
        {M,F,WithoutContext};
    memo_key({M,F}) -> 
        {M,F}.

    %% @doc Send the calculated value to the processes waiting for the result.
    memo_send_replies(Key, Value, Context) ->
        Pids = get_waiting_pids(Key, Context),
        [ catch gen_server:reply(Pid, {ok, Value}) || Pid <- Pids ],
        ok.

    %% @doc Send an error to the processes waiting for the result.
    memo_send_errors(Key, Reason, Context) ->
        Pids = get_waiting_pids(Key, Context),
        [ catch gen_server:reply(Pid, {error, Reason}) || Pid <- Pids ],
        error.


%% @spec set(Key, Data, Context) -> void()
%% @doc Add the key to the depcache, hold it for 3600 seconds and no dependencies
set(Key, Data, Context) ->
    set(Key, Data, 3600, [], Context).

%% @spec set(Key, Data, MaxAge, Context) -> void()
%% @doc Add the key to the depcache, hold it for MaxAge seconds and no dependencies
set(Key, Data, MaxAge, Context) ->
    set(Key, Data, MaxAge, [], Context).

%% @spec set(Key, Data, MaxAge, Depend, Context) -> void()
%% @doc Add the key to the depcache, hold it for MaxAge seconds and check the dependencies
set(Key, Data, MaxAge, Depend, #context{depcache=Depcache}) ->
    flush_process_dict(),
    gen_server:call(Depcache, {set, Key, Data, MaxAge, Depend}).


%% @spec get_wait(Key, Context) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, when the key does not exist then lock the entry and let 
%% the calling process insert the value. All other processes requesting the key will wait till
%% the key is updated and receive the key's new value.
get_wait(Key, Context) ->
    case get_process_dict(Key, Context) of
        NoValue when NoValue =:= undefined orelse NoValue =:= depcache_disabled ->
            gen_server:call(Context#context.depcache, {get_wait, Key}, ?MAX_GET_WAIT*1000);
        Other ->
            Other
    end.


%% @spec get_waiting_pids(Key, Context) -> {ok, Pids} | undefined
%% @doc Fetch the queue of pids that are waiting for a get_wait/1. This flushes the queue and
%% the key from the depcache.
get_waiting_pids(Key, #context{depcache=Depcache}) ->
    gen_server:call(Depcache, {get_waiting_pids, Key}, ?MAX_GET_WAIT*1000).



%% @spec get(Key, Context) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
get(Key, Context) ->
    case get_process_dict(Key, Context) of
        depcache_disabled -> gen_server:call(Context#context.depcache, {get, Key});
        Value -> Value
    end.


%% @spec get_subkey(Key, SubKey, Context) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
get_subkey(Key, SubKey, Context) ->
    case in_process() of
        true ->
            case erlang:get({depcache, {subkey, Key, SubKey}}) of
                {memo, Value} ->
                    Value;
                undefined ->
                    Value = gen_server:call(Context#context.depcache, {get, Key, SubKey}),
                    erlang:put({depcache, {subkey, Key, SubKey}}, {memo, Value}),
                    Value
            end;
        false ->
            gen_server:call(Context#context.depcache, {get, Key, SubKey})
    end.


%% @spec get(Key, SubKey, Context) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
get(Key, SubKey, Context) ->
    case get_process_dict(Key, Context) of
        undefined -> 
            undefined;
        depcache_disabled ->
            gen_server:call(Context#context.depcache, {get, Key, SubKey});
        {ok, Value} ->
            {ok, find_value(SubKey, Value)}
    end.


%% @spec flush(Key, #context{}) -> void()
%% @doc Flush the key and all keys depending on the key
flush(Key, #context{depcache=Depcache}) ->
    gen_server:call(Depcache, {flush, Key}),
    flush_process_dict().


%% @spec flush(#context{}) -> void()
%% @doc Flush all keys from the caches
flush(#context{depcache=Depcache}) ->
    gen_server:call(Depcache, flush),
    flush_process_dict().


%% @spec tick(#context{}) -> none()
%% @doc Periodic tick used for incrementing the clock
tick(#context{depcache=Depcache}) ->
    gen_server:cast(Depcache, tick).


%% @spec size(#context{}) -> int()
%% @doc Return the total memory size of all stored terms
size(Context) ->
    {_Meta, _Deps, Data} = get_tables(Context),
    ets:info(Data, memory).


%% @doc Fetch the depcache tables.
get_tables(#context{host=Host} = Context) ->
    case erlang:get(depcache_tables) of
        {ok, Host, Tables} ->
            Tables;
        {ok, _OtherHost, _Tables} ->
            flush_process_dict(),
            get_tables1(Context);
        undefined->
            get_tables1(Context)
    end.

    get_tables1(Context) ->
        {ok, Tables} = gen_server:call(Context#context.depcache, get_tables),
        erlang:put(depcache_tables, {ok, Context#context.host, Tables}),
        Tables.


%% @doc Fetch a value from the dependency cache, using the in-process cached tables.
get_process_dict(Key, Context) ->
    case in_process() of
        true ->
            case erlang:get({depcache, Key}) of
                {memo, Value} ->
                    Value;
                undefined ->
                    % Prevent the process dict memo from blowing up the process size
                    case z_utils:now() > erlang:get(depcache_now)
                          orelse erlang:get(depcache_count) > ?PROCESS_DICT_THRESHOLD of
                        true -> flush_process_dict();
                        false -> nop
                    end,

                    Value = get_ets(Key, Context),
                    erlang:put({depcache, Key}, {memo, Value}),
                    erlang:put(depcache_count, incr(erlang:get(depcache_count))),
                    Value
            end;
        false ->
            depcache_disabled
    end.


get_ets(Key, Context) ->
    {MetaTable, DepsTable, DataTable} = get_tables(Context),
    case get_concurrent(Key, get_now(), MetaTable, DepsTable, DataTable) of
        flush ->
            flush(Key, Context),
            undefined;
        undefined ->
            undefined;
        {ok, _Value} = Found ->
            Found
    end.


%% @doc Check if we use a local process dict cache
in_process() ->
    erlang:get(depcache_in_process) =:= true.

%% @doc Enable or disable the in-process caching using the process dictionary
in_process(true) ->
    erlang:put(depcache_in_process, true);
in_process(false) ->
    erlang:erase(depache_in_process),
    flush_process_dict();
in_process(undefined) ->
    in_process(false).

%% @doc Flush all items memoized in the process dictionary.
flush_process_dict() ->
    [ erlang:erase({depcache, Key}) || {{depcache, Key},_Value} <- erlang:get() ],
    erlang:erase(depache_now),
    erlang:put(depcache_count, 0),
    ok.


%% @doc Get the current system time in seconds
get_now() ->
    case erlang:get(depcache_now) of
        undefined ->
            Now = z_utils:now(),
            erlang:put(depache_now, Now),
            Now;
        Now ->
            Now
    end.




%% gen_server callbacks

%% @spec init(SiteProps) -> {ok, State}
%% @doc Initialize the depcache.  Creates ets tables for the deps, meta and data.  Spawns garbage collector.
init(SiteProps) ->
    Host      = proplists:get_value(host, SiteProps),
    Depcache  = z_utils:name_for_host(?MODULE, Host),
    case erlang:system_info(otp_release) of
        MinR14B when MinR14B >= "R14B" ->
            MetaTable = ets:new(?META_TABLE, [set, {keypos, 2}, protected, {read_concurrency, true}]),
            DepsTable = ets:new(?DEPS_TABLE, [set, {keypos, 2}, protected, {read_concurrency, true}]),
            DataTable = ets:new(?DATA_TABLE, [set, {keypos, 1}, protected, {read_concurrency, true}]);
        _Older ->
            MetaTable = ets:new(?META_TABLE, [set, {keypos, 2}, protected]),
            DepsTable = ets:new(?DEPS_TABLE, [set, {keypos, 2}, protected]),
            DataTable = ets:new(?DATA_TABLE, [set, {keypos, 1}, protected])
    end,
    State = #state{now=z_utils:now(), serial=0, meta_table=MetaTable, deps_table=DepsTable, data_table=DataTable, wait_pids=dict:new()},
    timer:apply_interval(1000, ?MODULE, tick, [#context{host=Host, depcache=Depcache}]),
    spawn_link(?MODULE, cleanup, [self(), MetaTable, DepsTable, DataTable]),
    {ok, State}.


%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

%% @doc Return the ets tables used by the cache
handle_call(get_tables, _From, State) ->
    {reply, {ok, {State#state.meta_table, State#state.deps_table, State#state.data_table}}, State};

%% @doc Fetch a key from the cache. When the key is not available then let processes wait till the
%% key is available.
handle_call({get_wait, Key}, From, State) ->
    case get_concurrent(Key, State#state.now, State#state.meta_table, State#state.deps_table, State#state.data_table) of
        NotFound when NotFound =:= flush; NotFound =:= undefined ->
            case NotFound of
                flush -> flush_key(Key, State);
                undefined -> State
            end,
            case dict:find(Key, State#state.wait_pids) of
                {ok, {MaxAge,List}} when State#state.now < MaxAge ->
                    %% Another process is already calculating the value, let the caller wait.
                    WaitPids = dict:store(Key, {MaxAge, [From|List]}, State#state.wait_pids),
                    {noreply, State#state{wait_pids=WaitPids}};
                _ ->
                    %% Nobody waiting or we hit a timeout, let next requestors wait for this caller.
                    WaitPids = dict:store(Key, {State#state.now+?MAX_GET_WAIT, []}, State#state.wait_pids),
                    {reply, undefined, State#state{wait_pids=WaitPids}}
            end;
        {ok, _Value} = Found -> 
            {reply, Found, State}
    end;

%% @doc Return the list of processes waiting for the rendering of a key. Flush the queue and the key.
handle_call({get_waiting_pids, Key}, _From, State) ->
    {State1, Pids} = case dict:find(Key, State#state.wait_pids) of
                        {ok, {_MaxAge, List}} -> 
                            WaitPids = dict:erase(Key, State#state.wait_pids),
                            {State#state{wait_pids=WaitPids}, List};
                        error ->
                            {State, []}
                     end,
    flush_key(Key, State1),
    {reply, Pids, State1};


%% @doc Fetch a key from the cache, returns undefined when not found.
handle_call({get, Key}, _From, State) ->
    {reply, get_in_depcache(Key, State), State};

%% @doc Fetch a subkey from a key from the cache, returns undefined when not found.
%% This is useful when the cached data is very large and the fetched data is small in comparison.
handle_call({get, Key, SubKey}, _From, State) ->
    case get_in_depcache(Key, State) of
        undefined -> {reply, undefined, State};
        {ok, Value} -> {reply, {ok, find_value(SubKey, Value)}, State}
    end;

%% Add an entry to the cache table
handle_call({set, Key, Data, MaxAge, Depend}, _From, State) ->
    z_utils:erase_process_dict(),
    State1 = State#state{serial=State#state.serial+1},
    case MaxAge of
        0 ->
            ets:delete(State1#state.meta_table, Key),
            ets:delete(State1#state.data_table, Key);
        _ ->
            ets:insert(State1#state.data_table, {Key, Data}),
            ets:insert(State1#state.meta_table, #meta{key=Key, expire=State1#state.now+MaxAge, serial=State1#state.serial, depend=Depend})
    end,
    
    %% Make sure all dependency keys are available in the deps table
    AddDepend = fun(D) -> 
                    ets:insert_new(State#state.deps_table, #depend{key=D, serial=State1#state.serial})
                end,
    lists:foreach(AddDepend, Depend),
    
    %% Flush the key itself in the dependency table - this will invalidate depending items
    case is_simple_key(Key) of
        true  -> ok;
        false -> ets:insert(State1#state.deps_table, #depend{key=Key, serial=State#state.serial})
    end,

    %% Check if other processes are waiting for this key, send them the data
    case dict:find(Key, State1#state.wait_pids) of
        {ok, {_MaxAge, List}} ->
            [ catch gen_server:reply(Pid, {ok, Data}) || Pid <- List ],
            WaitPids = dict:erase(Key, State1#state.wait_pids),
            {reply, ok, State1#state{wait_pids=WaitPids}};
        error ->
            {reply, ok, State1}
    end;

handle_call({flush, Key}, _From, State) ->
    flush_key(Key, State),
    {reply, ok, State};

handle_call(flush, _From, State) ->
    ets:delete_all_objects(State#state.data_table),
    ets:delete_all_objects(State#state.meta_table),
    ets:delete_all_objects(State#state.deps_table),
    z_utils:erase_process_dict(),
    {reply, ok, State}.



%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({flush, Key}, State) ->
    flush_key(Key, State),
    {noreply, State};

handle_cast(flush, State) ->
    ets:delete_all_objects(State#state.data_table),
    ets:delete_all_objects(State#state.meta_table),
    ets:delete_all_objects(State#state.deps_table),
    z_utils:erase_process_dict(),
    {noreply, State};

handle_cast(tick, State) ->
    z_utils:erase_process_dict(),
    z_utils:flush_message({'$gen_cast', tick}),
    {noreply, State#state{now=z_utils:now()}};

handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info(_Msg, State) -> 
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%% @doc Fetch a value, from within the depcache process.  Cache the value in the process dictionary of the depcache.
get_in_depcache(Key, State) ->
    case erlang:get({depcache, Key}) of
        {memo, Value} ->
            Value;
        undefined ->
            % Prevent the process dict memo from blowing up the process size
            case erlang:get(depcache_count) > ?PROCESS_DICT_THRESHOLD of
                true -> z_utils:erase_process_dict();
                false -> nop
            end,
            Value = get_in_depcache_ets(Key, State),
            erlang:put({depcache, Key}, {memo, Value}),
            erlang:put(depcache_count, incr(erlang:get(depcache_count))),
            Value
    end.
    
incr(undefined) -> 1;
incr(N) -> N+1.

get_in_depcache_ets(Key, State) ->
    case get_concurrent(Key, State#state.now, State#state.meta_table, State#state.deps_table, State#state.data_table) of
        flush -> 
            flush_key(Key, State),
            undefined;
        undefined ->
            undefined;
        {ok, Value} ->
            {ok, Value}
    end.


%% @doc Get a value from the depache.  Called by the depcache and other processes.
%% @spec get_concurrent(term(), now:int(), tid(), tid(), tid()) -> {ok, term()} | undefined | flush
get_concurrent(Key, Now, MetaTable, DepsTable, DataTable) ->
    case ets:lookup(MetaTable, Key) of
        [] -> 
            undefined;
        [#meta{serial=Serial, expire=Expire, depend=Depend}] ->
            %% Check expiration
            case Expire >= Now of
                false -> 
                    flush;
                true ->
                    %% Check dependencies
                    case check_depend(Serial, Depend, DepsTable) of
                        true ->
                            case ets:lookup(DataTable, Key) of
                                [] -> undefined;
                                [{_Key,Data}] -> {ok, Data}
                            end;
                        false ->
                            flush
                    end
            end
    end.



%% @doc Check if a key is usable as dependency key.  That is a string, atom, integer etc, but not a list of lists.
is_simple_key([]) ->
    true;
is_simple_key([H|_]) -> 
    not is_list(H);
is_simple_key(_Key) ->
    true.


%% @doc Flush a key from the cache, reset the in-process cache as well (we don't know if any cached value had a dependency)
flush_key(Key, State) ->
    ets:delete(State#state.data_table, Key),
    ets:delete(State#state.deps_table, Key),
    ets:delete(State#state.meta_table, Key),
    z_utils:erase_process_dict().


%% @doc Check if all dependencies are still valid, that is they have a serial before or equal to the serial of the entry
check_depend(_Serial, [], _DepsTable) ->
    true;
check_depend(Serial, Depend, DepsTable) ->
    CheckDepend = fun
                        (_Dep,false) -> false;
                        (Dep,true) ->
                            case ets:lookup(DepsTable, Dep) of
                                [#depend{serial=DepSerial}] -> DepSerial =< Serial;
                                _ -> false
                            end
                    end,
    lists:foldl(CheckDepend, true, Depend).



% Index of list with an integer like "a[2]"
find_value(Key, L) when is_integer(Key) andalso is_list(L) ->
    try
        lists:nth(Key, L)
    catch
        _:_ -> undefined
    end;
find_value(Key, {GBSize, GBData}) when is_integer(GBSize) ->
    case gb_trees:lookup(Key, {GBSize, GBData}) of
        {value, Val} ->
            Val;
        _ ->
            undefined
    end;

%% Regular proplist lookup
find_value(Key, L) when is_list(L) ->
    proplists:get_value(Key, L);

%% Resource list handling, special lookups when skipping the index
find_value(Key, #rsc_list{list=L}) when is_integer(Key) ->
    try
        lists:nth(Key, L)
    catch
        _:_ -> undefined
    end;
find_value(Key, #rsc_list{list=[H|_T]}) ->
    find_value(Key, H);
find_value(_Key, #rsc_list{list=[]}) ->
    undefined;

% Index of tuple with an integer like "a[2]"
find_value(Key, T) when is_integer(Key) andalso is_tuple(T) ->
    try
        element(Key, T)
    catch 
        _:_ -> undefined
    end;

%% Other cases: context, dict or parametrized module lookup.
find_value(Key, Tuple) when is_tuple(Tuple) ->
    Module = element(1, Tuple),
    case Module of
        dict -> 
            case dict:find(Key, Tuple) of
                {ok, Val} ->
                    Val;
                _ ->
                    undefined
            end;
        Module ->
            Exports = Module:module_info(exports),
            case proplists:get_value(Key, Exports) of
                1 ->
                    Tuple:Key();
                _ ->
                    case proplists:get_value(get, Exports) of
                        1 -> 
                            Tuple:get(Key);
                        _ ->
                            undefined
                    end
            end
    end;

%% Any subvalue of a non-existant value is empty
find_value(_Key, _Data) ->
    undefined.


%% @doc Cleanup process for the depcache.  Periodically checks a batch of depcache items for their validity.
%%      Asks the depcache server to delete invalidated items.  When the load of the data table is too high then
%%      This cleanup process starts to delete random entries.  By using a random delete we don't need to keep
%%      a LRU list, which is a bit expensive.

cleanup(Pid, MetaTable, DepsTable, DataTable) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    ?MODULE:cleanup(Pid, MetaTable, DepsTable, DataTable, 0, z_utils:now(), normal, 0).

%% Wrap around the end of table
cleanup(Pid, MetaTable, DepsTable, DataTable, '$end_of_table', Now, _Mode, Ct) ->
    case ets:info(MetaTable, size) of
        0 -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, DataTable, 0, Now, cleanup_mode(DataTable), 0);
        _ -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, DataTable, 0, Now, cleanup_mode(DataTable), Ct)
    end;

%% In normal cleanup, sleep a second between each batch before continuing our cleanup sweep
cleanup(Pid, MetaTable, DepsTable, DataTable, SlotNr, Now, normal, 0) -> 
    timer:sleep(1000),
    case ets:info(MetaTable, size) of
        0 -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, DataTable, 0, Now, normal, 0);
        _ -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, DataTable, SlotNr, z_utils:now(), cleanup_mode(DataTable), ?CLEANUP_BATCH)
    end;

%% After finishing a batch in cache_full mode, check if the cache is still full, if so keep deleting entries
cleanup(Pid, MetaTable, DepsTable, DataTable, SlotNr, Now, cache_full, 0) -> 
    case cleanup_mode(DataTable) of
        normal     -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, DataTable, SlotNr, Now, normal, 0);
        cache_full -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, DataTable, SlotNr, z_utils:now(), cache_full, ?CLEANUP_BATCH)
    end;

%% Normal cleanup behaviour - check expire stamp and dependencies
cleanup(Pid, MetaTable, DepsTable, DataTable, SlotNr, Now, normal, Ct) ->
    Slot =  try 
                ets:slot(MetaTable, SlotNr)
            catch
                _M:_E -> '$end_of_table'
            end,
    case Slot of
        '$end_of_table' -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, DataTable, '$end_of_table', Now, normal, 0);
        [] -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, DataTable, SlotNr+1, Now, normal, Ct-1);
        Entries ->
            lists:foreach(fun(Meta) -> flush_expired(Meta, Now, DepsTable, Pid) end, Entries),
            ?MODULE:cleanup(Pid, MetaTable, DepsTable, DataTable, SlotNr+1, Now, normal, Ct-1)
    end;

%% Full cache cleanup mode - randomly delete every 10th entry
cleanup(Pid, MetaTable, DepsTable, DataTable, SlotNr, Now, cache_full, Ct) ->
    Slot =  try 
                ets:slot(MetaTable, SlotNr)
            catch
                _M:_E -> '$end_of_table'
            end,
    case Slot of
        '$end_of_table' -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, DataTable, '$end_of_table', Now, cache_full, 0);
        [] -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, DataTable, SlotNr+1, Now, cache_full, Ct-1);
        Entries ->
            FlushExpire = fun (Meta) ->
                                case flush_expired(Meta, Now, DepsTable, Pid) of
                                    ok -> {ok, Meta};
                                    flushed -> flushed
                                end
                           end,
            RandomDelete = fun
                                ({ok, #meta{key=Key}}) ->
                                    case random:uniform(10) of
                                        10 -> ?MODULE:flush(Key);
                                        _  -> ok
                                    end;
                                (flushed) -> flushed
                           end,

            Entries1 = lists:map(FlushExpire, Entries),
            lists:foreach(RandomDelete, Entries1),
            ?MODULE:cleanup(Pid, MetaTable, DepsTable, DataTable, SlotNr+1, Now, cache_full, Ct-1)
    end.


%% @doc Check if an entry is expired, if so delete it
flush_expired(#meta{key=Key, serial=Serial, expire=Expire, depend=Depend}, Now, DepsTable, Pid) ->
    Expired = Expire < Now orelse not check_depend(Serial, Depend, DepsTable),
    case Expired of
        true  -> gen_server:cast(Pid, {flush, Key}), flushed;
        false -> ok
    end.


%% @doc When the data table is too large then we start to randomly delete keys.  It also signals the cleanup process
%% that it needs to be more aggressive, upping its batch size.
%% We use erts_debug:size() on the stored terms to calculate the total size of all terms stored.  This
%% is better than counting the number of entries.  Using the process_info(Pid,memory) is not very useful as the
%% garbage collection still needs to be done and then we delete too many entries.
cleanup_mode(DataTable) ->
    Memory = ets:info(DataTable, memory),
    if 
        Memory >= ?MEMORY_MAX -> cache_full;
        true -> normal
    end.
