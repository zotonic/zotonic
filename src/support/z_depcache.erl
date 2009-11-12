%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009  Marc Worrell
%%
%% @doc Simple caching server with dependency checks

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

-module(z_depcache).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% depcache exports
-export([set/3, set/4, set/5, get/2, get_wait/2, get/3, flush/2, flush/1, tick/1, size/1]).
-export([memo/2, memo/3, memo/4, memo/5]).

%% internal export
-export([cleanup/3, cleanup/7, test/0]).

-include_lib("zotonic.hrl").

-record(state, {now, serial, size=0, meta_table, deps_table, wait_pids}).
-record(meta,  {key, expire, serial, depend}).
-record(depend,{key, serial}).

start_link(SiteProps) -> 
    Host = proplists:get_value(host, SiteProps),
    gen_server:start_link({local, z_utils:name_for_host(?MODULE, Host)}, ?MODULE, SiteProps, []).


-define(META_TABLE, z_depcache_meta).
-define(DEPS_TABLE, z_depcache_deps).

%% Default max size of the stored data in the depcache before the gc kicks in.
-define(MEMORY_MAX, 20*1024*1024).

% Maximum time to wait for a get_wait/2 call before a timout failure (in secs).
-define(MAX_GET_WAIT, 30).

% Number of slots visited for each gc iteration
-define(CLEANUP_BATCH, 100).


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


%% @spec set(Key, Data, MaxAge) -> void()
%% @doc Add the key to the depcache, hold it for 3600 seconds and no dependencies
set(Key, Data, #context{depcache=Depcache}) ->
    Size = erts_debug:flat_size(Data),
    gen_server:call(Depcache, {set, Key, Data, Size, 3600, []}).


%% @spec set(Key, Data, MaxAge) -> void()
%% @doc Add the key to the depcache, hold it for MaxAge seconds and no dependencies
set(Key, Data, MaxAge, #context{depcache=Depcache}) ->
    Size = erts_debug:flat_size(Data),
    gen_server:call(Depcache, {set, Key, Data, Size, MaxAge, []}).


%% @spec set(Key, Data, MaxAge, Depend) -> void()
%% @doc Add the key to the depcache, hold it for MaxAge seconds and check the dependencies
set(Key, Data, MaxAge, Depend, #context{depcache=Depcache}) ->
    Size = erts_debug:flat_size(Data),
    gen_server:call(Depcache, {set, Key, Data, Size, MaxAge, Depend}).


%% @spec get(Key) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
get(Key, #context{depcache=Depcache}) ->
    gen_server:call(Depcache, {get, Key}).

%% @spec get_wait(Key) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, when the key does not exist then lock the entry and let 
%% the calling process insert the value. All other processes requesting the key will wait till
%% the key is updated and receive the key's new value.
get_wait(Key, #context{depcache=Depcache}) ->
    gen_server:call(Depcache, {get_wait, Key}, ?MAX_GET_WAIT*1000).

%% @spec get_waiting_pids(Key) -> {ok, Data} | undefined
%% @doc Fetch the queue of pids that are waiting for a get_wait/1. This flushes the queue and
%% the key from the depcache.
get_waiting_pids(Key, #context{depcache=Depcache}) ->
    gen_server:call(Depcache, {get_waiting_pids, Key}, ?MAX_GET_WAIT*1000).

%% @spec get(Key, SubKey) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
get(Key, SubKey, #context{depcache=Depcache}) ->
    gen_server:call(Depcache, {get, Key, SubKey}).


%% @spec flush(Key) -> void()
%% @doc Flush the key and all keys depending on the key
flush(Key, #context{depcache=Depcache}) ->
    gen_server:cast(Depcache, {flush, Key}).

%% @spec flush() -> void()
%% @doc Flush all keys from the caches
flush(#context{depcache=Depcache}) ->
    gen_server:cast(Depcache, flush).

%% @spec tick() -> none()
%% @doc Periodic tick used for incrementing the clock
tick(#context{depcache=Depcache}) ->
    gen_server:cast(Depcache, tick).


%% @spec size() -> int()
%% @doc Return the total size of all stored terms
size(#context{depcache=Depcache}) ->
    gen_server:call(Depcache, size);
size(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, size).
    

%% gen_server callbacks

%% @spec init(SiteProps) -> {ok, State}
%% @doc Initialize the depcache.  Creates ets tables for the deps and meta data.  Spawns garbage collector.
init(SiteProps) ->
    Host      = proplists:get_value(host, SiteProps),
    Depcache  = z_utils:name_for_host(?MODULE, Host),
    MetaTable = z_utils:name_for_host(?META_TABLE, Host),
    DepsTable = z_utils:name_for_host(?DEPS_TABLE, Host),
    
    ets:new(MetaTable, [set, named_table, {keypos, 2}, protected]),
    ets:new(DepsTable, [set, named_table, {keypos, 2}, protected]),
    State = #state{now=z_utils:now(), serial=0, meta_table=MetaTable, deps_table=DepsTable, wait_pids=dict:new()},
    timer:apply_interval(1000, ?MODULE, tick, [#context{host=Host, depcache=Depcache}]),
    spawn_link(?MODULE, cleanup, [self(), MetaTable, DepsTable]),
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

%% @doc Fetch a key from the cache. When the key is not available then let processes wait till the
%% key is available.
handle_call({get_wait, Key}, From, State) ->
	case handle_call({get, Key}, From, State) of
		{reply, undefined, State1} ->
			case dict:find(Key, State1#state.wait_pids) of
				{ok, {MaxAge,List}} when State#state.now < MaxAge ->
					%% Another process is already calculating the value, let the caller wait.
					WaitPids = dict:store(Key, {MaxAge, [From|List]}, State1#state.wait_pids),
					{noreply, State1#state{wait_pids=WaitPids}};
				error ->
					%% Nobody waiting or we hit a timeout, let next requestors wait for this caller.
					WaitPids = dict:store(Key, {State#state.now+?MAX_GET_WAIT, []}, State1#state.wait_pids),
					{reply, undefined, State1#state{wait_pids=WaitPids}}
			end;
		Other -> 
			Other
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
	{reply, Pids, flush_key(Key, State1)};

%% @doc Fetch a key from the cache, returns undefined when not found.
handle_call({get, Key}, _From, State) ->
    %% Find the key in the data table
    case ets:lookup(State#state.meta_table, Key) of
        [] -> 
            {reply, undefined, State};
        [#meta{serial=Serial, expire=Expire, depend=Depend}] ->
            %% Check expiration
            case Expire >= State#state.now of
                false -> 
                    ets:delete(State#state.meta_table, Key),
                    State1 = erase_key(Key, State),
                    {reply, undefined, State1};
                true ->
                    %% Check dependencies
                    case check_depend(Serial, Depend, State#state.deps_table) of
                        true ->
                            case erlang:get(Key) of
                                {ok, _, Data} -> {reply, {ok, Data}, State};
                                _ -> {reply, undefined, State}
                            end;
                        false ->
                            ets:delete(State#state.meta_table, Key),
                            State1 = erase_key(Key, State),
                            {reply, undefined, State1}
                    end
            end
    end;

%% @doc Fetch a subkey from a key from the cache, returns undefined when not found.
%% This is useful when the cached data is very large and the fetched data is small in comparison.
handle_call({get, Key, SubKey}, From, State) ->
    {reply, Data, State1} = handle_call({get, Key}, From, State),
    case Data of
        {ok, Value} ->
            {reply, {ok, find_value(SubKey, Value)}, State1};
        _ ->
            {reply, Data, State1}
    end;


%% @doc Return the size of all stored terms
handle_call(size, _From, State) ->
    {reply, State#state.size, State};


%% Add an entry to the cache table
handle_call({set, Key, Data, Size, MaxAge, Depend}, _From, State) ->
    %% On every update we inc the serial, flushing depending items
    State1 = State#state{serial=State#state.serial+1},
    
    %% Make sure all dependency keys are available in the deps table
    AddDepend = fun(D) -> 
                    ets:insert_new(State#state.deps_table, #depend{key=D, serial=State1#state.serial})
                end,
    lists:foreach(AddDepend, Depend),
    
    %% Flush the key itself in the dependency table - this will invalidate depending items
    case is_simple_key(Key) of
        true  -> ok;
        false -> ets:insert(State#state.deps_table, #depend{key=Key, serial=State#state.serial})
    end,

    %% Insert the record into the cache table, increment load counter.
	State2 = case MaxAge of
		0 ->
			ets:delete(State1#state.meta_table, Key),
			erase_key(Key, State);
		_ ->
			OldSize = case erlang:put(Key, {ok, Size, Data}) of {ok, Sz, _} -> Sz; _ -> 0 end,
    		ets:insert(State1#state.meta_table, #meta{key=Key, expire=State1#state.now+MaxAge, serial=State1#state.serial, depend=Depend}),
			State1#state{size=State1#state.size + Size - OldSize}
	end,

	%% Check if other processes are waiting for this key, send them the data
	case dict:find(Key, State2#state.wait_pids) of
		{ok, {_MaxAge, List}} ->
			[ catch gen_server:reply(Pid, {ok, Data}) || Pid <- List ],
			WaitPids = dict:erase(Key, State2#state.wait_pids),
			{reply, ok, State2#state{wait_pids=WaitPids}};
		error ->
			{reply, ok, State2}
	end.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({flush, Key}, State) ->
    {noreply, flush_key(Key, State)};

handle_cast(flush, State) ->
    ets:delete_all_objects(State#state.meta_table),
    ets:delete_all_objects(State#state.deps_table),
    erlang:erase(),
    {noreply, State#state{size=0}};

handle_cast(tick, State) ->
    {noreply, State#state{now=z_utils:now()}};

handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info(_Msg, State) -> 
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.



%% @doc Check if a key is usable as dependency key.  That is a string, atom, integer etc, but not a list of lists.
is_simple_key([]) ->
    true;
is_simple_key([H|_]) -> 
    not is_list(H);
is_simple_key(_Key) ->
    true.


%% @doc Erase the key from the process dictionary, substract the size from the accumulator.
erase_key(Key, State) ->
    Data = erlang:erase(Key),
    case Data of
        {ok, Size, _} ->
            State#state{size = State#state.size - Size};
        undefined ->
            State
    end.

%% @doc Flush a key from the cache
flush_key(Key, State) ->
	ets:delete(State#state.deps_table, Key),
	ets:delete(State#state.meta_table, Key),
	erase_key(Key, State).


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

cleanup(Pid, MetaTable, DepsTable) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    ?MODULE:cleanup(Pid, MetaTable, DepsTable, 0, z_utils:now(), normal, 0).

%% Wrap around the end of table
cleanup(Pid, MetaTable, DepsTable, '$end_of_table', Now, _Mode, Ct) ->
    case ets:info(MetaTable, size) of
        0 -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, 0, Now, cleanup_mode(Pid), 0);
        _ -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, 0, Now, cleanup_mode(Pid), Ct)
    end;

%% In normal cleanup, sleep a second between each batch before continuing our cleanup sweep
cleanup(Pid, MetaTable, DepsTable, SlotNr, Now, normal, 0) -> 
    timer:sleep(1000),
    case ets:info(MetaTable, size) of
        0 -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, 0, Now, normal, 0);
        _ -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, SlotNr, z_utils:now(), cleanup_mode(Pid), ?CLEANUP_BATCH)
    end;

%% After finishing a batch in cache_full mode, check if the cache is still full, if so keep deleting entries
cleanup(Pid, MetaTable, DepsTable, SlotNr, Now, cache_full, 0) -> 
    case cleanup_mode(Pid) of
        normal     -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, SlotNr, Now, normal, 0);
        cache_full -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, SlotNr, z_utils:now(), cache_full, ?CLEANUP_BATCH)
    end;

%% Normal cleanup behaviour - check expire stamp and dependencies
cleanup(Pid, MetaTable, DepsTable, SlotNr, Now, normal, Ct) ->
    Slot =  try 
                ets:slot(MetaTable, SlotNr)
            catch
                _M:_E -> '$end_of_table'
            end,
    case Slot of
        '$end_of_table' -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, '$end_of_table', Now, normal, 0);
        [] -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, SlotNr+1, Now, normal, Ct-1);
        Entries ->
            lists:foreach(fun(Meta) -> flush_expired(Meta, Now, DepsTable, Pid) end, Entries),
            ?MODULE:cleanup(Pid, MetaTable, DepsTable, SlotNr+1, Now, normal, Ct-1)
    end;

%% Full cache cleanup mode - randomly delete every 10th entry
cleanup(Pid, MetaTable, DepsTable, SlotNr, Now, cache_full, Ct) ->
    Slot =  try 
                ets:slot(MetaTable, SlotNr)
            catch
                _M:_E -> '$end_of_table'
            end,
    case Slot of
        '$end_of_table' -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, '$end_of_table', Now, cache_full, 0);
        [] -> ?MODULE:cleanup(Pid, MetaTable, DepsTable, SlotNr+1, Now, cache_full, Ct-1);
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
            ?MODULE:cleanup(Pid, MetaTable, DepsTable, SlotNr+1, Now, cache_full, Ct-1)
    end.


%% @doc Check if an entry is expired, if so delete it
flush_expired(#meta{key=Key, serial=Serial, expire=Expire, depend=Depend}, Now, DepsTable, Pid) ->
    Expired = Expire < Now orelse not check_depend(Serial, Depend, DepsTable),
    case Expired of
        true  -> gen_server:cast(Pid, {flush, Key});
        false -> ok
    end.


%% @doc When the data table is too large then we start to randomly delete keys.  It also signals the cleanup process
%% that it needs to be more aggressive, upping its batch size.
%% We use erts_debug:size() on the stored terms to calculate the total size of all terms stored.  This
%% is better than counting the number of entries.  Using the process_info(Pid,memory) is not very useful as the
%% garbage collection still needs to be done and then we delete too many entries.
cleanup_mode(Pid) ->
    Memory = ?MODULE:size(Pid),
    if 
        Memory >= ?MEMORY_MAX -> cache_full;
        true -> normal
    end.



test() ->
	C = z_context:new(default),
	?MODULE:flush(test_m_key, C),
	[ test_m(C) || _N <- lists:seq(1,100) ],
	ok.

	test_m(Context) ->
		?DEBUG(?MODULE:memo(fun test_f/0, test_m_key, Context)).

	test_f() ->
		?DEBUG(waiting),
		receive after 5000 -> y end.
