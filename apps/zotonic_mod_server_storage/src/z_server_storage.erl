%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc Simple data storage in processes.

%% Copyright 2020 Marc Worrell
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

-module(z_server_storage).

-behaviour(gen_server).

-export([
    ping/2,
    stop/2,
    lookup/3,
    store/4,
    delete/3,
    delete/2,
    secure_lookup/3,
    secure_store/4,
    secure_delete/3,
    secure_delete/2
    ]).

-export([
    start_link/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(STORAGE_EXPIRE, 900).

% Max storage size is 1MB - above this writes are refused
-define(STORAGE_SIZE_MAX, 1000000).

-record(state, {
        id :: binary(),
        timeout :: integer(),
        size :: integer(),
        data :: map(),
        secure :: map()
    }).

-spec start_link( binary(), z:context()) -> {ok, pid()} | {error, {already_started, pid()}}.
start_link(SessionId, Context) ->
    gen_server:start_link({via, z_proc, {{?MODULE, SessionId}, Context}}, ?MODULE, [SessionId, timeout(Context)], []).


%%% ------------------------------------------------------------------------------------
%%% API
%%% ------------------------------------------------------------------------------------


-spec lookup( binary(), term(), z:context() ) -> {ok, term()} | {error, not_found | no_session}.
lookup(SessionId, Key, Context) ->
    case z_proc:whereis({?MODULE, SessionId}, Context) of
        undefined -> {error, no_session};
        Pid -> gen_server:call(Pid, {lookup, Key})
    end.

-spec store( binary(), term(), term(), z:context() ) -> ok | {error, no_session | full}.
store(SessionId, Key, Value, Context) ->
    case z_proc:whereis({?MODULE, SessionId}, Context) of
        undefined -> {error, no_session};
        Pid -> gen_server:call(Pid, {store, Key, Value})
    end.

-spec delete( binary(), term(), z:context() ) -> ok | {error, no_session}.
delete(SessionId, Key, Context) ->
    case z_proc:whereis({?MODULE, SessionId}, Context) of
        undefined -> {error, no_session};
        Pid -> gen_server:cast(Pid, {delete, Key})
    end.

-spec delete( binary(), z:context() ) -> ok | {error, no_session}.
delete(SessionId, Context) ->
    case z_proc:whereis({?MODULE, SessionId}, Context) of
        undefined -> {error, no_session};
        Pid -> gen_server:cast(Pid, delete)
    end.

-spec secure_lookup( binary(), term(), z:context() ) -> {ok, term()} | {error, not_found | no_session}.
secure_lookup(SessionId, Key, Context) ->
    case z_proc:whereis({?MODULE, SessionId}, Context) of
        undefined -> {error, no_session};
        Pid -> gen_server:call(Pid, {secure_lookup, Key})
    end.

-spec secure_store( binary(), term(), term(), z:context() ) -> ok | {error, no_session}.
secure_store(SessionId, Key, Value, Context) ->
    case z_proc:whereis({?MODULE, SessionId}, Context) of
        undefined -> {error, no_session};
        Pid -> gen_server:cast(Pid, {secure_store, Key, Value})
    end.

-spec secure_delete( binary(), term(), z:context() ) -> ok | {error, no_session}.
secure_delete(SessionId, Key, Context) ->
    case z_proc:whereis({?MODULE, SessionId}, Context) of
        undefined -> {error, no_session};
        Pid -> gen_server:cast(Pid, {secure_delete, Key})
    end.

-spec secure_delete( binary(), z:context() ) -> ok | {error, no_session}.
secure_delete(SessionId, Context) ->
    case z_proc:whereis({?MODULE, SessionId}, Context) of
        undefined -> {error, no_session};
        Pid -> gen_server:cast(Pid, secure_delete)
    end.

-spec ping( binary(), z:context() ) -> ok | {error, no_session}.
ping(SessionId, Context) ->
    case z_proc:whereis({?MODULE, SessionId}, Context) of
        undefined -> {error, no_session};
        Pid ->
            case erlang:is_process_alive(Pid) of
                true -> gen_server:cast(Pid, ping);
                false -> {error, no_session}
            end
    end.

-spec stop( binary(), z:context() ) -> ok | {error, no_session}.
stop(SessionId, Context) ->
    case z_proc:whereis({?MODULE, SessionId}, Context) of
        undefined -> {error, no_session};
        Pid -> gen_server:cast(Pid, stop)
    end.


%%% ------------------------------------------------------------------------------------
%%% gen_server callbacks
%%% ------------------------------------------------------------------------------------

init([SessionId, Timeout]) ->
    {ok, #state{
        id = SessionId,
        timeout = Timeout * 1000,
        data = #{},
        secure = #{}
    }, Timeout}.

handle_call({lookup, Key}, _From, #state{ data = Data } = State) ->
    case maps:find(Key, Data) of
        {ok, Value} ->
            {reply, {ok, Value}, State, State#state.timeout};
        error ->
            {reply, {error, not_found}, State, State#state.timeout}
    end;
handle_call({secure_lookup, Key}, _From, #state{ secure = Data } = State) ->
    case maps:find(Key, Data) of
        {ok, Value} ->
            {reply, {ok, Value}, State, State#state.timeout};
        error ->
            {reply, {error, not_found}, State, State#state.timeout}
    end;
handle_call({store, Key, Value}, _From, #state{ data = Data, size = Size } = State) ->
    OldKVSize = kv_size(Key, maps:find(Key, Data)),
    NewKVSize = kv_size(Key, {ok, Data}),
    AfterSize = Size - OldKVSize + NewKVSize,
    if
        AfterSize > ?STORAGE_SIZE_MAX ->
            {reply, {error, full}, State, State#state.timeout};
        true ->
            State1 = State#state{
                size = AfterSize,
                data = Data#{ Key => Value }
            },
            {reply, ok, State1, State#state.timeout}
    end.

handle_cast({delete, Key}, #state{ data = Data, size = Size } = State) ->
    OldKVSize = kv_size(Key, maps:find(Key, Data)),
    State1 = State#state{
        size = Size - OldKVSize,
        data = maps:remove(Key, Data)
    },
    {noreply, State1, State#state.timeout};
handle_cast(delete, #state{} = State) ->
    State1 = State#state{
        size = 0,
        data = #{}
    },
    {noreply, State1, State#state.timeout};

handle_cast({secure_store, Key, Value}, #state{ secure = Data } = State) ->
    State1 = State#state{ secure = Data#{ Key => Value } },
    {noreply, State1, State#state.timeout};
handle_cast({secure_delete, Key}, #state{ secure = Data } = State) ->
    State1 = State#state{ secure = maps:remove(Key, Data) },
    {noreply, State1, State#state.timeout};
handle_cast(secure_delete, #state{} = State) ->
    State1 = State#state{ secure = #{} },
    {noreply, State1, State#state.timeout};

handle_cast(ping, State) ->
    {noreply, State, State#state.timeout};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(timeout, State) ->
    {stop, normal, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%%% ------------------------------------------------------------------------------------
%%% support
%%% ------------------------------------------------------------------------------------

timeout(Context) ->
    case m_config:get_value(mod_server_storage, storage_expire, ?STORAGE_EXPIRE, Context) of
        <<>> -> ?STORAGE_EXPIRE;
        undefined -> ?STORAGE_EXPIRE;
        V -> z_convert:to_integer(V)
    end.

kv_size(_Key, error) -> 0;
kv_size(Key, {ok, V}) -> term_size(Key) + term_size(V).

term_size(V) when is_binary(V) -> byte_size(V);
term_size(A) when is_atom(A) -> 4;
term_size(V) -> erlang:external_size(V).
