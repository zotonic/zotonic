%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2014-2024 Arjan Scherpenisse
%% @doc Postgresql pool worker
%% @end

%% Copyright 2014-2024 Arjan Scherpenisse
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

-module(z_db_pgsql).
-behaviour(gen_server).

-behaviour(poolboy_worker).
-behaviour(z_db_worker).

-include("zotonic.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% poolboy_worker callbacks
-export([start_link/1]).

%% z_db_worker callbacks
-export([
    pool_return_connection/2,
    pool_get_connection/1,
    is_connection_alive/1,

    ensure_all_started/0,
    test_connection/1,
    squery/3,
    equery/4,
    execute_batch/4,
    get_raw_connection/1
]).

%% Used by the z_install_update to access props columns
-export([
    decode_value/1
    ]).

-define(TERM_MAGIC_NUMBER, 16#01326A3A:1/big-unsigned-unit:32).

-define(CONNECT_TIMEOUT, 5000).
-define(IDLE_TIMEOUT, 60000).

-define(CONNECT_RETRIES, 50).
-define(CONNECT_RETRY_SHORT,   100).
-define(CONNECT_RETRY_MIDDLE, 1000).
-define(CONNECT_RETRY_SLEEP, 10000).

%% @doc Threshold above which we do an automatic explain of traced queries.
 -define(DBTRACE_EXPLAIN_MSEC, 100).

-record(state, {
    conn = undefined :: undefined | pid(),
    conn_args = undefined :: undefined | list(),
    busy_monitor = undefined :: undefined | reference(),
    busy_pid = undefined :: undefined | pid(),
    busy_ref = undefined :: undefined | reference(),
    busy_timeout = undefined :: undefined | integer(),
    busy_start = undefined :: undefined | pos_integer(),
    busy_sql = undefined :: undefined | string() | binary(),
    busy_params = [] :: list(),
    busy_tracing = false :: boolean()
}).

-type query_result() :: epgsql:reply(epgsql:equery_row())
                      | epgsql:reply(epgsql:squery_row()).

-export_type([ query_result/0 ]).


%%
%% API
%%

start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec test_connection( list() ) -> ok | {error, term()}.
test_connection(Args) ->
    case try_connect_tcp(Args) of
        ok ->
            test_connection_1(Args);
        {error, _} = Error ->
            Error
    end.

ensure_all_started() ->
    application:ensure_all_started(epgsql).

test_connection_1(Args) ->
    case connect(Args) of
        {ok, Conn} ->
            {dbschema, Schema} = proplists:lookup(dbschema, Args),
            case z_db:schema_exists_conn(Conn, Schema) of
                true ->
                    epgsql:close(Conn),
                    ok;
                false ->
                    epgsql:close(Conn),
                    {error, noschema}
            end;
        {error, _} = E ->
            E
    end.

-spec is_connection_alive(Worker) -> boolean() when
    Worker :: pid().
is_connection_alive(Worker) ->
    erlang:is_process_alive(Worker).

-spec pool_get_connection(Context) -> {ok, Conn} | {error, Reason} when
    Context :: z:context(),
    Conn :: pid(),
    Reason :: term().
pool_get_connection(Context) ->
    z_db_pool:get_connection(Context).

-spec pool_return_connection(Worker, Context) -> ok | {error, Reason} when
    Worker :: pid(),
    Context :: z:context(),
    Reason :: term().
pool_return_connection(Worker, Context) ->
    case is_connection_alive(Worker) of
        true ->
            try
                case gen_server:call(Worker, {pool_return_connection_check, self()}) of
                    ok ->
                        z_db_pool:return_connection(Worker, Context);
                    {error, _} = Error ->
                        Error
                end
            catch
                exit:Reason:Stack ->
                    z_context:logger_md(Context),
                    ?LOG_ERROR(#{
                        text => <<"Return connection failed.">>,
                        in => zotonic_core,
                        result => exit,
                        reason => Reason,
                        stack => Stack,
                        worker_pid => Worker
                    }),
                    {error, Reason}
            end;
        false ->
            {error, connection_down}
    end.

%% @doc Simple query without parameters, the query is interrupted if it takes
%%      longer than Timeout msec.
-spec squery(Worker, Sql, Timeout) -> Result when
    Worker :: pid(),
    Sql :: string() | binary(),
    Timeout :: pos_integer(),
    Result :: query_result().
squery(Worker, Sql, Timeout) ->
    case is_connection_alive(Worker) of
        true ->
            case fetch_conn(Worker, Sql, [], Timeout) of
                {ok, {Conn, Ref}} ->
                    Result = epgsql:squery(Conn, Sql),
                    ok = return_conn(Worker, Ref),
                    decode_reply(Result);
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, connection_down}
    end.

%% @doc Query with parameters, the query is interrupted if it takes
%%      longer than Timeout msec.
-spec equery(Worker, Sql, Parameters, Timeout) -> Result when
    Worker :: pid(),
    Sql :: string() | binary(),
    Parameters :: list(),
    Timeout :: pos_integer(),
    Result :: query_result().
equery(Worker, Sql, Parameters, Timeout) ->
    case is_connection_alive(Worker) of
        true ->
            case fetch_conn(Worker, Sql, Parameters, Timeout) of
                {ok, {Conn, Ref}} ->
                    Result = epgsql:equery(Conn, Sql, encode_values(Parameters)),
                    ok = return_conn(Worker, Ref),
                    decode_reply(Result);
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, connection_down}
    end.

%% @doc Batch Query, the query is interrupted if it takes
%%      longer than Timeout msec.
-spec execute_batch(Worker, Sql, Batch, Timeout) -> Result when
    Worker :: pid(),
    Sql :: string() | binary(),
    Batch :: list( list() ),
    Timeout :: pos_integer(),
    Result :: query_result().
execute_batch(Worker, Sql, Batch, Timeout) ->
    case is_connection_alive(Worker) of
        true ->
            case fetch_conn(Worker, Sql, Batch, Timeout) of
                {ok, {Conn, Ref}} ->
                    EncodedBatch = [encode_values(P) || P <- Batch],
                    Result = epgsql:execute_batch(Conn, Sql, EncodedBatch),
                    ok = return_conn(Worker, Ref),
                    decode_reply(Result);
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, connection_down}
    end.

%% @doc Request the SQL connection from the worker. The query is passed for logging
% purposes. This caller will do the query using the returned connection.
-spec fetch_conn(Worker, Sql, Parameters, Timeout) -> {ok, {Conn, Ref}} | {error, Reason} when
    Worker :: pid(),
    Sql :: string() | binary(),
    Parameters :: list(),
    Timeout :: pos_integer(),
    Conn :: pid(),
    Ref :: reference(),
    Reason :: term().
fetch_conn(Worker, Sql, Parameters, Timeout) ->
    case is_connection_alive(Worker) of
        true ->
            try
                Ref = erlang:make_ref(),
                {ok, Conn} = gen_server:call(Worker, {fetch_conn, Ref, self(), Sql, Parameters, Timeout, is_tracing()}),
                {ok, {Conn, Ref}}
            catch
                exit:Reason:Stack ->
                    ?LOG_ERROR(#{
                        text => <<"Fetch connection failed.">>,
                        in => zotonic_core,
                        result => exit,
                        reason => Reason,
                        stack => Stack,
                        worker_pid => Worker,
                        sql => Sql
                    }),
                    {error, Reason}
            end;
        false ->
            {error, connection_down}
    end.

%% @doc Return the SQL connection to the worker, must be done within the timeout
%%      specified in the fetch_conn/4 call.
-spec return_conn(Worker, Ref) -> ok | {error, Reason} when
    Worker :: pid(),
    Ref :: reference(),
    Reason :: term().
return_conn(Worker, Ref) ->
    case is_connection_alive(Worker) of
        true ->
            gen_server:call(Worker, {return_conn, Ref, self()});
        false ->
            {error, connection_down}
    end.


%% @doc Return the tracing flag from the process dictionary.
-spec is_tracing() -> boolean().
is_tracing() ->
    case erlang:get(is_dbtrace) of
        true -> true;
        _ -> false
    end.

%% @doc This function MUST NOT be used, but currently is required by the
%% install / upgrade routines. Can only be called from inside a
%% z_db:transaction/2.
get_raw_connection(#context{dbc=Worker}) when Worker =/= undefined ->
    gen_server:call(Worker, get_raw_connection).


%%
%% gen_server callbacks
%%

init(Args) ->
    %% Start disconnected
    process_flag(trap_exit, true),
    {ok, #state{conn=undefined, conn_args=Args}, ?IDLE_TIMEOUT}.

handle_call({pool_return_connection_check, _CallerPid}, _From, #state{ busy_pid = undefined } = State) ->
    {reply, ok, State};
handle_call({pool_return_connection_check, CallerPid}, From, #state{
            busy_pid = Pid,
            busy_sql = Sql,
            busy_params = Params
        } = State) ->
    ?LOG_ERROR(#{
        text => <<"Connection return to pool by but still running">>,
        in => zotonic_core,
        result => error,
        reason => running,
        request_pid => CallerPid,
        busy_pid => Pid,
        query => Sql,
        args => Params,
        worker_pid => self()
    }),
    gen_server:reply(From, {error, checkin_busy}),
    State1 = disconnect(State),
    {stop, normal, State1};

handle_call({fetch_conn, _Ref, _CallerPid, _Sql, _Params, _Timeout, _IsTracing} = Cmd, From,
            #state{ busy_pid = undefined, conn = undefined, conn_args = Args } = State) ->
    case connect(Args, From) of
        {ok, Conn} ->
            erlang:monitor(process, Conn),
            handle_call(Cmd, From, State#state{conn=Conn});
        {error, _} = E ->
            {reply, E, State}
    end;

handle_call({fetch_conn, Ref, CallerPid, Sql, Params, Timeout, IsTracing}, _From, #state{ busy_pid = undefined } = State) ->
    Start = trace_start(),
    State1 = State#state{
        busy_monitor = erlang:monitor(process, CallerPid),
        busy_pid = CallerPid,
        busy_ref = Ref,
        busy_timeout = Timeout,
        busy_start = Start,
        busy_sql = Sql,
        busy_params = Params,
        busy_tracing = IsTracing
    },
    {reply, {ok, State#state.conn}, State1, Timeout};

handle_call({fetch_conn, _Ref, CallerPid, Sql, Params, _Timeout, _IsTracing}, From, #state{ busy_pid = OtherPid } = State)
    when CallerPid =:= OtherPid ->
    % Caller is confused - starting a request whilst the current request isn't finished yet.
    % Log an error, stop the running query, and kill this worker.
    % No hope of recovery, as the caller is in an illegal state reusing this connection
    % for multiple queries.
    ?LOG_ERROR(#{
        text => <<"Connection requested but already using same connection">>,
        in => zotonic_core,
        result => error,
        reason => busy,
        request_pid => CallerPid,
        query => Sql,
        args => Params,
        worker_pid => self()
    }),
    gen_server:reply(From, {error, busy}),
    State1 = disconnect(State),
    {stop, normal, State1};

handle_call({fetch_conn, _Ref, CallerPid, Sql, Params, _Timeout, _IsTracing}, _From, #state{ busy_pid = OtherPid } = State) ->
    % This can happen if a connection is shared by two processes.
    % Deny the request and continue with the running request.
    ?LOG_ERROR(#{
        text => <<"Connection requested but in use by other pid">>,
        in => zotonic_core,
        result => error,
        reason => busy,
        request_pid => CallerPid,
        busy_pid => OtherPid,
        query => Sql,
        args => Params,
        worker_pid => self()
    }),
    {reply, {error, busy}, State, timeout(State)};

handle_call({return_conn, Ref, Pid}, _From,
        #state{
            busy_monitor = Monitor,
            busy_ref = Ref,
            busy_pid = Pid,
            busy_sql = Sql,
            busy_params = Params,
            busy_start = Start,
            busy_tracing = IsTracing,
            conn = Conn
        } = State) ->
    erlang:demonitor(Monitor),
    trace_end(IsTracing, Start, Sql, Params, Conn),
    State1 = reset_busy_state(State),
    {reply, ok, State1, timeout(State1)};

handle_call({return_conn, _Ref, Pid}, _From, #state{ busy_pid = undefined } = State) ->
    ?LOG_ERROR(#{
        text => <<"SQL connection returned but not in use.">>,
        in => zotonic_core,
        request_pid => Pid
    }),
    {reply, {error, idle}, State, timeout(State)};

handle_call({return_conn, _Ref, Pid}, _From, #state{ busy_pid = OtherPid } = State) ->
    ?LOG_ERROR(#{
        text => <<"SQL connection returned but in use by other pid">>,
        in => zotonic_core,
        result => error,
        reason => busy,
        request_pid => Pid,
        busy_pid => OtherPid,
        worker_pid => self()
    }),
    {reply, {error, notyours}, State, timeout(State)};

handle_call(get_raw_connection, From, #state{ conn = undefined, conn_args = Args } = State) ->
    case connect(Args, From) of
        {ok, Conn} ->
            erlang:monitor(process, Conn),
            handle_call(get_raw_connection, From, State#state{conn=Conn});
        {error, _} = E ->
            {reply, E, State}
    end;
handle_call(get_raw_connection, _From, #state{ conn = Conn } = State) ->
    {reply, Conn, State, timeout(State)};

handle_call(Message, _From, State) ->
    ?LOG_NOTICE(#{
        text => <<"SQL unexpected call message">>,
        in => zotonic_core,
        request => Message,
        worker_pid => self()
    }),
    {reply, {error, unknown_call}, State, timeout(State)}.


handle_cast(_Msg, State) ->
    {noreply, State, ?IDLE_TIMEOUT}.

handle_info(disconnect, #state{ conn = undefined } = State) ->
    {noreply, State};


handle_info(disconnect, #state{ busy_pid = undefined } = State) ->
    Database = get_arg(dbdatabase, State#state.conn_args),
    Schema = get_arg(dbschema, State#state.conn_args),
    ?LOG_DEBUG(#{
        text => <<"SQL closing connection">>,
        in => zotonic_core,
        database => Database,
        schema => Schema,
        worker_pid => self()
    }),
    {noreply, disconnect(State), hibernate};

handle_info(disconnect, State) ->
    Database = get_arg(dbdatabase, State#state.conn_args),
    Schema = get_arg(dbschema, State#state.conn_args),
    ?LOG_ERROR(#{
        text => <<"SQL disconnect whilst busy with query">>,
        in => zotonic_core,
        result => error,
        reason => disconnect,
        database => Database,
        schema => Schema,
        query => State#state.busy_sql,
        args => State#state.busy_params,
        worker_pid => self()
    }),
    {noreply, State, disconnect(State), hibernate};

handle_info(timeout, #state{ busy_pid = undefined } = State) ->
    % Idle timeout - no SQL query is running
    {noreply, disconnect(State), hibernate};

handle_info(timeout, #state{
        busy_pid = Pid,
        busy_sql = Sql,
        busy_params = Params,
        busy_timeout = Timeout
    } = State) ->
    % Query timeout - pull the connection from underneath the caller
    % The connection needs to be killed to stop the out-of-bounds query
    % on the db server. This to prevent that long running queries are
    % filling up all our connections and also slowing down the database.
    Database = get_arg(dbdatabase, State#state.conn_args),
    Schema = get_arg(dbschema, State#state.conn_args),
    ?LOG_ERROR(#{
        text => <<"SQL Timeout">>,
        in => zotonic_core,
        result => error,
        reason => timeout,
        busy_pid => Pid,
        timeout => Timeout,
        database => Database,
        schema => Schema,
        query => Sql,
        args => Params,
        worker_pid => self()
    }),
    State1 = disconnect(State),
    {stop, normal, State1};

handle_info({'DOWN', _Ref, process, BusyPid, Reason}, #state{
        busy_pid = BusyPid,
        busy_sql = Sql,
        busy_params = Params
    } = State) ->
    % The process using our connection is down.
    % As it might have been in a transaction, we just kill
    % the connection and let the database clean up.
    Database = get_arg(dbdatabase, State#state.conn_args),
    Schema = get_arg(dbschema, State#state.conn_args),
    ?LOG_DEBUG(#{
        text => <<"SQL caller down during query">>,
        in => zotonic_core,
        result => error,
        reason => Reason,
        busy_pid => BusyPid,
        database => Database,
        schema => Schema,
        query => Sql,
        args => Params,
        worker_pid => self()
    }),
    {noreply, disconnect(State), hibernate};

handle_info({'DOWN', _Ref, process, ConnPid, Reason}, #state{
        conn = ConnPid,
        busy_pid = BusyPid,
        busy_sql = Sql,
        busy_params = Params
    } = State) when is_pid(BusyPid) ->
    % Unexpected DOWN from the connection during query
    Database = get_arg(dbdatabase, State#state.conn_args),
    Schema = get_arg(dbschema, State#state.conn_args),
    ?LOG_ERROR(#{
        text => <<"SQL connection drop during query">>,
        in => zotonic_core,
        result => error,
        reason => Reason,
        busy_pid => BusyPid,
        database => Database,
        schema => Schema,
        query => Sql,
        args => Params,
        worker_pid => self()
    }),
    State1 = State#state{ conn = undefined },
    {noreply, disconnect(State1), hibernate};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{ conn = Pid } = State) ->
    % Connection down, no processes running, ok to hibernate
    State1 = State#state{ conn = undefined },
    {noreply, disconnect(State1), hibernate};

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, #state{ busy_pid = undefined } = State) ->
    % Might be a late down message from the busy pid, ignore.
    {noreply, State, timeout(State)};

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    % Stray 'DOWN' message, might be a race condition.
    ?LOG_NOTICE(#{
        text => <<"SQL got 'DOWN' message from unknown process">>,
        in => zotonic_core,
        down_pid => Pid,
        down_reason => Reason,
        state => State,
        worker_pid => self()
    }),
    {noreply, State, timeout(State)};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    % Ignore - we have monitors for the connection and the request caller.
    {noreply, State};

handle_info(Info, State) ->
    ?LOG_WARNING(#{
        text => <<"SQL unexpected info message">>,
        in => zotonic_core,
        message => Info,
        state => State,
        worker_pid => self()
    }),
    {noreply, State, timeout(State)}.

terminate(_Reason, #state{} = State) ->
    disconnect(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Helper functions
%%

%% @doc Close the connection to the SQL server
disconnect(#state{ conn = undefined } = State) ->
    demonitor_busy(State);
disconnect(#state{ conn = Conn } = State) ->
    ok = epgsql:close(Conn),
    State1 = receive
        {'DOWN', _Ref, process, Conn, _Reason} ->
            % The SQL connection sent the error to the busy pid
            reset_busy_state(State)
        after 500 ->
            % Assume busy pid did not receive the error, kill it
            erlang:exit(Conn, kill),
            demonitor_busy(State)
    end,
    State1#state{ conn = undefined }.

%% @doc Demonitor the busy process - it will kill itself by using the
%% now defunct connection process.
demonitor_busy(#state{ busy_pid = Pid } = State) when is_pid(Pid) ->
    #state{
        busy_monitor = Monitor,
        busy_sql = Sql,
        busy_params = Params,
        busy_start = Start,
        busy_tracing = IsTracing,
        conn = Conn
    } = State,
    erlang:demonitor(Monitor),
    trace_end(IsTracing, Start, Sql, Params, Conn),
    reset_busy_state(State);
demonitor_busy(State) ->
    reset_busy_state(State).


reset_busy_state(State) ->
    State#state{
        busy_monitor = undefined,
        busy_pid = undefined,
        busy_ref = undefined,
        busy_timeout = undefined,
        busy_start = undefined,
        busy_sql = undefined,
        busy_params = []
    }.

%% @doc Calculate the remaining timeout for the running query.
timeout(#state{ busy_timeout = undefined }) ->
    ?IDLE_TIMEOUT;
timeout(#state{ busy_timeout = Timeout, busy_start = Start }) ->
    Now = msec(),
    erlang:max(1, Timeout - (Now - Start)).

try_connect_tcp(Args) ->
    Addr = get_arg(dbhost, Args),
    Port = get_arg(dbport, Args),
    SockOpts = [{active, false}, {packet, raw}, binary],
    case gen_tcp:connect(Addr, Port, SockOpts, ?CONNECT_TIMEOUT) of
        {ok, Sock} ->
            gen_tcp:close(Sock),
            ok;
        {error, _} = Error ->
            Error
    end.

connect(Args) when is_list(Args) ->
    connect(Args, 0, undefined).

connect(Args, {Pid, _Ref}) when is_list(Args) ->
    MRef = monitor(process, Pid),
    Result = connect(Args, 0, MRef),
    demonitor(MRef),
    Result.

connect(_Args, RetryCt, _MRef) when RetryCt >= ?CONNECT_RETRIES ->
    {error, econnrefused};
connect(Args, RetryCt, undefined) ->
    connect_1(Args, RetryCt, undefined);
connect(Args, RetryCt, MRef) ->
    receive
        {'DOWN', MRef, process, _Pid, _Reson} ->
            {error, caller_down}
    after 0 ->
        connect_1(Args, RetryCt, MRef)
    end.

% Suppress warning about epgsql_connect not returning {error, econnrefused}
% It is returning it, but the type spec in epgsql is wrong.
-dialyzer({nowarn_function, connect_1/3}).
connect_1(Args, RetryCt, MRef) ->
    Hostname = get_arg(dbhost, Args),
    Port = get_arg(dbport, Args),
    Database = get_arg(dbdatabase, Args),
    Username = get_arg(dbuser, Args),
    Password = get_arg(dbpassword, Args),
    Schema = get_arg(dbschema, Args),
    try
        case epgsql:connect(Hostname, Username, Password,
                           [{database, Database}, {port, Port}]) of
            {ok, Conn} ->
                set_schema(Conn, Schema);
            {error, #error{ codename = too_many_connections }} ->
                retry(Args, too_many_connections, RetryCt, MRef);
            {error, #error{ codename = out_of_memory }} ->
                retry(Args, out_of_memory, RetryCt, MRef);
            {error, #error{ codename = admin_shutdown }} ->
                retry(Args, admin_shutdown, RetryCt, MRef);
            {error, #error{ codename = crash_shutdown }} ->
                retry(Args, crash_shutdown, RetryCt, MRef);
            {error, #error{ codename = cannot_connect_now }} ->
                retry(Args, cannot_connect_now, RetryCt, MRef);
            {error, econnrefused} ->
                retry(Args, econnrefused, RetryCt, MRef);
            {error, Reason} = E ->
                ?LOG_WARNING(#{
                    text => <<"Database psql connect returned error">>,
                    in => zotonic_core,
                    database => Database,
                    schema => Schema,
                    username => Username,
                    hostname => Hostname,
                    port => Port,
                    result => error,
                    reason => Reason
                }),
                E
        end
    catch
        A:B ->
            retry(Args, {A, B}, RetryCt, MRef)
    end.

set_schema(Conn, Schema) ->
    case epgsql:squery(Conn,"SET TIME ZONE 'UTC'; SET search_path TO \"" ++ Schema ++ "\"") of
        [{ok, [], []}, {ok, [], []}] ->
            {ok, Conn};
        Error ->
            catch epgsql:close(Conn),
            {error, Error}
    end.

%% @doc Retry connection to PostgreSQL server.
retry(Args, Reason, RetryCt, MRef) ->
    Hostname = get_arg(dbhost, Args),
    Port = get_arg(dbport, Args),
    Delay = retry_delay(Reason, RetryCt),
    ?LOG_WARNING(#{
        text => <<"Database psql connect failed">>,
        in => zotonic_core,
        hostname => Hostname,
        port => Port,
        result => error,
        reason => Reason,
        retry_msec => Delay,
        worker_pid => self()
    }),
    maybe_close_connections(Reason),
    timer:sleep(Delay),
    connect(Args, RetryCt + 1, MRef).

maybe_close_connections(out_of_memory) ->
    z_db_pool:close_connections();
maybe_close_connections(too_many_connections) ->
    z_db_pool:close_connections();
maybe_close_connections(_) ->
    nop.

retry_delay(_, RetryCount) when RetryCount < 2 ->
    ?CONNECT_RETRY_SHORT;
retry_delay(too_many_connections, _) ->
    ?CONNECT_RETRY_MIDDLE;
retry_delay(_, _RetryCount)  ->
    ?CONNECT_RETRY_SLEEP.


get_arg(K, Args) ->
    maybe_default(K, proplists:get_value(K, Args)).

maybe_default(dbport, 0) -> z_config:get(dbport);
maybe_default(K, undefined) -> z_config:get(K);
maybe_default(K, "") -> z_config:get(K);
maybe_default(K, <<>>) -> z_config:get(K);
maybe_default(_K, V) -> V.

%%
%% Request tracing
%%

trace_start() ->
    msec().

trace_end(false, _Start, _Sql, _Params, _Conn) ->
    ok;
trace_end(true, Start, Sql, Params, Conn) ->
    Duration = msec() - Start,
    ?LOG_NOTICE(#{
        text => <<"SQL TRACE">>,
        in => zotonic_core,
        msec => Duration,
        sql => Sql,
        params => Params
    }),
    maybe_explain(Duration, Sql, Params, Conn).

maybe_explain(Duration, _Sql, _Params, _Conn) when Duration < ?DBTRACE_EXPLAIN_MSEC ->
    ok;
maybe_explain(_Duration, Sql, Params, Conn) ->
    case is_explainable(z_string:to_lower(Sql)) of
        true ->
            Sql1 = "explain "++Sql,
            R = epgsql:equery(Conn, Sql1, encode_values(Params)),
            maybe_log_query_plan(R);
        false ->
            ok
    end.

is_explainable(<<"begin", _/binary>>) -> false;
is_explainable(<<"commit", _/binary>>) -> false;
is_explainable(<<"rollback", _/binary>>) -> false;
is_explainable(<<"explain ", _/binary>>) -> false;
is_explainable(<<"alter ", _/binary>>) -> false;
is_explainable(<<"drop ", _/binary>>) -> false;
is_explainable(<<"create ", _/binary>>) -> false;
is_explainable(_) -> true.

maybe_log_query_plan({ok, [ #column{ name = <<"QUERY PLAN">> } ], Rows}) ->
    Lines = lists:map( fun({R}) -> [ 10, R ] end, Rows ),
    ?LOG_NOTICE(#{
        text => <<"SQL EXPLAIN">>,
        in => zotonic_core,
        explain => iolist_to_binary(Lines)
    });
maybe_log_query_plan(Other) ->
    ?LOG_NOTICE(#{
        text => <<"SQL EXPLAIN">>,
        in => zotonic_core,
        explain => Other
    }),
    ok.

msec() ->
    {A, B, C} = os:timestamp(),
    A * 1000000000 + B * 1000 + C div 1000.

%%
%% These are conversion routines between how z_db expects values and how epgsl expects them.

%% Notable differences:
%% - Input values {term, ...} (use the ?DB_PROPS(...) macro!) are term_to_binary encoded and decoded
%% - null <-> undefined
%% - date/datetimes have a floating-point second argument in epgsql, in Zotonic they don't.

encode_values(L) when is_list(L) ->
    lists:map(fun encode_value/1, L).

encode_value(undefined) ->
    null;
encode_value({term, undefined}) ->
    null;
encode_value({term, Term}) ->
    B = term_to_binary(Term),
    <<?TERM_MAGIC_NUMBER, B/binary>>;
encode_value({term_json, undefined}) ->
    null;
encode_value({term_json, Term}) ->
    jsxrecord:encode(Term);
encode_value(Value) ->
    Value.


decode_reply({ok, Columns, Rows}) ->
    {ok, Columns, lists:map(fun decode_values/1, Rows)};
decode_reply({ok, Nr, Columns, Rows}) ->
    {ok, Nr, Columns, lists:map(fun decode_values/1, Rows)};
decode_reply(R) ->
    R.

decode_values(T) when is_tuple(T) ->
    list_to_tuple(decode_values(tuple_to_list(T)));
decode_values(L) when is_list(L) ->
    lists:map(fun decode_value/1, L).

decode_value({V}) ->
    {decode_value(V)};

decode_value(null) ->
    undefined;
decode_value(<<?TERM_MAGIC_NUMBER, B/binary>>) ->
    binary_to_term(B);
decode_value({H,M,S}) when is_float(S) ->
    {H,M,trunc(S)};
decode_value({{Y,Mm,D},{H,M,S}}) when is_float(S) ->
    {{Y,Mm,D},{H,M,trunc(S)}};
decode_value(V) ->
    V.
