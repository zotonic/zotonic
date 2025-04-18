%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2014-2025 Arjan Scherpenisse
%% @doc Database pool wrapper. Start and stop database pool workers.
%% @end

%% Copyright 2014-2025 Arjan Scherpenisse
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

-module(z_db_pool).

-include_lib("zotonic.hrl").

-define(DEFAULT_DB_DRIVER, z_db_pgsql).
-define(DEFAULT_DB_MAX_CONNECTIONS, 20).

-export([
    status/0,
    status/1,
    close_connections/0,
    close_connections/1,
    pause_connections/1,
    unpause_connections/1,
    child_spec/2,
    get_database_options/1,
    test_connection/1,
    test_connection/2,
    db_pool_name/1,
    db_driver_default/0,
    db_driver/1,
    database_options/2,
    database_options/3,
    get_connection/1,
    get_unpaused_connection/1,
    return_connection/2
]).

status() ->
    Ctxs = z_sites_manager:get_site_contexts(),
    lists:map(fun status/1, Ctxs).

status(Context) ->
    case m_site:get(dbdatabase, Context) of
        none ->
            {z_context:site(Context), {0,0}};
        _Db ->
            PoolName = db_pool_name(Context),
            case erlang:whereis(PoolName) of
                Pid when is_pid(Pid) ->
                    {_StateName, Workers, _Overflow, Working} = poolboy:status(Pid),
                    {z_context:site(Context), {Workers,Working}};
                undefined ->
                    {z_context:site(Context), {0,0}}
            end
    end.

close_connections() ->
    Ctxs = z_sites_manager:get_site_contexts(),
    lists:foreach(fun close_connections/1, Ctxs).

close_connections(Context) ->
    case m_site:get(dbdatabase, Context) of
        none -> ok;
        _Db ->
            PoolName = db_pool_name(Context),
            close_workers(erlang:whereis(PoolName))
    end.

close_workers(undefined) ->
    ok;
close_workers(PoolPid) when is_pid(PoolPid) ->
    WorkerPids = gen_server:call(PoolPid, get_avail_workers),
    lists:foreach(
                fun(WorkerPid) ->
                    WorkerPid ! disconnect
                end,
                WorkerPids).

%% @doc Ensure that all worker processes are paused. This is useful if
%% a large update of the schema needs to be done.
pause_connections(Context) ->
    case m_site:get(dbdatabase, Context) of
        none -> ok;
        _Db ->
            PoolName = db_pool_name(Context),
            pause_workers(erlang:whereis(PoolName))
    end.

pause_workers(undefined) ->
    ok;
pause_workers(PoolPid) when is_pid(PoolPid) ->
    WorkerPids = gen_server:call(PoolPid, get_avail_workers),
    lists:foreach(
                fun(WorkerPid) ->
                    catch gen_server:call(WorkerPid, pause, infinity)
                end,
                WorkerPids).

%% @doc Tell all (paused) workers that it is ok to continue.
unpause_connections(Context) ->
    case m_site:get(dbdatabase, Context) of
        none -> ok;
        _Db ->
            PoolName = db_pool_name(Context),
            unpause_workers(erlang:whereis(PoolName))
    end.

unpause_workers(undefined) ->
    ok;
unpause_workers(PoolPid) when is_pid(PoolPid) ->
    WorkerPids = gen_server:call(PoolPid, get_avail_workers),
    lists:foreach(
                fun(WorkerPid) ->
                    catch gen_server:call(WorkerPid, unpause, infinity)
                end,
                WorkerPids).


db_pool_name(Site) when is_atom(Site) ->
    list_to_atom("z_db_pool" ++ [$$ | atom_to_list(Site)]);
db_pool_name(#context{} = Context) ->
    db_pool_name(z_context:site(Context)).


db_driver_default() ->
    ?DEFAULT_DB_DRIVER.

db_driver(SiteProps) when is_list(SiteProps) ->
    proplists:get_value(dbdriver, SiteProps, ?DEFAULT_DB_DRIVER);
db_driver(Context) ->
    case m_site:get(dbdriver, Context) of
        undefined -> ?DEFAULT_DB_DRIVER;
        Driver -> Driver
    end.

%% @doc Perform a connect to test if the database is working.
-spec test_connection( atom(), proplists:proplist() ) -> ok | {error, nodatabase | noschema | term()}.
test_connection(Site, SiteProps) when is_list(SiteProps) ->
    Database = proplists:get_value(dbdatabase, SiteProps),
    case has_database(Database) of
        true ->
            DbDriver = db_driver(SiteProps),
            DbOpts = database_options(Site, SiteProps),
            DbDriver:test_connection(DbOpts);
        false ->
            {error, nodatabase}
    end.

-spec test_connection( z:context() ) -> ok | {error, nodatabase | noschema | term()}.
test_connection(Context) ->
    Database = m_site:get(dbdatabase, Context),
    case has_database(Database) of
        true ->
            DbDriver = db_driver(Context),
            DbDriver:test_connection(get_database_options(Context));
        false ->
            {error, nodatabase}
    end.


%% @doc Get all configuration options for this site which are related
%% to the database configuration.
-spec get_database_options( z:context() ) -> proplists:proplist().
get_database_options(Context) ->
    z_depcache:memo(
        fun() ->
            database_options(z_context:site(Context), m_site:all(Context))
        end,
        z_db_opts,
        ?DAY,
        Context).

%% @doc Optionally add the db pool connection
child_spec(Site, SiteProps) ->
    case has_database( proplists:get_value(dbdatabase, SiteProps, atom_to_list(Site)) ) of
        false ->
            %% No database connection needed
            undefined;
        true ->
            %% Add a db pool to the site's processes
            Name = db_pool_name(Site),
            WorkerModule = db_driver(SiteProps),
            WorkerArgs = database_options(Site, SiteProps),

            PoolSize = proplists:get_value(db_max_connections, SiteProps, ?DEFAULT_DB_MAX_CONNECTIONS),
            PoolArgs = [{name, {local, Name}},
                        {worker_module, WorkerModule},
                        {size, PoolSize},
                        {max_overflow, 0}],
            poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end.

has_database(none) -> false;
has_database(<<"none">>) -> false;
has_database("none") -> false;
has_database(_) -> true.


%% @doc Merge the database options from the global config into the site config.
%%      If the site uses the default database and it has no schema defined then
%%      the site's name is used as the schema name. If the site uses its own
%%      database then the schema defaults to "public".
-spec database_options( atom(), proplists:proplist() ) -> proplists:proplist().
database_options(Sitename, SiteProps) ->
    database_options(Sitename, SiteProps, db_opts_global()).

-spec database_options( atom(), proplists:proplist(), proplists:proplist() ) -> proplists:proplist().
database_options(Sitename, SiteProps, GlobalProps) ->
    SiteProps1 = lists:filter(
        fun({K, V}) ->
            proplists:is_defined(K, db_optkeys()) andalso not is_empty(V)
        end,
        SiteProps),
    DefaultDB = get_value(dbdatabase, GlobalProps, "zotonic"),
    SitePropsDB = proplists:get_value(dbdatabase, SiteProps1, DefaultDB),
    DefaultSchema = case SitePropsDB of
        DefaultDB -> proplists:get_value(dbschema, SiteProps1, z_convert:to_list(Sitename));
        _ -> get_value(dbschema, GlobalProps, "public")
    end,
    Defaults = [
        {dbhost, get_value(dbhost, GlobalProps, "localhost")},
        {dbport, get_value(dbport, GlobalProps, 5432)},
        {dbpassword, get_value(dbpassword, GlobalProps, "")},
        {dbuser, get_value(dbuser, GlobalProps, "zotonic")},
        {dbdatabase, DefaultDB},
        {dbschema, DefaultSchema},
        {dbdropschema, get_value(dbdropschema, GlobalProps, false)},
        {dbdriver, get_value(dbdriver, GlobalProps, ?DEFAULT_DB_DRIVER)}
    ],
    lists:ukeymerge(1, lists:sort(SiteProps1), lists:sort(Defaults)).

get_value(K, Props, Default) ->
    V = proplists:get_value(K, Props),
    case is_empty(V) of
        true -> Default;
        false -> V
    end.

db_opts_global() ->
    lists:map(
        fun(K) ->
            {K, z_config:get(K)}
        end,
        db_optkeys()).

db_optkeys() ->
    [ dbhost, dbport, dbpassword, dbuser, dbdatabase, dbschema, dbdropschema, dbdriver ].

is_empty(undefined) -> true;
is_empty("") -> true;
is_empty(<<>>) -> true;
is_empty(0) -> true;
is_empty(null) -> true;
is_empty(_) -> false.


%% @doc Request a database connection and unpause it if it was paused.
-spec get_unpaused_connection(Context) -> {ok, pid()} | {error, full | nodatabase} when
    Context :: z:context().
get_unpaused_connection(Context) ->
    case get_connection(Context) of
        {ok, Pid} ->
            gen_server:call(Pid, unpause, infinity),
            {ok, Pid};
        {error, _} = Error ->
            Error
    end.


%% @doc Request a database connection worker from the database pool. Each worker
%% manages a single database connection. If a worker is available then its pid is
%% returned. The worker can then provide the actual database connection for querying
%% the database. See z_db_pgql for query functions that fetch the database connection
%% from the worker process.
-spec get_connection( z:context() ) -> {ok, pid()} | {error, full | nodatabase}.
get_connection(#context{db={Pool,_}} = Context) ->
    case timer:tc(fun() -> poolboy:checkout(Pool) end) of
        {Time, full} ->
            % No connections for > 5secs, really full
            z_stats:record_event(db, pool_full, Context),
            z_stats:record_duration(db, connection_wait, Time, Context),
            {error, full};
        {Time, Pid} when is_pid(Pid), Time > 10000 ->
            % Start warning if we have to wait > 10 msec for a connection
            record_db_status(poolboy:status(Pool), Context),
            z_stats:record_duration(db, connection_wait, Time, Context),
            {ok, Pid};
        {Time, Pid} when is_pid(Pid) ->
            % All ok, we quickly got a connection, so no overload.
            z_stats:record_duration(db, connection_wait, Time, Context),
            {ok, Pid}
    end;
get_connection(_Context) ->
    {error, nodatabase}.

record_db_status({full, _Avail, _Overflow, _Busy}, Context) ->
    z_stats:record_event(db, pool_high_usage, Context);
record_db_status({overflow, _Avail, _Overflow, _Busy}, Context) ->
    z_stats:record_event(db, pool_high_usage, Context);
record_db_status({ready, 0, _Overflow, _Busy}, Context) ->
    z_stats:record_event(db, pool_high_usage, Context);
record_db_status({ready, _Avail, _X, _Busy}, Context) ->
    z_stats:record_event(db, pool_slow_checkout, Context).

-spec return_connection( pid(), z:context() ) -> ok | {error, term()}.
return_connection(Worker, #context{db={Pool,_}}) when is_pid(Worker) ->
    poolboy:checkin(Pool, Worker);
return_connection(_Worker, _Context) ->
    {error, nodatabase}.
