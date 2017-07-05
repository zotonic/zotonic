%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2014 Arjan Scherpenisse
%% Date: 2014-04-29
%%
%% @doc Database pool wrapper

%% Copyright 2014 Arjan Scherpenisse
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

-export([
         status/0,
         status/1,
         close_connections/0,
         close_connections/1,
         child_spec/2,
         get_database_options/1,
         test_connection/1,
         db_pool_name/1,
         db_driver/1,
         db_opts/1,
         get_connection/1,
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


db_pool_name(#context{} = Context) ->
    db_pool_name(z_context:site(Context));
db_pool_name(Site) ->
    list_to_atom("z_db_pool" ++ [$$ | atom_to_list(Site)]).

db_driver(SiteProps) when is_list(SiteProps) ->
    proplists:get_value(dbdriver, SiteProps, ?DEFAULT_DB_DRIVER);
db_driver(Context=#context{}) ->
    case m_site:get(dbdriver, Context) of
        undefined -> ?DEFAULT_DB_DRIVER;
        Driver -> Driver
    end.

%% @doc Perform a connect to test whether the database is working.
test_connection(Context) ->
    DbDriver = db_driver(Context),
    DbDriver:test_connection(get_database_options(Context)).


%% @doc Get all configuration options for this site which are related
%% to the database configuration.
get_database_options(Context) ->
    z_depcache:memo(fun() -> db_opts(m_site:all(Context)) end, z_db_opts, ?WEEK, Context).


%% @doc Optionally add the db pool connection
child_spec(Host, SiteProps) ->
    case proplists:get_value(dbdatabase, SiteProps, atom_to_list(Host)) of
        none ->
            %% No database connection needed
            undefined;
        _ ->
            %% Add a db pool to the site's processes
            PoolSize    = proplists:get_value(db_max_connections, SiteProps, 10),

            Name = db_pool_name(Host),

            WorkerModule = db_driver(SiteProps),
            WorkerArgs = db_opts(SiteProps),

            PoolArgs = [{name, {local, Name}},
                        {worker_module, WorkerModule},
                        {size, PoolSize},
                        {max_overflow, 0}],
            poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end.

%% @doc Any argument starting with 'db' is considered a DB driver
%% argument and those are the only arguments that are given to the
%% database pool worker processes.
db_opts(SiteProps) ->
    Kvs = lists:filter(fun({K, V}) ->
                               case atom_to_list(K) of
                                   "db"++_ -> not z_utils:is_empty(V);
                                   _ -> false
                               end
                       end,
                       SiteProps),
    Defaults = [{dbhost, z_config:get(dbhost, "localhost")},
                {dbport, z_config:get(dbport, 5432)},
                {dbpassword, z_config:get(dbpassword, "")},
                {dbuser, z_config:get(dbuser, "zotonic")},
                {dbdatabase, z_config:get(dbdatabase, "zotonic")},
                {dbschema, z_config:get(dbschema, "public")}],
    lists:ukeymerge(1, lists:sort(Kvs), lists:sort(Defaults)).

get_connection(#context{db={Pool,_}}) ->
    poolboy:checkout(Pool).

return_connection(Worker, #context{db={Pool,_}}) ->
    poolboy:checkin(Pool, Worker).
