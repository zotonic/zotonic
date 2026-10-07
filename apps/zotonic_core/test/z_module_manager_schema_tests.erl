%% @doc Regression tests for schema transaction cache invalidation.
-module(z_module_manager_schema_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/zotonic.hrl").

-export([manage_schema/2]).

-define(COLUMNS_KEY, {columns, "schema_test", "public", "rsc"}).

schema_cache_test() ->
    {ok, Cache} = depcache:start_link([]),
    Context = #context{depcache = Cache},
    Modules = [z_db, z_db_pool, z_datamodel],
    try
        lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
        meck:expect(z_db_pool, get_database_options, fun(_) ->
            [{dbdatabase, "schema_test"}]
        end),
        meck:expect(z_datamodel, manage, fun(?MODULE, #datamodel{}, Ctx) ->
            %% The datamodel must not consume pre-commit column metadata.
            ?assertEqual(undefined, z_depcache:get(?COLUMNS_KEY, Ctx)),
            ok
        end),
        lists:foreach(fun(Outcome) ->
            meck:expect(z_db, transaction, fun(Fun, Ctx) ->
                Result = Fun(Ctx#context{dbc = self()}),
                ?assertEqual({ok, old_columns}, z_depcache:get(?COLUMNS_KEY, Ctx)),
                case Outcome of
                    commit -> Result;
                    rollback -> {rollback, test_failure}
                end
            end),
            case Outcome of
                commit -> ?assertEqual(ok, z_module_manager:reinstall(?MODULE, Context));
                rollback -> ?assertError(function_clause,
                    z_module_manager:reinstall(?MODULE, Context))
            end,
            ?assertEqual(undefined, z_depcache:get(?COLUMNS_KEY, Context))
        end, [commit, rollback])
    after
        lists:foreach(fun(M) -> meck:unload(M) end, Modules),
        gen_server:stop(Cache)
    end.

%% @doc Simulate a parallel reader caching the old schema before commit.
manage_schema(install, #context{dbc = Connection} = Context) when is_pid(Connection) ->
    ?assertEqual(self(), Connection),
    ok = z_db:flush(Context),
    {Pid, Ref} = spawn_monitor(fun() ->
        ok = z_depcache:set(?COLUMNS_KEY, old_columns, 3600,
            [{database, "schema_test"}], Context)
    end),
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok;
        {'DOWN', Ref, process, Pid, Reason} -> error(Reason)
    after 1000 ->
        error(cache_writer_timeout)
    end,
    #datamodel{}.
