%% @doc Optional database work must yield to foreground connection usage.
-module(z_db_low_load_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

low_load_test() ->
    PoolName = z_db_low_load_test_pool,
    {ok, Pool} = poolboy:start_link([{name, {local, PoolName}},
        {worker_module, z_db_pgsql}, {size, 4}, {max_overflow, 0}]),
    Context = #context{db = {PoolName, z_db_pgsql}},
    try
        % Database workers start disconnected. Idle timeouts close sockets,
        % not workers: all configured slots still count towards capacity.
        Workers = gen_server:call(Pool, get_avail_workers),
        lists:foreach(fun(Worker) ->
            Worker ! timeout,
            _ = sys:get_state(Worker)
        end, Workers),
        ?assertMatch({ready, 4, 0, 0}, poolboy:status(Pool)),
        ?assertEqual({ok, ran}, z_db:run_if_low_load(fun() -> {ok, ran} end, Context)),
        ?assertError(callback_failed, z_db:run_if_low_load(fun() -> error(callback_failed) end, Context)),
        A = poolboy:checkout(Pool),
        ?assert(z_db:is_low_load(Context)),
        B = poolboy:checkout(Pool),
        ?assertNot(z_db:is_low_load(Context)),
        ?assertEqual({error, busy}, z_db:run_if_low_load(fun unexpected/0, Context)),
        C = poolboy:checkout(Pool),
        _D = poolboy:checkout(Pool),
        ?assertEqual({error, busy}, z_db:run_if_low_load(fun unexpected/0, Context)),
        poolboy:checkin(Pool, A),
        poolboy:checkin(Pool, B),
        ?assertNot(z_db:is_low_load(Context)),
        poolboy:checkin(Pool, C),
        ?assertEqual(resumed, z_db:run_if_low_load(fun() -> resumed end, Context))
    after
        poolboy:stop(Pool)
    end,
    ?assertEqual({error, busy}, z_db:run_if_low_load(fun unexpected/0, Context)),
    ?assertNot(z_db:is_low_load(#context{})).

unresponsive_pool_test() ->
    Pool = spawn(fun() -> receive stop -> ok end end),
    try
        true = register(z_db_low_load_unresponsive_test_pool, Pool),
        ?assertEqual({error, busy}, z_db:run_if_low_load(fun unexpected/0,
            #context{db = {z_db_low_load_unresponsive_test_pool, z_db_pgsql}}))
    after
        Pool ! stop
    end.

unexpected() -> error(optional_work_was_started).
