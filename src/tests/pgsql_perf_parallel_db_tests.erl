-module(pgsql_perf_parallel_db_tests).

-compile(export_all).

-define(TESTTIME, 15000).
-define(NUM_WORKERS, 15). 

ts() ->
    z_utils:now_msec().


test() ->
    Context = z:c(zanymeta),
    test(fun test_z_db_q1/3, z_db, "select now()", [], Context),
    test(fun test_z_db_q1/3, z_db, "select true", [], Context),
    test(fun test_z_db_q1/3, z_db, "select 'hello'", [], Context),
    test(fun test_z_db_q1/3, z_db, "select count(*) from rsc", [], Context),
    ok.
    


test(Fun, Name, Sql, Args, Context) ->
    {result, Count} = do_work(Fun, [Sql, Args, Context], ?NUM_WORKERS, ?TESTTIME),
    lager:info("~p test result: ~p queries in ~p ms, ~p parallel test runners (~s)", [Name, Count, ?TESTTIME, ?NUM_WORKERS, Sql]).

test_z_db_q1(Query, Args, Context) ->
    z_db:q1(Query, Args, Context).


do_work(Fun, Args, N, T) ->
    Parent = self(),
    Workers = [spawn_link(fun() -> worker_loop(Parent, Fun, Args) end) || _ <- lists:seq(1,N)],
    work_dispatch_loop(Workers, ts(), T, 0).


work_dispatch_loop(Workers, Start, MaxT, Count) ->
    case Count rem 5000 =:= 0 of
        true ->
            nop;%lager:warning("Pool status: ~p", [poolboy:status(whereis('z_db_pool$zanymeta'))]);
        false -> nop
    end,
            
    case ts() - Start > MaxT of
        true ->
            [begin
                 W ! stop,
                 receive
                     stopped -> ok
                 end
             end|| W <- Workers],
            {result, Count};
        false ->
            receive
                {want_work, Worker} ->
                    Worker ! work,
                    work_dispatch_loop(Workers, Start, MaxT, Count+1)
            end
    end.
                                
                                   
worker_loop(Parent, Fun, Args) ->
    Parent ! {want_work, self()},
    receive
        work ->
            apply(Fun, Args),
            worker_loop(Parent, Fun, Args);
        stop ->
            Parent ! stopped
    end.


