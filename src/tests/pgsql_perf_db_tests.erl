-module(pgsql_perf_db_tests).

-compile(export_all).

-define(TESTTIME, 15000).


connect() ->
    {ok, Conn} = pgsql:connect("localhost", "zotonic", "",
                               [{database, "zotonic"}]),
    Conn.

ts() ->
    z_utils:now_msec().


test() ->
    Conn = connect(),
    test(fun test_squery/4, squery, Conn, "select now()", []),
    test(fun test_squery/4, squery, Conn, "select true", []),
    test(fun test_squery/4, squery, Conn, "select 'hello'", []),
    test(fun test_squery/4, squery, Conn, "select count(*) from rsc", []),

    test(fun test_equery/4, equery, Conn, "select now()", []),
    test(fun test_equery/4, equery, Conn, "select true", []),
    test(fun test_equery/4, equery, Conn, "select 'hello'", []),
    test(fun test_equery/4, equery, Conn, "select count(*) from rsc", []),

    Context = z:c(zanymeta),
    test(fun test_z_db_q1/4, z_db, Context, "select now()", []),
    test(fun test_z_db_q1/4, z_db, Context, "select true", []),
    test(fun test_z_db_q1/4, z_db, Context, "select 'hello'", []),
    test(fun test_z_db_q1/4, z_db, Context, "select count(*) from rsc", []),

    ok.
    


test(Fun, Name, Conn, Sql, Args) ->
    {result, Count} = Fun(Conn, Sql, Args, ?TESTTIME),
    lager:info("~p test result: ~p queries in ~p ms (~s)", [Name, Count, ?TESTTIME, Sql]).


test_squery(Conn, Sql, _Args, Time) ->
    test_squery1(Conn, Sql, Time, 0, ts()).

test_squery1(Conn, Sql, Time, Count, Start) ->
    {ok, _, _} = pgsql:squery(Conn, Sql),
    case ts() - Start of
        T when T < Time ->
            test_squery1(Conn, Sql, Time, Count+1, Start);
        _ ->
            {result, Count}
    end.

test_equery(Conn, Sql, Args, Time) ->
    test_equery1(Conn, Sql, Args, Time, 0, ts()).

test_equery1(Conn, Sql, Args, Time, Count, Start) ->
    {ok, _, _} = pgsql:equery(Conn, Sql, Args),
    case ts() - Start of
        T when T < Time ->
            test_equery1(Conn, Sql, Args, Time, Count+1, Start);
        _ ->
            {result, Count}
    end.

test_z_db_q1(Conn, Sql, Args, Time) ->
    test_z_db_q1(Conn, Sql, Args, Time, 0, ts()).

test_z_db_q1(Context, Sql, Args, Time, Count, Start) ->
    %%{ok, _, _} = pgsql:equery(Conn, Sql, Args),
    z_db:q1(Sql, Args, Context),
    case ts() - Start of
        T when T < Time ->
            test_z_db_q1(Context, Sql, Args, Time, Count+1, Start);
        _ ->
            {result, Count}
    end.
