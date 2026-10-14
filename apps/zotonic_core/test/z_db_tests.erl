%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(z_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/zotonic.hrl").

alter_table_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    ok = z_db:drop_table(dbtests, Context),
    A = [
        #column_def{
            name = id, type = "serial", is_nullable = false, primary_key = true
        },
        #column_def{
            name = a, type = "character varying", length = 20
        },
        #column_def{
            name = d, type = "integer", length = undefined
        },
        #column_def{
            name = e, type = "float", length = undefined
        }
    ],
    ok = z_db:alter_table(dbtests, A, Context),
    {ok, #column_def{ name = id, type = <<"serial">> }} = z_db:column(dbtests, id, Context),
    {ok, #column_def{ name = a, type = <<"character varying">>, length = 20 }} = z_db:column(dbtests, a, Context),
    {ok, #column_def{ name = d, type = <<"integer">> }} = z_db:column(dbtests, d, Context),
    {error, enoent} = z_db:column(dbtests, f, Context),
    A1 = [
        #column_def{
            name = id, type = <<"bigserial">>, is_nullable = false, primary_key = true
        },
        #column_def{
            name = a, type = "character varying", length = 10
        },
        #column_def{
            name = d, type = "varchar", length = 30
        },
        #column_def{
            name = f, type = "integer", length = undefined
        }
    ],
    ok = z_db:alter_table(dbtests, A1, Context),
    {ok, #column_def{ name = id, type = <<"bigserial">> }} = z_db:column(dbtests, id, Context),
    {ok, #column_def{ name = a, type = <<"character varying">>, length = 10 }} = z_db:column(dbtests, a, Context),
    {ok, #column_def{ name = d, type = <<"character varying">>, length = 30 }} = z_db:column(dbtests, d, Context),
    {ok, #column_def{ name = f, type = <<"integer">> }} = z_db:column(dbtests, f, Context),
    {error, enoent} = z_db:column(dbtests, e, Context),
    ok = z_db:drop_table(dbtests, Context),
    ok.

postgres_datetime_conversion_test() ->
    Context = z_context:new(zotonic_site_testsandbox),

    ?assertMatch( [{{{_,_,_},{_,_,S}}}] when is_integer(S), z_db:q("select now();", Context)),
    ?assertMatch( [{[{{_,_,_},{_,_,S}}]}] when is_integer(S), z_db:q("select array_agg(now());", Context)),
    ?assertMatch( [{[{{{_,_,_},{_,_, S}}, _} ]}] when is_integer(S), z_db:q("select array_agg(row(now(), 1));", Context)),

    ok.

postgres_null_conversion_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    [{undefined}] = z_db:q("select null;", Context),
    ok.

postgres_eterm_conversion_test() ->
    Context = z_context:new(zotonic_site_testsandbox),

    %% Should be encoded as null value in postgres, which is returned as undefined.
    [{undefined}] = z_db:q("select $1::bytea;", [{term, undefined}], Context),

    [{#{ test := 123 }}] = z_db:q("select $1::bytea;", [{term, #{ test => 123 }}], Context),
    ok.

postgres_json_conversion_test() ->
    Context = z_context:new(zotonic_site_testsandbox),

    %% Should be encoded as null value in postgres, which is returned as undefined.
    [{undefined}] = z_db:q("select $1::jsonb;", [{term_json, undefined}], Context),

    %% jsonb typed cells are decoded.
    [{#{<<"test">> := 123}}] = z_db:q("select $1::jsonb;", [{term_json, #{ test => 123 }}], Context),

    %% Check if jsxrecord is used to encode and decode the terms.
    [{#rsc_list{ list = [1] }}] =
        z_db:q("select $1::jsonb;", [{term_json, #rsc_list{ list = [1] }}], Context),

    ok.

cancel_timeout_test() ->
    Context = z_context:new(zotonic_site_testsandbox),

    % Ensure that we can see our own queries
    {ok, [ #{ <<"query">> := <<"select * from pg_stat_activity ", _/binary>> } ]}
        = z_db:qmap("select * from pg_stat_activity where state = 'active' and query like 'select * from pg_stat_activity %'", Context),

    % Query that should not timeout
    {ok, [ #{ <<"pg_sleep">> :=  <<>> } ]} = z_db:qmap("select PG_SLEEP(0.1)", [], [ {timeout, 200} ], Context),

    % Query that must time out
    {error, query_timeout} = z_db:qmap("select PG_SLEEP(30)", [], [ {timeout, 200} ], Context),

    % Canceled query should be gone
    {ok, []} = z_db:qmap("select * from pg_stat_activity where state = 'active' and query = 'select PG_SLEEP(30)'", Context),

    ok.

disconnect_test() ->
    Context = z_context:new(zotonic_site_testsandbox),

    %% Disconnect the worker while a query is running. 
    %% The transaction should not be able to complete, because
    %% the COMMIT message could not be sent.
    {rollback,{error,connection_down}} = z_db:transaction(
        fun(Ctx) ->
                spawn(fun() ->
                              z_db:qmap("select pg_sleep(10)", Ctx),
                              ok
                      end),
                P = z_context:db_connection(Ctx),
                timer:sleep(1),
                P ! disconnect,
                ok 
        end, Context),

    %% Kill the worker during a transaction.
    %% Checks if the expected errors are returned and the
    %% transactions also returns the right error, because 
    %% it could not send the COMMIT message either.
    {rollback,{error,connection_down}} = z_db:transaction(
        fun(Ctx) ->
                {ok, [#{<<"?column?">> := 1}]} = z_db:qmap("select 1", Ctx),

                %% Kill the connection
                P = z_context:db_connection(Ctx),
                exit(P, kill),

                %% Wait a bit until the worker is really dead.
                timer:sleep(1),

                %% The expected error in this situation
                {error, connection_down} = z_db:qmap("select 1", Ctx)
        end, Context),

    ok.

%% Notifications belong to the outer transaction and must not escape a rollback.
transaction_notifications_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    Event = z_db_test_notification,
    ok = z_notifier:observe(Event, self(), Context),
    try
        lists:foreach(fun(Outcome) ->
            Ref = make_ref(),
            Result = z_db:transaction(fun(Tx) ->
                z_notifier:notify({Event, Ref, notify}, Tx),
                ok = z_db:transaction(fun(NestedTx) ->
                    z_notifier:notify1({Event, Ref, notify1}, NestedTx),
                    ok
                end, Tx),
                ?assertMatch([_, _], erlang:get(notify_queue)),
                receive
                    {'$gen_cast', {{Event, Ref, _}, _}} -> error(premature_notification)
                after 0 -> ok
                end,
                case Outcome of
                    commit -> committed;
                    rollback -> {rollback, cancelled};
                    thrown_rollback -> throw({rollback, cancelled});
                    nested_rollback ->
                        z_db:transaction(fun(_) -> {rollback, cancelled} end, Tx),
                        error(continued_after_nested_rollback);
                    exception -> error(cancelled)
                end
            end, Context),
            ?assertEqual(undefined, erlang:get(notify_queue)),
            case Outcome of
                commit ->
                    ?assertEqual(committed, Result),
                    lists:foreach(fun(Kind) ->
                        receive
                            {'$gen_cast', {{Event, Ref, Kind}, _}} -> ok
                        after 1000 -> error({missing_notification, Kind})
                        end
                    end, [notify, notify1]);
                _ ->
                    case Outcome of
                        exception -> ?assertMatch({rollback, {cancelled, _}}, Result);
                        _ -> ?assertEqual({rollback, cancelled}, Result)
                    end,
                    receive
                        {'$gen_cast', {{Event, Ref, _}, _}} -> error(rollback_notification)
                    after 100 -> ok
                    end
            end
        end, [commit, rollback, thrown_rollback, nested_rollback, exception])
    after
        z_notifier:detach(Event, Context),
        z_notifier:notify_queue_flush(Context)
    end.
