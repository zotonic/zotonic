%% @hidden
-module(z_pivot_custom_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/zotonic.hrl").
-export([custom_pivot/2]).

custom_pivot_upsert_test_() ->
    {timeout, 60, fun() ->
        ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
        Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
        Table = pivot_upsert_test,
        ok = z_pivot_rsc:define_custom_pivot(upsert_test, [
            {required, "text not null"},
            #column_def{name=tags, type="text", is_array=true},
            {#column_def{name=props_json, type="jsonb"}, [noindex]}
        ], Context),
        {ok, Id} = m_rsc:insert(#{<<"category_id">> => text, <<"title">> => <<"Pivot upsert test">>}, Context),
        {ok, OtherId} = m_rsc:insert(#{<<"category_id">> => text}, Context),
        {ok, OtherId} = z_db:insert(Table, #{id => OtherId, required => <<"other">>}, Context),
        % Run the real pivot job synchronously without changing the queue manager's state.
        JobManager = spawn(fun job_manager/0),
        Ctx = Context#context{pivot_server=JobManager},
        ok = z_notifier:observe(custom_pivot, {?MODULE, custom_pivot}, Context),
        try
            % A sparse result is valid even before the resource has a pivot row.
            pivot(Id, undefined, Ctx),
            ?assertEqual({error, enoent}, z_db:select(Table, Id, Context)),
            pivot(Id, #{id => -1, <<"id">> => -2, required => <<"present">>, tags => [first], keep => true}, Ctx),
            {ok, Row1} = z_db:select(Table, Id, Context),
            ?assertEqual([<<"first">>], maps:get(<<"tags">>, Row1)),
            ?assertEqual(true, maps:get(<<"keep">>, Row1)),
            % Proplists and maps must both use the resource id, regardless of supplied ids.
            pivot(Id, [{id, -1}, {<<"id">>, -2}, {required, <<"changed">>}, {tags, [second]}, {added, true}], Ctx),
            {ok, Row2} = z_db:select(Table, Id, Context),
            ?assertEqual(<<"changed">>, maps:get(<<"required">>, Row2)),
            ?assertEqual([<<"second">>], maps:get(<<"tags">>, Row2)),
            ?assertEqual(true, maps:get(<<"keep">>, Row2)),
            ?assertEqual(true, maps:get(<<"added">>, Row2)),
            % Omitted required columns retain their values, also when merging properties.
            pivot(Id, #{tags => [third], added => undefined, partial => true}, Ctx),
            {ok, Row3} = z_db:select(Table, Id, Context),
            ?assertEqual(<<"changed">>, maps:get(<<"required">>, Row3)),
            ?assertEqual([<<"third">>], maps:get(<<"tags">>, Row3)),
            ?assertEqual(true, maps:get(<<"keep">>, Row3)),
            ?assertEqual(true, maps:get(<<"partial">>, Row3)),
            ?assertNot(maps:is_key(<<"added">>, Row3)),
            % A complete scalar update uses the upsert without touching stored properties.
            pivot(Id, #{required => <<"scalar">>, tags => [fourth]}, Ctx),
            {ok, Row4} = z_db:select(Table, Id, Context),
            ?assertEqual(<<"scalar">>, maps:get(<<"required">>, Row4)),
            ?assertEqual([<<"fourth">>], maps:get(<<"tags">>, Row4)),
            ?assertEqual(true, maps:get(<<"keep">>, Row4)),
            lists:foreach(fun(Result) ->
                pivot_result(Id, Result, Ctx),
                ?assertEqual({ok, Row4}, z_db:select(Table, Id, Context))
            end, [undefined, none, ok]),
            % Deletion uses the enclosing transaction, like other pivot writes.
            Rollback = z_db:transaction(fun(Tx) ->
                pivot(Id, undefined, Tx),
                ?assertEqual({error, enoent}, z_db:select(Table, Id, Tx)),
                {rollback, test}
            end, Ctx),
            ?assertEqual({rollback, test}, Rollback),
            ?assertEqual({ok, Row4}, z_db:select(Table, Id, Context)),
            pivot_result(Id, [{upsert_test, undefined}], Ctx),
            ?assertEqual({error, enoent}, z_db:select(Table, Id, Context)),
            pivot(Id, undefined, Ctx),
            ?assertEqual({error, enoent}, z_db:select(Table, Id, Context)),
            {ok, OtherRow} = z_db:select(Table, OtherId, Context),
            ?assertEqual(<<"other">>, maps:get(<<"required">>, OtherRow)),
            pivot(Id, #{required => <<"recreated">>}, Ctx),
            {ok, Recreated} = z_db:select(Table, Id, Context),
            ?assertEqual(<<"recreated">>, maps:get(<<"required">>, Recreated)),
            ?assertNot(maps:is_key(<<"keep">>, Recreated))
        after
            erase({?MODULE, Id}),
            z_notifier:detach(custom_pivot, Context),
            exit(JobManager, kill),
            m_rsc:delete(Id, Context),
            m_rsc:delete(OtherId, Context),
            z_db:drop_table(Table, Context)
        end
    end}.

pivot(Id, Props, Context) ->
    pivot_result(Id, {upsert_test, Props}, Context).

pivot_result(Id, Result, Context) ->
    put({?MODULE, Id}, {result, Result}),
    ok = z_pivot_rsc_job:pivot_job([Id], Context).

custom_pivot(#custom_pivot{id=Id}, _Context) ->
    case get({?MODULE, Id}) of
        undefined -> undefined;
        {result, Result} -> Result
    end.

job_manager() ->
    receive
        {'$gen_call', From, {pivot_done, _, _}} ->
            gen_server:reply(From, ok),
            job_manager();
        {'$gen_cast', {pivot_ping, _, _}} ->
            job_manager()
    end.
