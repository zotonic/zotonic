%% @author Marc Worrell
%% @hidden
-module(z_db_upsert_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/zotonic.hrl").

upsert_test_() ->
    {timeout, 60, fun() ->
        ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
        Context = z_context:new(zotonic_site_testsandbox),
        lists:foreach(fun(PropsCols) -> check_upsert(PropsCols, Context) end, [
            [],
            [#column_def{name=props, type="bytea"}],
            [#column_def{name=props_json, type="jsonb"}],
            [#column_def{name=props_json, type="bytea"}],
            [#column_def{name=props, type="bytea"}, #column_def{name=props_json, type="jsonb"}]
        ])
    end}.

check_upsert(PropsCols, Context) ->
    Table = db_upsert_test,
    Columns = [
        #column_def{name=id, type="serial", primary_key=true, is_nullable=false},
        #column_def{name=name, type="text", unique=true},
        #column_def{name=value, type="integer"},
        #column_def{name=tags, type="text", is_array=true}
    ] ++ PropsCols,
    ok = z_db:create_table(Table, Columns, Context),
    try
        Options = [{conflict, update}],
        ?assertEqual({ok, 10}, z_db:insert(Table, #{id => 10, name => <<"first">>, value => 1, tags => [first]}, Options, Context)),
        ?assertEqual([<<"first">>], z_db:q1("select tags from db_upsert_test where id = 10", Context)),
        ?assertEqual({ok, 10}, z_db:insert(Table, #{id => 10, tags => [second]}, Options, Context)),
        ?assertEqual([<<"second">>], z_db:q1("select tags from db_upsert_test where id = 10", Context)),
        ?assertEqual({ok, 10}, z_db:insert(Table, #{id => 10, value => 2}, Options, Context)),
        ?assertEqual({ok, 10}, z_db:insert(Table, #{id => 10}, Options, Context)),
        ?assertEqual({ok, <<"first">>}, z_db:insert(Table, #{id => 10}, [{returning, name} | Options], Context)),
        ?assertEqual({ok, undefined}, z_db:insert(Table, #{id => 10}, [{conflict, ignore}], Context)),
        ?assertEqual({error, badarg}, z_db:insert(Table, #{id => 10},
            [{conflict_target, [missing]} | Options], Context)),
        ByName = [{conflict_target, [name]} | Options],
        ?assertEqual({ok, 10}, z_db:insert(Table, #{id => 20, name => <<"first">>, value => 3}, ByName, Context)),
        ?assertEqual(3, z_db:q1("select value from db_upsert_test where id = 10", Context)),
        case PropsCols of
            [] -> ok;
            _ -> check_props(Table, PropsCols, Options, ByName, Context)
        end
    after
        ok = z_db:drop_table(Table, Context)
    end.

check_props(Table, PropsCols, Options, ByName, Context) ->
    % New inserts retain insert filtering and may use a generated id.
    {ok, NewId} = z_db:insert(Table, #{name => <<"new">>, empty => <<>>}, Options, Context),
    {ok, NewRow} = z_db:select(Table, NewId, Context),
    ?assertNot(maps:is_key(<<"empty">>, NewRow)),
    ?assertEqual({ok, 10}, z_db:insert(Table,
        #{id => 10, keep => 1, remove => 2}, Options, Context)),
    ?assertEqual({ok, <<"first">>}, z_db:insert(Table,
        #{id => 20, name => <<"first">>, remove => undefined, empty => <<>>, added => 3},
        [{returning, name} | ByName], Context)),
    {ok, Row} = z_db:select(Table, 10, Context),
    ?assertEqual(1, maps:get(<<"keep">>, Row)),
    ?assertEqual(3, maps:get(<<"added">>, Row)),
    ?assertEqual(<<>>, maps:get(<<"empty">>, Row)),
    ?assertNot(maps:is_key(<<"remove">>, Row)),
    case length(PropsCols) of
        2 ->
            z_db:q("update db_upsert_test set props = $1 where id = 10",
                [?DB_PROPS(#{<<"legacy">> => true})], Context),
            ?assertEqual({ok, 10}, z_db:insert(Table, #{id => 10, extra => true}, Options, Context)),
            {ok, Migrated} = z_db:select(Table, 10, Context),
            ?assertEqual(true, maps:get(<<"legacy">>, Migrated)),
            ?assertEqual(undefined, z_db:q1("select props from db_upsert_test where id = 10", Context));
        _ -> ok
    end,
    % An outer rollback also undoes the fallback's nested transaction.
    ?assertEqual({rollback, test}, z_db:transaction(fun(Ctx) ->
        {ok, 10} = z_db:insert(Table, #{id => 10, keep => 99}, Options, Ctx),
        {rollback, test}
    end, Context)),
    {ok, Unchanged} = z_db:select(Table, 10, Context),
    ?assertEqual(1, maps:get(<<"keep">>, Unchanged)),
    % Errors during the fallback must roll back and retain the old properties.
    {ok, OtherId} = z_db:insert(Table, #{name => <<"other">>}, Context),
    ?assertMatch({error, _}, z_db:insert(Table,
        #{id => 10, name => <<"other">>, keep => 99}, Options, Context)),
    {ok, AfterError} = z_db:select(Table, 10, Context),
    ?assertEqual(1, maps:get(<<"keep">>, AfterError)),
    ?assertEqual(<<"first">>, maps:get(<<"name">>, AfterError)),
    {ok, 1} = z_db:delete(Table, OtherId, Context),
    Parent = self(),
    Refs = [begin
        Ref = make_ref(),
        spawn(fun() ->
            Props = #{id => 10, integer_to_binary(N) => N},
            Parent ! {Ref, z_db:insert(Table, Props, Options, Context)}
        end),
        Ref
    end || N <- lists:seq(1, 8)],
    lists:foreach(fun(Ref) ->
        receive {Ref, Result} -> ?assertEqual({ok, 10}, Result)
        after 10000 -> error(upsert_timeout)
        end
    end, Refs),
    {ok, Merged} = z_db:select(Table, 10, Context),
    lists:foreach(fun(N) -> ?assertEqual(N, maps:get(integer_to_binary(N), Merged)) end, lists:seq(1, 8)).
