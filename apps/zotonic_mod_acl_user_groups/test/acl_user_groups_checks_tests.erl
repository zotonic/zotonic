%% @hidden

-module(acl_user_groups_checks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


normalize_category_visibility_all_categories_test() ->
    Result = acl_user_groups_checks:test_normalize_category_visibility([
        {1, 0},
        {2, 0},
        {undefined, 1},
        {4, 1}
    ]),
    ?assertEqual(
        #{0 => [1, 2], 1 => all},
        normalize_result(Result)
    ).


normalize_category_visibility_explicit_categories_unchanged_test() ->
    Result = acl_user_groups_checks:test_normalize_category_visibility([
        {10, 0},
        {11, 0},
        {12, 1}
    ]),
    ?assertEqual(
        #{0 => [10, 11], 1 => [12]},
        normalize_result(Result)
    ).


visibility_cats_sql_all_categories_with_visibility_test() ->
    ?assertEqual(
        {"r.visible_for = $1", [0]},
        flatten_clause(acl_user_groups_checks:test_visibility_cats_sql(0, all, "r", []))
    ).


visibility_cats_sql_all_categories_without_visibility_test() ->
    ?assertEqual(
        {[], []},
        flatten_clause(acl_user_groups_checks:test_visibility_cats_sql(undefined, all, "r", []))
    ).


visibility_cats_sql_explicit_categories_unchanged_test() ->
    ?assertEqual(
        {"r.visible_for = $1 AND r.category_id = any($2::int[])", [0, [10, 11]]},
        flatten_clause(acl_user_groups_checks:test_visibility_cats_sql(0, [10, 11], "r", []))
    ).


normalize_result(Result) ->
    maps:map(
        fun (_Visibility, all) -> all;
            (_Visibility, CatIds) -> lists:sort(CatIds)
        end,
        maps:from_list(Result)
    ).


flatten_clause({Clause, Args}) ->
    {lists:flatten(Clause), Args}.

-endif.
