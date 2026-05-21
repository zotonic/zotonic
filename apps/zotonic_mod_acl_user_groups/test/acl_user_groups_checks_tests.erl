%% @hidden

-module(acl_user_groups_checks_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


normalize_category_visibility_all_categories_test() ->
    %% A view rule covering all categories (PropId = undefined) emits a sentinel
    %% entry with CatId = undefined in the ETS table.  await_match returns the
    %% sentinel alongside all the concretely-expanded category IDs.  The result
    %% for the visibility that has the sentinel must be 'all'.
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


normalize_category_visibility_all_with_expanded_ids_test() ->
    %% When await_match returns the sentinel (undefined) PLUS all the
    %% expanded concrete category IDs for the same visibility, the result
    %% must still be 'all' (not a huge list of IDs).
    Result = acl_user_groups_checks:test_normalize_category_visibility([
        {1, 1},
        {2, 1},
        {3, 1},
        {undefined, 1}
    ]),
    ?assertEqual(
        #{1 => all},
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
