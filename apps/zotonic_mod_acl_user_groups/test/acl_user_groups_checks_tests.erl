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


%% ---- restrict_viewable_cats ----

%% When SearchCats = all, all CG entries are returned unchanged.
restrict_viewable_cats_search_all_test() ->
    Lines = [
        {cg1, 0, [10, 11]},
        {cg1, 1, all},
        {cg2, 0, [20]}
    ],
    ?assertEqual(Lines, acl_user_groups_checks:test_restrict_viewable_cats(Lines, all)).

%% When the search restricts to a specific set of cats, entries whose category
%% list has no overlap with the search cats are dropped entirely.
restrict_viewable_cats_drops_non_overlapping_test() ->
    Lines = [
        {cg1, 0, [10, 11]},
        {cg2, 0, [20, 21]}
    ],
    Result = acl_user_groups_checks:test_restrict_viewable_cats(Lines, [10]),
    ?assertEqual([{cg1, 0, [10]}], Result).

%% When a CG entry covers 'all' categories, it is kept as-is even when
%% the search provides a specific cat list -- the category restriction is
%% handled upstream by the pivot_category_nr check.
restrict_viewable_cats_keeps_all_entry_test() ->
    Lines = [
        {cg1, 0, all},
        {cg2, 0, [20, 21]}
    ],
    Result = acl_user_groups_checks:test_restrict_viewable_cats(Lines, [10]),
    ?assertEqual([{cg1, 0, all}], Result).

%% Category lists in entries are intersected with the search cats.
restrict_viewable_cats_intersects_cat_list_test() ->
    Lines = [
        {cg1, 0, [10, 11, 12]},
        {cg2, 1, [11, 13]}
    ],
    Result = acl_user_groups_checks:test_restrict_viewable_cats(Lines, [11, 12]),
    ?assertEqual(
        [{cg1, 0, [11, 12]}, {cg2, 1, [11]}],
        Result
    ).

%% An empty Lines list returns empty.
restrict_viewable_cats_empty_lines_test() ->
    ?assertEqual([], acl_user_groups_checks:test_restrict_viewable_cats([], [10])).


%% ---- restrict_collab_cats ----

%% When SearchCats = all, all collab entries are returned unchanged.
restrict_collab_cats_search_all_test() ->
    Lines = [{0, [10, 11]}, {1, all}],
    ?assertEqual(Lines, acl_user_groups_checks:test_restrict_collab_cats(Lines, all)).

%% Entries with no category overlap with the search cats are dropped.
restrict_collab_cats_drops_non_overlapping_test() ->
    Lines = [{0, [10, 11]}, {1, [20]}],
    Result = acl_user_groups_checks:test_restrict_collab_cats(Lines, [10]),
    ?assertEqual([{0, [10]}], Result).

%% 'all'-category collab entries are kept regardless of search cats.
restrict_collab_cats_keeps_all_entry_test() ->
    Lines = [{0, all}, {1, [20]}],
    Result = acl_user_groups_checks:test_restrict_collab_cats(Lines, [10]),
    ?assertEqual([{0, all}], Result).

%% Category lists in collab entries are intersected with the search cats.
restrict_collab_cats_intersects_cat_list_test() ->
    Lines = [{0, [10, 11, 12]}, {1, [11, 13]}],
    Result = acl_user_groups_checks:test_restrict_collab_cats(Lines, [11, 12]),
    ?assertEqual([{0, [11, 12]}, {1, [11]}], Result).

-endif.
