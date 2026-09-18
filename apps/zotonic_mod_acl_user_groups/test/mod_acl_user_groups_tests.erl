%% @doc Tests for mod_acl_user_groups
%% @end
-module(mod_acl_user_groups_tests).
-moduledoc("
EUnit tests for ACL user-group authorization behavior.
").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    is_allowed_always_true/2
]).

tree_expand_test() ->
    [] = acl_user_groups_rules:tree_expand([]),
    [{1,[1]}, {2,[2]}] = acl_user_groups_rules:tree_expand([{1,[]}, {2,[]}]),
    [{1,[1]},{2,[2,3]},{3,[3]}] = acl_user_groups_rules:tree_expand([{1,[]}, {2,[{3,[]}]}]),
    ok.

normalize_category_visibility_all_categories_test() ->
    %% A view rule covering all categories (PropId = undefined) emits a sentinel
    %% entry with CatId = undefined in the ETS table.  await_match returns the
    %% sentinel alongside all the concretely-expanded category IDs.  The result
    %% for the visibility that has the sentinel must be 'all'.
    Result = acl_user_groups_checks:normalize_category_visibility([
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
    Result = acl_user_groups_checks:normalize_category_visibility([
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
    Result = acl_user_groups_checks:normalize_category_visibility([
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
        {<<"r.visible_for = $1">>, [0]},
        flatten_clause(acl_user_groups_checks:visibility_cats_sql(0, all, "r", []))
    ).


visibility_cats_sql_all_categories_without_visibility_test() ->
    ?assertEqual(
        {<<>>, []},
        flatten_clause(acl_user_groups_checks:visibility_cats_sql(undefined, all, "r", []))
    ).


visibility_cats_sql_explicit_categories_unchanged_test() ->
    ?assertEqual(
        {<<"r.visible_for = $1 AND r.category_id = any($2::int[])">>, [0, [10, 11]]},
        flatten_clause(acl_user_groups_checks:visibility_cats_sql(0, [10, 11], "r", []))
    ).

visibility_cats_sql_no_categories_test() ->
    ?assertEqual(
        {<<"false">>, []},
        flatten_clause(acl_user_groups_checks:visibility_cats_sql(0, [], "r", []))
    ).

normalize_result(Result) ->
    maps:map(
        fun (_Visibility, all) -> all;
            (_Visibility, CatIds) -> lists:sort(CatIds)
        end,
        maps:from_list(Result)
    ).


flatten_clause({Clause, Args}) ->
    {iolist_to_binary(Clause), Args}.


%% ---- restrict_viewable_cats ----

%% When SearchCats = all, all CG entries are returned unchanged.
restrict_viewable_cats_search_all_test() ->
    Lines = [
        {cg1, 0, [10, 11]},
        {cg1, 1, all},
        {cg2, 0, [20]}
    ],
    ?assertEqual(Lines, acl_user_groups_checks:restrict_viewable_cats(Lines, all)).

%% When the search restricts to a specific set of cats, entries whose category
%% list has no overlap with the search cats are dropped entirely.
restrict_viewable_cats_drops_non_overlapping_test() ->
    Lines = [
        {cg1, 0, [10, 11]},
        {cg2, 0, [20, 21]}
    ],
    Result = acl_user_groups_checks:restrict_viewable_cats(Lines, [10]),
    ?assertEqual([{cg1, 0, [10]}], Result).

%% When a CG entry covers 'all' categories, it is kept as-is even when
%% the search provides a specific cat list -- the category restriction is
%% handled upstream by the pivot_category_nr check.
restrict_viewable_cats_keeps_all_entry_test() ->
    Lines = [
        {cg1, 0, all},
        {cg2, 0, [20, 21]}
    ],
    Result = acl_user_groups_checks:restrict_viewable_cats(Lines, [10]),
    ?assertEqual([{cg1, 0, all}], Result).

%% Category lists in entries are intersected with the search cats.
restrict_viewable_cats_intersects_cat_list_test() ->
    Lines = [
        {cg1, 0, [10, 11, 12]},
        {cg2, 1, [11, 13]}
    ],
    Result = acl_user_groups_checks:restrict_viewable_cats(Lines, [11, 12]),
    ?assertEqual(
        [{cg1, 0, [11, 12]}, {cg2, 1, [11]}],
        Result
    ).

%% An empty Lines list returns empty.
restrict_viewable_cats_empty_lines_test() ->
    ?assertEqual([], acl_user_groups_checks:restrict_viewable_cats([], [10])).


%% ---- restrict_collab_cats ----

%% When SearchCats = all, all collab entries are returned unchanged.
restrict_collab_cats_search_all_test() ->
    Lines = [{0, [10, 11]}, {1, all}],
    ?assertEqual(Lines, acl_user_groups_checks:restrict_collab_cats(Lines, all)).

%% Entries with no category overlap with the search cats are dropped.
restrict_collab_cats_drops_non_overlapping_test() ->
    Lines = [{0, [10, 11]}, {1, [20]}],
    Result = acl_user_groups_checks:restrict_collab_cats(Lines, [10]),
    ?assertEqual([{0, [10]}], Result).

%% 'all'-category collab entries are kept regardless of search cats.
restrict_collab_cats_keeps_all_entry_test() ->
    Lines = [{0, all}, {1, [20]}],
    Result = acl_user_groups_checks:restrict_collab_cats(Lines, [10]),
    ?assertEqual([{0, all}], Result).

%% Category lists in collab entries are intersected with the search cats.
restrict_collab_cats_intersects_cat_list_test() ->
    Lines = [{0, [10, 11, 12]}, {1, [11, 13]}],
    Result = acl_user_groups_checks:restrict_collab_cats(Lines, [11, 12]),
    ?assertEqual([{0, [11, 12]}, {1, [11]}], Result).

person_can_edit_own_resource_test() ->
    ContextAnon = context(),
    ContextSudo = z_acl:sudo(ContextAnon),
    with_test_group(
        ContextSudo,
        fun(UserGroupId) ->
            with_managed_rules(
                [
                    {rsc, [
                        {acl_user_group_id, UserGroupId},
                        {actions, [view, update]},
                        {is_owner, true},
                        {category_id, person}
                    ]}
                ],
                ContextAnon,
                fun() ->
                    with_resources(
                        [
                            #{ <<"category_id">> => person },
                            #{ <<"category_id">> => person, <<"creator_id">> => self },
                            #{ <<"category_id">> => person, <<"creator_id">> => self }
                        ],
                        ContextSudo,
                        fun([UserId1, UserId2, UserId3]) ->
                            {ok, UserId2} = m_rsc:update(
                                UserId2,
                                #{ <<"creator_id">> => UserId1 },
                                ContextSudo),
                            {ok, _} = m_edge:insert(
                                UserId1, hasusergroup, UserGroupId, ContextSudo),
                            {ok, _} = m_edge:insert(
                                UserId2, hasusergroup, UserGroupId, ContextSudo),
                            {ok, _} = m_edge:insert(
                                UserId3, hasusergroup, UserGroupId, ContextSudo),
                            ContextUser1 = z_acl:logon(UserId1, ContextAnon),
                            ContextUser3 = z_acl:logon(UserId3, ContextAnon),

                            ?assertEqual(
                                {error, eacces},
                                m_rsc:update(
                                    UserId1,
                                    [{title, <<"Test">>}],
                                    ContextAnon)),
                            ?assertEqual(
                                {error, eacces},
                                m_rsc:update(
                                    UserId1,
                                    [{title, <<"Test">>}],
                                    ContextUser3)),
                            ?assertEqual(
                                {error, eacces},
                                m_rsc:update(
                                    UserId2,
                                    [{title, <<"Test">>}],
                                    ContextUser3)),

                            {ok, _} = m_rsc:update(
                                UserId1, [{title, "Test"}], ContextUser1),
                            {ok, _} = m_rsc:update(
                                UserId2, [{title, "Test"}], ContextUser1),
                            {ok, _} = m_rsc:update(
                                UserId3, [{title, "Test"}], ContextUser3)
                        end)
                end)
        end).


person_can_insert_text_in_default_content_group_only_test() ->
    Context = context(),
    SudoContext = z_acl:sudo(Context),
    with_test_group(
        SudoContext,
        fun(UserGroupId) ->
            with_managed_rules(
                [
                    {rsc, [
                        {acl_user_group_id, UserGroupId},
                        {content_group_id, default_content_group},
                        {actions, [insert]},
                        {is_owner, true},
                        {category_id, article}
                    ]},
                    {rsc, [
                        {acl_user_group_id, UserGroupId},
                        {actions, [view]}
                    ]}
                ],
                Context,
                fun() ->
                    with_resource(
                        [{category, person}],
                        SudoContext,
                        SudoContext,
                        fun(UserId) ->
                            {ok, _} = m_edge:insert(
                                UserId,
                                hasusergroup,
                                UserGroupId,
                                SudoContext),
                            UserContext = z_acl:logon(UserId, Context),
                            DefaultContentGroupId = m_rsc:p(
                                default_content_group, id, Context),
                            with_resource(
                                [
                                    {category, article},
                                    {content_group_id, DefaultContentGroupId}
                                ],
                                UserContext,
                                SudoContext,
                                fun(_TextId) ->
                                    SystemContentGroupId = m_rsc:p(
                                        system_content_group, id, Context),
                                    ?assertEqual(
                                        {error, eacces},
                                        m_rsc:insert(
                                            [
                                                {category, article},
                                                {content_group_id, SystemContentGroupId}
                                            ],
                                            UserContext))
                                end)
                        end)
                end)
        end).


acl_is_allowed_accepts_rsc_name_object_test() ->
    ?assertEqual(false, acl_user_groups_checks:acl_is_allowed(#acl_is_allowed{action = insert, object = text}, context())).

%% @doc See https://github.com/zotonic/zotonic/issues/1306
acl_is_allowed_override_test() ->
    ContextAnon = context(),
    ContextSudo = z_acl:sudo(ContextAnon),
    with_resource(
        [{category_id, person}],
        ContextSudo,
        ContextSudo,
        fun(UserId) ->
            {ok, _} = m_edge:insert(
                UserId,
                hasusergroup,
                acl_user_group_anonymous,
                ContextSudo),
            ContextUser = z_acl:logon(UserId, ContextAnon),
            ok = z_notifier:observe(
                acl_is_allowed,
                {?MODULE, is_allowed_always_true},
                10,
                ContextAnon),
            try
                with_resource(
                    [{category_id, text}],
                    ContextUser,
                    ContextSudo,
                    fun(TextId) ->
                        ?assertEqual(
                            m_rsc:rid(default_content_group, ContextSudo),
                            m_rsc:p_no_acl(
                                TextId,
                                content_group_id,
                                ContextSudo)),
                        ?assert(z_acl:rsc_visible(TextId, ContextAnon)),
                        ?assertNot(z_acl:rsc_editable(TextId, ContextAnon)),
                        ?assert(z_acl:rsc_visible(TextId, ContextUser)),
                        ?assert(z_acl:rsc_editable(TextId, ContextUser))
                    end)
            after
                z_notifier:detach(acl_is_allowed, ContextAnon)
            end
        end).

publish_test() ->
    Context = context(),
    with_managed_rules(
        [
            {rsc, [
                {acl_user_group_id, acl_user_group_anonymous},
                {actions, [view]}
            ]}
        ],
        Context,
        fun() ->
            SudoContext = z_acl:sudo(Context),
            with_resource(
                [
                    {is_published, false},
                    {title, <<"Top secret!">>},
                    {category, text}
                ],
                SudoContext,
                SudoContext,
                fun(Id) ->
                    ?assertEqual(
                        <<"Top secret!">>,
                        m_rsc:p(Id, title, SudoContext)),
                    ?assertNot(z_acl:rsc_visible(Id, Context)),
                    ?assertEqual(undefined, m_rsc:p(Id, title, Context)),
                    {ok, Id} = m_rsc:update(
                        Id,
                        [{is_published, true}],
                        SudoContext),
                    ?assertEqual(<<"Top secret!">>, m_rsc:p(Id, title, Context))
                end)
        end).

context() ->
    Context = z_context:new(zotonic_site_testsandbox),
    start_modules(Context),
    Context.

start_modules(Context) ->
    ok = z_module_manager:activate_await(mod_content_groups, Context),
    ok = z_module_manager:activate_await(mod_acl_user_groups, Context),
    ok = z_module_manager:upgrade_await(Context).

is_allowed_always_true(#acl_is_allowed{}, _Context) ->
    true.

replace_managed(Rules, Context) ->
    SudoContext = z_acl:sudo(Context),
    ok = m_acl_rule:replace_managed(
        Rules,
        ?MODULE,
        SudoContext),
    await_acl_rebuild(SudoContext).

with_managed_rules(Rules, Context, Fun) ->
    replace_managed(Rules, Context),
    try
        Fun()
    after
        replace_managed([], Context)
    end.

with_resources(PropsList, Context, Fun) ->
    with_resources(PropsList, Context, [], Fun).

with_resources([], _Context, Ids, Fun) ->
    Fun(lists:reverse(Ids));
with_resources([Props | Rest], Context, Ids, Fun) ->
    {ok, Id} = m_rsc:insert(Props, Context),
    try
        with_resources(Rest, Context, [Id | Ids], Fun)
    after
        ok = m_rsc:delete(Id, Context)
    end.

with_resource(Props, InsertContext, DeleteContext, Fun) ->
    {ok, Id} = m_rsc:insert(Props, InsertContext),
    try
        Fun(Id)
    after
        ok = m_rsc:delete(Id, DeleteContext)
    end.

await_acl_rebuild(Context) ->
    await_acl_rebuild(1000, Context).

await_acl_rebuild(0, _Context) ->
    error(acl_rebuild_timeout);
await_acl_rebuild(N, Context) ->
    {ok, Status} = mod_acl_user_groups:status(Context),
    case {
        proplists:get_value(is_rebuilding, Status),
        proplists:get_value(is_rebuild_publish, Status)
    } of
        {false, false} ->
            ok;
        _ ->
            timer:sleep(10),
            await_acl_rebuild(N - 1, Context)
    end.

with_test_group(Context, Fun) ->
    with_resource(
        [
            {is_published, true},
            {category_id, acl_user_group},
            {title, <<"Test user group">>}
        ],
        Context,
        Context,
        Fun).
