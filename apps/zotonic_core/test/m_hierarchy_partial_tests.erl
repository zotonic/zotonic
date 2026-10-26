-module(m_hierarchy_partial_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

merge_test_() ->
    [
        {"empty declaration preserves the tree", ?_assertEqual(
            {ok, [{1, [{2, []}]}]},
            m_hierarchy:merge_partial_tree([{1, [{2, []}]}], []))},
        {"empty children preserve descendants and the anchor's parent", ?_assertEqual(
            {ok, [{1, [{2, [{3, []}]}]}]},
            m_hierarchy:merge_partial_tree([{1, [{2, [{3, []}]}]}], [{2, []}]))},
        {"moving a subtree preserves unrelated nodes and descendants", ?_assertEqual(
            {ok, [{1, [{2, []}, {4, [{5, []}]}]}, {3, []}]},
            m_hierarchy:merge_partial_tree(
                [{1, [{2, []}]}, {3, [{4, [{5, []}]}]}], [{1, [{4, []}]}]))},
        {"new roots and nested nodes follow declaration order", ?_assertEqual(
            {ok, [{1, []}, {3, [{4, [{5, []}]}, {2, []}]}]},
            m_hierarchy:merge_partial_tree([{1, []}], [{3, [{4, [{5, []}]}, {2, []}]}]))},
        {"existing siblings keep their order", ?_assertEqual(
            {ok, [{1, [{2, []}, {3, []}, {5, []}, {4, []}]}]},
            m_hierarchy:merge_partial_tree(
                [{1, [{2, []}, {3, []}]}], [{1, [{3, []}, {5, []}, {2, []}, {4, []}]}]))},
        {"duplicate ids are rejected", ?_assertEqual(
            {error, {duplicate_id, 2}},
            m_hierarchy:merge_partial_tree([], [{1, [{2, []}]}, {2, []}]))},
        {"malformed children are rejected", ?_assertEqual(
            {error, invalid_tree}, m_hierarchy:merge_partial_tree([], [{1, invalid}]))},
        {"cycles through existing ancestry are rejected", ?_assertMatch(
            {error, {cycle, _}},
            m_hierarchy:merge_partial_tree([{1, [{2, [{3, []}]}]}], [{3, [{1, []}]}]))},
        {"final graph is checked, not intermediate moves", ?_assertEqual(
            {ok, [{3, [{2, [{1, []}]}]}]},
            m_hierarchy:merge_partial_tree(
                [{1, [{2, []}]}, {3, []}], [{3, [{2, [{1, []}]}]}]))}
    ].

idempotent_test() ->
    Current = [{1, [{2, []}]}, {3, [{4, [{5, []}]}]}],
    Partial = [{1, [{4, []}, {6, []}]}],
    {ok, Merged} = m_hierarchy:merge_partial_tree(Current, Partial),
    ?assertEqual({ok, Merged}, m_hierarchy:merge_partial_tree(Merged, Partial)).

save_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun() ->
            meck:expect(z_acl, is_admin, fun(_) -> false end),
            ?assertEqual({error, eacces}, save([{1, []}])),
            ?assertEqual(0, meck:num_calls(z_db, transaction, '_'))
        end,
        fun() ->
            ?assertEqual({error, {unknown_resource, missing}}, save([{missing, []}])),
            ?assertEqual({error, {invalid_category, wrong}}, save([{wrong, []}])),
            ?assertEqual({error, invalid_tree}, save([{1, invalid}])),
            ?assertEqual(0, meck:num_calls(z_db, transaction, '_'))
        end,
        fun() ->
            ?assertEqual({error, {duplicate_id, 1}}, save([{1, []}, {member, []}])),
            ?assertEqual(0, meck:num_calls(z_notifier, notify, '_'))
        end,
        fun() ->
            ?assertMatch({error, {cycle, _}}, save([{2, [{1, []}]}])),
            ?assertEqual(0, meck:num_calls(z_notifier, notify, '_')),
            % Only the initial tree read; no persistence queries.
            ?assertEqual(1, meck:num_calls(z_db, q, ['_', '_', '_']))
        end,
        fun() ->
            ?assertEqual(ok, save([{member, []}])),
            ?assertEqual(0, meck:num_calls(z_notifier, notify, '_'))
        end,
        fun() ->
            ?assertEqual(ok, save([{member, [{vip, []}]}])),
            ?assert(meck:called(z_depcache, flush, [{hierarchy, <<"acl_user_group">>}, '_'])),
            ?assert(meck:called(z_notifier, notify, [#hierarchy_updated{
                root_id = <<"acl_user_group">>, predicate = undefined,
                inserted_ids = [3], deleted_ids = []
            }, '_']))
        end,
        fun() ->
            meck:expect(z_db, transaction, fun(_, _) -> {rollback, test_failure} end),
            ?assertEqual({error, test_failure}, save([{1, [{3, []}]}])),
            ?assertEqual(0, meck:num_calls(z_depcache, flush, '_')),
            ?assertEqual(0, meck:num_calls(z_notifier, notify, '_'))
        end
    ]}.

save(Tree) ->
    m_hierarchy:save_partial_tree(acl_user_group, acl_user_group, Tree,
        #context{db = {test, z_db_pgsql}}).

setup() ->
    meck:new([z_acl, m_category, m_rsc, z_db, z_depcache, z_notifier], [no_link]),
    meck:expect(z_acl, is_admin, fun(_) -> true end),
    meck:expect(m_category, name_to_id, fun(acl_user_group, _) -> {ok, 10} end),
    meck:expect(m_rsc, rid, fun
        (member, _) -> 1;
        (vip, _) -> 3;
        (wrong, _) -> 4;
        (Id, _) when is_integer(Id) -> Id;
        (_, _) -> undefined
    end),
    meck:expect(m_rsc, is_a, fun(Id, 10, _) -> Id =/= 4 end),
    meck:expect(z_db, transaction, fun(F, Context) -> F(Context) end),
    meck:expect(z_db, q, fun("lock table hierarchy in share row exclusive mode", _) -> [] end),
    meck:expect(z_db, q, fun(Sql, _, _) ->
        case string:trim(Sql) of
            "select id, parent_id, lvl, lft, rght" ++ _ ->
                [{1, undefined, 1, 100, 200}, {2, 1, 2, 200, 200}];
            "select id, parent_id, lvl, nr" ++ _ ->
                [{1, undefined, 1, 100}, {2, 1, 2, 200}];
            _ -> []
        end
    end),
    meck:expect(z_depcache, flush, fun(_, _) -> ok end),
    meck:expect(z_notifier, notify, fun(_, _) -> ok end).

cleanup(_) ->
    meck:unload([z_acl, m_category, m_rsc, z_db, z_depcache, z_notifier]).
