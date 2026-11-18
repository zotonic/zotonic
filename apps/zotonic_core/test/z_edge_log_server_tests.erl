-module(z_edge_log_server_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

edge_log_test_() ->
    z_test_support:isolated({foreach, fun setup/0, fun cleanup/1, [
        fun deleted_insert/0,
        fun existing_insert/0,
        fun deleted_update/0,
        fun existing_update/0,
        fun deleted_update_followed_by_delete/0,
        fun deleted_insert_followed_by_delete/0
    ]}).

setup() ->
    Modules = [z_context, z_acl, z_db, z_depcache, m_rsc, z_notifier, z_mqtt],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    meck:expect(z_context, new, fun(edge_log_test) -> #context{} end),
    meck:expect(z_acl, sudo, fun(C) -> C end),
    meck:expect(z_depcache, flush, fun(_, _) -> ok end),
    meck:expect(m_rsc, rid, fun(relation, _) -> 10 end),
    meck:expect(m_rsc, p_no_acl, fun(_, <<"is_dependent">>, _) -> false end),
    meck:expect(z_db, transaction, fun(F, C) -> F(C) end),
    meck:expect(z_notifier, notify_sync, fun(_, _) -> ok end),
    meck:expect(z_mqtt, publish, fun(_, _, _) -> ok end),
    Modules.

cleanup(Modules) ->
    lists:foreach(fun meck:unload/1, Modules).

deleted_insert() ->
    deleted_event(<<"INSERT">>).

deleted_update() ->
    deleted_event(<<"UPDATE">>).

deleted_event(Op) ->
    check_rows([{1, Op, 20, <<"relation">>, 30, 40,
        undefined, undefined, undefined}]),
    ?assertEqual(0, meck:num_calls(z_notifier, notify_sync, '_')),
    ?assertEqual(0, meck:num_calls(z_mqtt, publish, '_')),
    ?assert(meck:called(z_depcache, flush, [20, '_'])),
    ?assert(meck:called(z_depcache, flush, [30, '_'])).

existing_insert() ->
    Created = {{2026, 11, 18}, {12, 0, 0}},
    %% A deleted creator does not mean the edge itself has been deleted.
    check_rows([{1, <<"INSERT">>, 20, <<"relation">>, 30, 40,
        7, undefined, Created}]),
    Edge = #edge_insert{subject_id = 20, predicate = relation, object_id = 30,
        edge_id = 40, seq = 7, creator_id = undefined, created = Created},
    ?assertEqual(1, meck:num_calls(z_notifier, notify_sync, '_')),
    ?assert(meck:called(z_notifier, notify_sync, [Edge, '_'])),
    assert_published([Edge]).

existing_update() ->
    check_rows([{1, <<"UPDATE">>, 20, <<"relation">>, 30, 40,
        7, undefined, {{2026, 11, 18}, {12, 0, 0}}}]),
    Edge = #edge_update{subject_id = 20, predicate = relation, object_id = 30, edge_id = 40},
    ?assertEqual(1, meck:num_calls(z_notifier, notify_sync, '_')),
    ?assert(meck:called(z_notifier, notify_sync, [Edge, '_'])),
    assert_published([Edge]).

deleted_insert_followed_by_delete() ->
    deleted_event_followed_by_delete(<<"INSERT">>).

deleted_update_followed_by_delete() ->
    deleted_event_followed_by_delete(<<"UPDATE">>).

deleted_event_followed_by_delete(Op) ->
    check_rows([
        {1, Op, 20, <<"relation">>, 30, 40, undefined, undefined, undefined},
        {2, <<"DELETE">>, 20, <<"relation">>, 30, 40, undefined, undefined, undefined}
    ]),
    Edge = #edge_delete{subject_id = 20, predicate = relation, object_id = 30, edge_id = 40},
    ?assertEqual(1, meck:num_calls(z_notifier, notify_sync, '_')),
    ?assert(meck:called(z_notifier, notify_sync, [Edge, '_'])),
    assert_published([Edge]).

check_rows(Rows) ->
    meck:expect(z_db, q, fun
        (_, [100], _) -> Rows;
        ("delete from edge_log where id = $1", [1], _) -> 1;
        ("delete from edge_log where id >= $1 and id <= $2", [1, 2], _) -> 2
    end),
    ?assertMatch({noreply, _, 100}, z_edge_log_server:handle_cast(check, {state, edge_log_test})),
    %% Every fetched row, including skipped inserts and updates, is removed from the log.
    case length(Rows) of
        1 -> ?assert(meck:called(z_db, q, ["delete from edge_log where id = $1", [1], '_']));
        2 -> ?assert(meck:called(z_db, q,
            ["delete from edge_log where id >= $1 and id <= $2", [1, 2], '_']))
    end.

assert_published(Events) ->
    ?assertEqual(2, meck:num_calls(z_mqtt, publish, '_')),
    ?assert(meck:called(z_mqtt, publish,
        [[<<"model">>, <<"edge">>, <<"event">>, 20, <<"o">>, <<"relation">>], Events, '_'])),
    ?assert(meck:called(z_mqtt, publish,
        [[<<"model">>, <<"edge">>, <<"event">>, 30, <<"s">>, <<"relation">>], Events, '_'])).
