-module(z_dependent_cleanup_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

dependent_cleanup_test_() ->
    z_test_support:isolated({foreach, fun setup/0, fun cleanup/1, [
        fun ignored_only_schedules/0,
        fun retaining_edge_does_not_schedule/0,
        fun nondependent_does_not_schedule/0,
        fun delete_unconnected/0,
        fun keep_connected_or_protected/0,
        fun unknown_resource/0,
        fun predicate_defaults_and_cache/0
    ]}).

setup() ->
    Modules = [z_db, m_rsc, m_predicate, z_pivot_rsc, z_acl, z_context, z_depcache],
    [meck:new(M, [passthrough, no_link]) || M <- Modules],
    meck:expect(m_rsc, rid, fun(Id, _) -> Id end),
    meck:expect(m_rsc, p_no_acl, fun(_, <<"is_dependent">>, _) -> true end),
    meck:expect(m_predicate, dependency_ignored, fun(_) -> [20, 21] end),
    meck:expect(z_pivot_rsc, insert_task_after, fun(_, _, _, _, _, _) -> {ok, 1} end),
    meck:expect(z_acl, sudo, fun(C) -> C end),
    meck:expect(z_context, set, fun(is_dependent_delete, true, C) -> C end),
    meck:expect(m_rsc, delete, fun(_, _) -> ok end),
    Modules.

cleanup(Modules) ->
    [meck:unload(M) || M <- Modules].

ignored_only_schedules() ->
    expect_connection(undefined),
    ?assertEqual(ok, z_edge_log_server:maybe_schedule_dependent_check(10, #context{})),
    ?assert(meck:called(z_pivot_rsc, insert_task_after,
        [43200, z_edge_log_server, delete_if_unconnected, <<"10">>, [10], '_'])).

retaining_edge_does_not_schedule() ->
    expect_connection(123),
    ?assertEqual(ok, z_edge_log_server:maybe_schedule_dependent_check(10, #context{})),
    ?assertEqual(0, meck:num_calls(z_pivot_rsc, insert_task_after, '_')).

nondependent_does_not_schedule() ->
    meck:expect(m_rsc, p_no_acl, fun(_, _, _) -> false end),
    ?assertEqual(ok, z_edge_log_server:maybe_schedule_dependent_check(10, #context{})),
    ?assertEqual(0, meck:num_calls(z_db, q1, '_')).

expect_connection(Result) ->
    meck:expect(z_db, q1, fun(Sql, [10, [20, 21]], _) ->
        ?assert(string:find(Sql, "object_id = $1") =/= nomatch),
        ?assert(string:find(Sql, "not (predicate_id = any($2::integer[]))") =/= nomatch),
        Result
    end).

delete_unconnected() ->
    expect_cleanup(10),
    ?assertEqual(ok, z_edge_log_server:delete_if_unconnected(10, #context{})),
    ?assert(meck:called(m_rsc, delete, [10, '_'])).

keep_connected_or_protected() ->
    expect_cleanup(undefined),
    ?assertEqual(ok, z_edge_log_server:delete_if_unconnected(10, #context{})),
    ?assertEqual(0, meck:num_calls(m_rsc, delete, '_')).

expect_cleanup(Result) ->
    meck:expect(z_db, q1, fun(Sql, [10, [20, 21]], _) ->
        [ ?assert(string:find(Sql, Part) =/= nomatch) || Part <-
            ["r.is_dependent", "not r.is_protected", "not exists",
             "e.object_id = r.id", "not (e.predicate_id = any($2::integer[]))"] ],
        Result
    end).

unknown_resource() ->
    meck:expect(m_rsc, rid, fun(_, _) -> undefined end),
    ?assertNot(m_edge:has_dependency_subjects(unknown, #context{})),
    ?assertEqual(0, meck:num_calls(z_db, q1, '_')).

predicate_defaults_and_cache() ->
    meck:delete(m_predicate, dependency_ignored, 1),
    meck:expect(z_depcache, memo, fun(_F, predicate, _Age, _Context) ->
        [{a, [{id, 20}, {is_dependency_ignored, true}]},
         {b, [{id, 21}, {is_dependency_ignored, false}]},
         {c, [{id, 22}]}]
    end),
    meck:expect(z_depcache, memo, fun(F, predicate_dependency_ignored, _, [predicate, category], _) -> F() end),
    ?assertEqual([20], m_predicate:dependency_ignored(#context{})).
