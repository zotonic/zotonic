%% @doc Migration scheduling must respect schema commits and backup environments.
-module(backup_gone_migration_tests).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

migration_scheduling_test() ->
    Context = #context{},
    Modules = [z_db, m_site, z_pivot_rsc],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    try
        meck:expect(z_db, q, fun(_, _) -> [] end),
        meck:expect(z_db, flush, fun(_) -> ok end),
        meck:expect(z_db, has_connection, fun(_) -> true end),
        meck:expect(z_db, table_exists, fun(backup_gone_migration, _) -> true end),
        meck:expect(m_site, environment, fun(_) -> backup end),
        meck:expect(z_pivot_rsc, insert_task,
            fun(backup_gone_migration, migrate, <<>>, [0], _) -> {ok, 1} end),

        % Schema work persists progress without scheduling on another connection.
        ?assertEqual(ok, backup_gone_migration:start(Context)),
        ?assertEqual(ok, mod_backup:manage_data({upgrade, 6}, Context)),
        ?assertNot(meck:called(z_pivot_rsc, insert_task, '_')),

        % Normal startup resumes the progress left by a backup-mode upgrade.
        meck:expect(m_site, environment, fun(_) -> production end),
        ?assertEqual(ok, backup_gone_migration:resume(Context)),
        ?assertEqual(1, meck:num_calls(z_pivot_rsc, insert_task, '_')),

        % The post-commit callback also schedules a normal upgrade.
        ?assertEqual(ok, mod_backup:manage_data({upgrade, 6}, Context)),
        ?assertEqual(2, meck:num_calls(z_pivot_rsc, insert_task, '_')),

        meck:expect(z_db, table_exists, fun(backup_gone_migration, _) -> false end),
        ?assertEqual(ok, backup_gone_migration:resume(Context)),
        ?assertEqual(2, meck:num_calls(z_pivot_rsc, insert_task, '_')),
        ?assert(lists:all(fun meck:validate/1, Modules))
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

-endif.
