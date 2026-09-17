%% @doc Regression coverage for tombstones consumed by concurrent restores.
-module(m_backup_restore_tests).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

restore_missing_tombstone_test_() ->
    {timeout, 60, fun restore_missing_tombstone/0}.

restore_missing_tombstone() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_module_manager:activate_await(mod_backup, Context),
    restore_missing_tombstone(Context).

restore_missing_tombstone(Context) ->
    {ok, Id} = m_rsc:insert(#{ <<"category_id">> => article,
        <<"title">> => <<"Original">> }, Context),
    ok = m_rsc:delete(Id, Context),
    Rev = revision(Id, Context),
    meck:new(z_db, [passthrough, no_link]),
    meck:expect(z_db, q, fun
        ("select id from rsc_gone where id = $1 for update", [RscId], _Ctx)
            when RscId =:= Id ->
            % PostgreSQL returns no row when another restore deletes it while we wait.
            [];
        (Sql, Args, Ctx) ->
            meck:passthrough([Sql, Args, Ctx])
    end),
    try
        ?assertEqual({error, enoent}, m_backup_revision:revert_resource(Id, Rev, [], Context)),
        ?assertNot(m_rsc:exists(Id, Context)),
        ?assert(m_rsc_gone:is_gone(Id, Context)),
        ?assert(meck:validate(z_db))
    after
        meck:unload(z_db)
    end,
    ?assertEqual(ok, m_backup_revision:revert_resource(Id, Rev, [], Context)),
    % A live resource has no tombstone, but must still support revision rollback.
    {ok, Id} = m_rsc:update(Id, #{ <<"title">> => <<"Edited">> }, Context),
    ?assertEqual(ok, m_backup_revision:revert_resource(Id, Rev, [], Context)),
    ?assertEqual(<<"Original">>, m_rsc:p(Id, title, Context)).

revision(Id, Context) ->
    z_db:q1("select id from backup_revision where rsc_id = $1 order by created desc, id desc limit 1", [Id], Context).

-endif.
