%% @doc Deleted overview membership across page deletion, recovery and re-deletion.
-module(m_backup_redelete_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

redelete_page_test_() ->
    {timeout, 60, fun redelete_page/0}.

redelete_page() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_module_manager:activate_await(mod_backup, Context),
    {ok, Id} = m_rsc:insert(#{
        <<"category_id">> => article,
        <<"title">> => <<"Delete, recover, delete again">>
    }, Context),
    ?assert(m_rsc:exists(Id, Context)),
    ?assertEqual([], deleted_entries(Id, Context)),

    ?assertEqual(ok, m_rsc:delete(Id, Context)),
    ?assertNot(m_rsc:exists(Id, Context)),
    ?assertMatch([#{ <<"id">> := Id }], deleted_entries(Id, Context)),
    Rev = z_db:q1("select id from backup_revision where rsc_id = $1
        order by created desc, id desc limit 1", [Id], Context),

    ?assertEqual(ok, m_backup_revision:revert_resource(Id, Rev, [], Context)),
    ?assert(m_rsc:exists(Id, Context)),
    ?assertEqual(<<"Delete, recover, delete again">>, m_rsc:p(Id, title, Context)),
    ?assertEqual([], deleted_entries(Id, Context)),

    ?assertEqual(ok, m_rsc:delete(Id, Context)),
    ?assertNot(m_rsc:exists(Id, Context)),
    ?assertMatch([#{ <<"id">> := Id }], deleted_entries(Id, Context)).

deleted_entries(Id, Context) ->
    #search_result{ result = Deleted } = m_backup_revision:list_deleted({1, 100}, Context),
    [Entry || #{ <<"id">> := GoneId } = Entry <- Deleted, GoneId =:= Id].
