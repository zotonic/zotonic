%% @doc Regression coverage for repeatedly deleting and restoring media.
-module(m_backup_media_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

redelete_media_test_() ->
    {timeout, 60, fun redelete_media/0}.

redelete_media() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_module_manager:activate_await(mod_backup, Context),
    File = filename:join(code:priv_dir(zotonic_site_testsandbox), "files/archive/koe.jpg"),
    {ok, Id} = m_media:insert_file(File, Context),
    redelete_media(Id, Context),
    % Older recoveries left this column NULL; raw reads supply its default.
    {ok, 1} = z_db:update(rsc, Id, #{ <<"content_group_id">> => undefined }, Context),
    z_depcache:flush(Id, Context),
    redelete_media(Id, Context).

redelete_media(Id, Context) ->
    lists:foreach(
        fun(_) ->
            ?assert(is_map(m_media:get(Id, Context))),
            ?assertEqual(ok, m_rsc:delete(Id, Context)),
            ?assertNot(m_rsc:exists(Id, Context)),
            ?assert(m_rsc_gone:is_gone(Id, Context)),
            #search_result{ result = Deleted } = m_backup_revision:list_deleted({1, 100}, Context),
            ?assert(lists:any(fun(#{ <<"id">> := GoneId }) -> GoneId =:= Id end, Deleted)),
            Rev = z_db:q1("select id from backup_revision where rsc_id = $1
                order by created desc, id desc limit 1", [Id], Context),
            ?assertEqual(ok, m_backup_revision:revert_resource(Id, Rev, [], Context)),
            ?assert(m_rsc:exists(Id, Context)),
            ?assertEqual(m_rsc:p(Id, content_group_id, Context),
                z_db:q1("select content_group_id from rsc where id = $1", [Id], Context)),
            ?assertNot(m_rsc_gone:is_gone(Id, Context))
        end,
        lists:seq(1, 3)).
