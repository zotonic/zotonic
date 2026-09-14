%% @doc Regression tests for deleted-resource authorization, migration and recovery.
-module(m_backup_deleted_tests).
-moduledoc("Integration tests using the disposable Zotonic test sandbox.").
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

deleted_pages_test_() ->
    {timeout, 180, fun deleted_pages/0}.

deleted_pages() ->
    Anon = z_context:new(zotonic_site_testsandbox),
    Admin = z_acl:sudo(Anon),
    ok = z_module_manager:activate_await(mod_backup, Admin),
    ok = z_module_manager:upgrade_await(Admin),
    ?assertNot(z_db:table_exists(backup_gone_migration, Admin)),
    ok = mod_backup:manage_schema({upgrade, 7}, Admin),
    ok = m_backup_revision:insert_deleted_revisions(Admin),
    ?assertNot(z_db:table_exists(backup_gone_migration, Admin)),
    authorization(Anon, Admin),
    references(Admin),
    migration(Admin),
    pruning(Admin),
    uri_alias(Admin),
    templates(Admin),
    ok.

authorization(Anon, Admin) ->
    {ok, Group} = m_rsc:insert(#{ <<"category_id">> => acl_user_group }, Admin),
    {ok, User} = m_rsc:insert(#{ <<"category_id">> => person }, Admin),
    {ok, _} = m_edge:insert(User, hasusergroup, Group, Admin),
    rules(Group, [update], Admin),
    Context = z_acl:logon(User, Anon),
    {ok, Id} = m_rsc:insert(#{ <<"category_id">> => article,
        <<"creator_id">> => User, <<"title">> => <<"Private deleted title">> }, Admin),
    {ok, Other} = m_rsc:insert(#{ <<"category_id">> => article }, Admin),
    ok = m_rsc:delete(Id, Admin),
    ok = m_rsc:delete(Other, Admin),
    Rev = revision(Id, Admin),
    ?assert(m_backup_revision:can_view(Id, Context)),
    ?assertNot(m_backup_revision:can_view(Other, Context)),
    ?assertNot(m_backup_revision:can_view(Id, Anon)),
    ?assertEqual({error, eacces}, m_backup_revision:get_revision(Rev, Anon)),
    #search_result{result = Rows, total = Total} = m_backup_revision:list_deleted({1, 1000}, Context),
    ?assertEqual(1, Total),
    ?assert(lists:any(fun(#{ <<"id">> := R }) -> R =:= Id end, Rows)),
    ?assertNot(lists:any(fun(#{ <<"id">> := R }) -> R =:= Other end, Rows)),
    ?assertEqual({error, eacces}, m_backup_revision:revert_resource(Id, Rev, [], Context)),
    ?assertNot(m_rsc:exists(Id, Admin)),
    ?assert(m_rsc_gone:is_gone(Id, Admin)),
    % Author ownership must agree between the per-resource check and SQL filtering.
    m_config:set_value(mod_acl_user_groups, author_is_owner, true, Admin),
    {ok, Authored} = m_rsc:insert(#{ <<"category_id">> => article }, Admin),
    {ok, _} = m_edge:insert(Authored, author, User, Admin),
    ok = m_rsc:delete(Authored, Admin),
    ?assert(m_backup_revision:can_view(Authored, Context)),
    #search_result{result = AuthoredRows} = m_backup_revision:list_deleted({1, 1000}, Context),
    ?assert(lists:any(fun(#{ <<"id">> := R }) -> R =:= Authored end, AuthoredRows)),
    m_config:set_value(mod_acl_user_groups, author_is_owner, false, Admin),
    rules(Group, [update, insert], Admin),
    Context1 = z_acl:logon(User, Anon),
    ?assertEqual(ok, m_backup_revision:revert_resource(Id, Rev, [], Context1)),
    ?assert(m_rsc:exists(Id, Admin)),
    ?assertNot(m_rsc_gone:is_gone(Id, Admin)),
    ?assertEqual(undefined, m_rsc_gone:get(Id, Admin)),
    ?assertEqual(0, z_db:q1("select count(*) from rsc_gone where id = $1", [Id], Admin)),
    ok.

references(Admin) ->
    {ok, Creator} = m_rsc:insert(#{ <<"category_id">> => person }, Admin),
    {ok, CG} = m_rsc:insert(#{ <<"category_id">> => content_group,
        <<"title">> => <<"Old content group">> }, Admin),
    Cat = m_category:insert(m_rsc:rid(article, Admin), <<"backup_deleted_test_category">>, [], Admin),
    {ok, Id} = m_rsc:insert(#{ <<"category_id">> => Cat, <<"content_group_id">> => CG,
        <<"creator_id">> => Creator }, Admin),
    ok = m_rsc:delete(Id, Admin),
    Rev = revision(Id, Admin),
    ok = m_rsc:delete(Creator, Admin),
    ok = m_rsc:delete(CG, Admin),
    Article = m_rsc:rid(article, Admin),
    ok = m_rsc:delete(Cat, Article, Admin),
    ?assertEqual(Article, m_rsc_gone:followup(Cat, Admin)),
    DefaultCG = m_rsc:rid(default_content_group, Admin),
    z_db:q("update rsc_gone set new_id = $2 where id = $1", [CG, DefaultCG], Admin),
    ?assertEqual(DefaultCG, m_rsc_gone:followup(CG, Admin)),
    ?assertEqual({error, missing_reference}, m_backup_revision:revert_resource(Id, Rev, [], Admin)),
    ?assertNot(m_rsc:exists(Id, Admin)),
    ?assertEqual(ok, m_backup_revision:revert_resource(Id, Rev, [{category_id, Article}, {content_group_id, DefaultCG}], Admin)),
    ?assertNotEqual(Creator, m_rsc:p_no_acl(Id, creator_id, Admin)),
    % Followup resolution must terminate on cycles.
    z_db:q("update rsc_gone set new_id = $2 where id = $1", [CG, Creator], Admin),
    z_db:q("update rsc_gone set new_id = $2 where id = $1", [Creator, CG], Admin),
    ?assertEqual(undefined, m_rsc_gone:followup(CG, Admin)),
    % Rollback also requires confirmation for a missing content group.
    ?assertEqual({error, missing_reference}, m_backup_revision:revert_resource(Id, Rev, [], Admin)),
    ok.

migration(Admin) ->
    migrate(0, Admin),
    {ok, Id} = m_rsc:insert(#{ <<"category_id">> => article, <<"title">> => <<"First">> }, Admin),
    {ok, Id} = m_rsc:update(Id, #{ <<"title">> => <<"Latest">> }, Admin),
    ok = m_rsc:delete(Id, Admin),
    z_db:q("update rsc_gone set category_id = null, props_json = null where id = $1", [Id], Admin),
    {ok, Live} = m_rsc:insert(#{ <<"category_id">> => article }, Admin),
    {ok, Live} = m_rsc_gone:gone(Live, Admin),
    start_migration(Admin),
    migrate(0, Admin),
    ?assertEqual(0, z_db:q1("select count(*) from rsc_gone where id = $1", [Live], Admin)),
    ?assertEqual(m_rsc:rid(article, Admin), z_db:q1("select category_id from rsc_gone where id = $1", [Id], Admin)),
    {ok, Gone} = z_db:qmap_props_row("select * from rsc_gone where id = $1", [Id], Admin),
    ?assertEqual(<<"Latest">>, maps:get(<<"title">>, Gone)),
    ?assertNot(z_db:column_exists(rsc_gone, acl_migrated, Admin)),
    ?assertNot(z_db:table_exists(backup_gone_migration, Admin)),
    migrate(0, Admin),
    % A batch commits at most 100 rows and supplies a cursor for the next invocation.
    Inserted = z_db:q("insert into rsc_gone (id, version)
        select nextval('rsc_id_seq'), 1 from generate_series(1, 101) returning id", Admin),
    Ids = [R || {R} <- Inserted],
    start_migration(Admin),
    z_db:q("update backup_gone_migration set last_id = $1", [lists:min(Ids)-1], Admin),
    {delay, _, [Cursor]} = backup_gone_migration:migrate(0, Admin),
    ?assertEqual(Cursor, z_db:q1("select last_id from backup_gone_migration", Admin)),
    ?assertEqual(100, z_db:q1("select count(*) from rsc_gone where id = any($1) and props_json is not null", [Ids], Admin)),
    % A stale task argument must resume from the persisted table cursor.
    migrate(0, Admin),
    ?assertEqual(101, z_db:q1("select count(*) from rsc_gone where id = any($1) and props_json is not null", [Ids], Admin)),
    ?assertNot(z_db:table_exists(backup_gone_migration, Admin)),
    z_db:q("delete from rsc_gone where id = any($1)", [Ids], Admin),
    ok.

migrate(After, Context) ->
    case backup_gone_migration:migrate(After, Context) of
        ok -> ok;
        {delay, _, [Next]} -> migrate(Next, Context)
    end.

revision(Id, Context) ->
    z_db:q1("select id from backup_revision where rsc_id = $1 order by created desc, id desc limit 1", [Id], Context).

rules(Group, Actions, Context) ->
    z_mqtt:subscribe(<<"model/acl_user_groups/event/acl-rules/publish-rebuild">>, Context),
    m_acl_rule:replace_managed([
        {module, [{acl_user_group_id, Group}, {actions, [use]}, {module, mod_admin}]},
        {rsc, [{acl_user_group_id, Group}, {actions, Actions}, {is_owner, true},
               {category_id, article}, {content_group_id, default_content_group}]}
    ], ?MODULE, Context),
    receive {mqtt_msg, _} -> ok after 30000 -> error(acl_rebuild_timeout) end,
    z_mqtt:unsubscribe(<<"model/acl_user_groups/event/acl-rules/publish-rebuild">>, Context).


uri_alias(Admin) ->
    CascadeSql = "select confupdtype = 'c' and confdeltype = 'c'
                  from pg_constraint
                  where conrelid = 'rsc_uri_alias'::regclass
                    and conname = 'rsc_uri_alias_rsc_id_fkey'",
    ?assertEqual(true, z_db:q1(CascadeSql, Admin)),
    Uri = <<"https://example.test/imported-backup-test">>,
    {ok, Id} = m_rsc:insert(#{ <<"category_id">> => article,
        <<"uri">> => Uri, <<"is_authoritative">> => false }, Admin),
    ?assertEqual({ok, Id}, m_rsc:make_authoritative(Id, Admin)),
    ?assertEqual(Id, m_rsc:uri_lookup(Uri, Admin)),
    ?assertNot(m_rsc_gone:is_gone(Id, Admin)),
    ?assertEqual(0, z_db:q1("select count(*) from rsc_gone where id = $1", [Id], Admin)),
    {ok, #{ <<"data">> := Authoritative }} = m_backup_revision:get_revision(revision(Id, Admin), Admin),
    ?assertEqual([Uri], maps:get(<<"backup_uri_aliases">>, Authoritative)),
    {ok, Id} = m_rsc:update(Id, #{ <<"title">> => <<"Alias revision">> }, Admin),
    Rev = revision(Id, Admin),
    {ok, #{ <<"data">> := Saved }} = m_backup_revision:get_revision(Rev, Admin),
    ?assertEqual([Uri], maps:get(<<"backup_uri_aliases">>, Saved)),
    LateUri = <<"https://example.test/late-alias">>,
    ok = m_rsc:remember_uri(Id, LateUri, Admin),
    ok = m_rsc:delete(Id, Admin),
    DeletedRev = revision(Id, Admin),
    {ok, #{ <<"data">> := Deleted }} = m_backup_revision:get_revision(DeletedRev, Admin),
    ?assertEqual(lists:sort([Uri, LateUri]), maps:get(<<"backup_uri_aliases">>, Deleted)),
    ?assertEqual([], m_rsc:uri_aliases(Id, Admin)),
    ok = m_backup_revision:revert_resource(Id, DeletedRev, [], Admin),
    ?assertEqual(Id, m_rsc:uri_lookup(Uri, Admin)),
    ?assertEqual(Id, m_rsc:uri_lookup(LateUri, Admin)),
    ?assertEqual(undefined, m_rsc:p_no_acl(Id, <<"backup_uri_aliases">>, Admin)),
    {ok, Other} = m_rsc:insert(#{ <<"category_id">> => article }, Admin),
    ok = m_rsc:remember_uri(Other, Uri, Admin),
    ok = m_backup_revision:revert_resource(Id, Rev, [], Admin),
    ?assertEqual(Other, m_rsc:uri_lookup(Uri, Admin)),
    ok.


templates(Context) ->
    lists:foreach(fun(T) ->
        {ok, _} = z_template:template_module(T, #{}, Context)
    end, [<<"admin_backup_deleted.tpl">>, <<"admin_backup_revision.tpl">>,
          <<"_dialog_backup_revert_confirm.tpl">>]),
    _ = z_template:render_block(content, <<"admin_backup_deleted.tpl">>, [], Context),
    Id = z_db:q1("select rsc_id from backup_revision order by id desc limit 1", Context),
    {Html, _} = z_template:render_to_iolist(<<"_dialog_backup_revert_confirm.tpl">>,
            [{rsc_id, Id}, {rev_id, revision(Id, Context)}], Context),
    DefaultCG = integer_to_binary(m_rsc:rid(default_content_group, Context)),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Html), <<"value=\"", DefaultCG/binary, "\"">>)),
    ok.


start_migration(Context) ->
    ok = mod_backup:manage_schema({upgrade, 6}, Context),
    z_pivot_rsc:delete_task(backup_gone_migration, migrate, Context),
    ok.

pruning(Context) ->
    Expired = deleted_title(<<"Expired private title">>, Context),
    Retained = deleted_title(<<"Retained title">>, Context),
    Personal = deleted_title(<<"Expired personal title">>, Context),
    Unaffected = deleted_title(<<"Unchanged title">>, Context),
    z_db:q("delete from backup_revision where rsc_id = $1", [Unaffected], Context),
    z_db:q("update backup_revision set created = '2000-01-01' where rsc_id = $1", [Expired], Context),
    z_db:q("update backup_revision set created = '2000-01-01'
            where rsc_id = $1 and id <> $2", [Retained, revision(Retained, Context)], Context),
    z_db:q("update rsc_gone set is_personal_data = true, modified = '2000-01-01' where id = $1", [Personal], Context),
    ok = m_backup_revision:periodic_cleanup(Context),
    lists:foreach(fun(Id) ->
        ?assertEqual(0, z_db:q1("select count(*) from backup_revision where rsc_id = $1", [Id], Context)),
        ?assertEqual(false, z_db:q1("select props_json ? 'title' from rsc_gone where id = $1", [Id], Context)),
        ?assertEqual(m_rsc:rid(article, Context), z_db:q1("select category_id from rsc_gone where id = $1", [Id], Context))
    end, [Expired, Personal]),
    ?assertEqual(<<"Retained title">>, z_db:q1("select props_json->>'title' from rsc_gone where id = $1", [Retained], Context)),
    ?assertEqual(<<"Unchanged title">>, z_db:q1("select props_json->>'title' from rsc_gone where id = $1", [Unaffected], Context)),
    ok.

deleted_title(Title, Context) ->
    {ok, Id} = m_rsc:insert(#{ <<"category_id">> => article, <<"title">> => Title }, Context),
    ok = m_rsc:delete(Id, Context),
    Id.
