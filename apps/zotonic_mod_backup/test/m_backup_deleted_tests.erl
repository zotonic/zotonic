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
    legacy_author_ownership(Admin),
    person_labels(Admin),
    deletion_without_view_access(Admin),
    lock_order(restore, Admin),
    lock_order(migrate, Admin),
    ok.

deletion_without_view_access(Admin) ->
    {ok, Group} = m_rsc:insert(#{ <<"category_id">> => acl_user_group }, Admin),
    {ok, User} = m_rsc:insert(#{ <<"category_id">> => person }, Admin),
    {ok, _} = m_edge:insert(User, hasusergroup, Group, Admin),
    rules(Group, [delete], Admin),
    Context = z_acl:logon(User, z_context:new(Admin)),
    {ok, Id} = m_rsc:insert(#{ <<"category_id">> => article,
        <<"creator_id">> => User, <<"is_published">> => false }, Admin),
    ?assertEqual(undefined, m_rsc:get(Id, Context)),
    ?assert(z_acl:is_allowed(delete, Id, Context)),
    % The deletion revision must capture current raw data, not an earlier revision.
    {ok, 1} = z_db:update(rsc, Id, #{ <<"body">> => <<"Hidden final body">> }, Admin),
    ok = m_rsc:delete(Id, Context),
    {ok, #{ <<"type">> := $D, <<"data">> := Props }} =
        m_backup_revision:get_revision(revision(Id, Admin), Admin),
    ?assertEqual(<<"Hidden final body">>, maps:get(<<"body">>, Props)).

%% Hold deletion's first lock while the competing operation starts. Its wait
%% must not hold the tombstone, which deletion still needs to lock.
lock_order(Operation, Admin) ->
    {ok, Id} = m_rsc:insert(#{ <<"category_id">> => article,
        <<"uri">> => <<"https://example.test/lock-order/", (atom_to_binary(Operation, utf8))/binary>> }, Admin),
    {ok, Id} = m_rsc_gone:gone(Id, Admin),
    Rev = revision(Id, Admin),
    case Operation of
        migrate ->
            start_migration(Admin),
            z_db:q("update backup_gone_migration set last_id = $1, max_id = $2", [Id-1, Id], Admin);
        restore -> ok
    end,
    Parent = self(),
    Ref = make_ref(),
    {Worker, Monitor} = spawn_monitor(fun() ->
        receive {run, Ref} -> ok end,
        WorkerContext = z_acl:sudo(z_context:new(Admin)),
        Result = case Operation of
            restore -> m_backup_revision:revert_resource(Id, Rev, [], WorkerContext);
            migrate -> backup_gone_migration:migrate(0, WorkerContext)
        end,
        Parent ! {Ref, Result}
    end),
    try
        ?assertEqual(ok, z_db:transaction(fun(Ctx) ->
            Id = z_db:q1("select id from rsc where id = $1 for update", [Id], Ctx),
            Backend = z_db:q1("select pg_backend_pid()", Ctx),
            Worker ! {run, Ref},
            wait_for_blocked_transaction(Backend, Ctx, 200),
            Id = z_db:q1("select id from rsc_gone where id = $1 for update nowait", [Id], Ctx),
            m_rsc:delete(Id, Ctx)
        end, Admin)),
        receive
            {Ref, Result} ->
                case Operation of
                    restore -> ?assertEqual({error, enoent}, Result);
                    migrate -> ?assertMatch({delay, _, [Id]}, Result)
                end;
            {'DOWN', Monitor, process, Worker, Reason} -> error({worker_failed, Reason})
        after 10000 -> error(worker_timeout)
        end,
        ?assertNot(m_rsc:exists(Id, Admin)),
        case Operation of
            migrate -> migrate(0, Admin);
            restore -> ok
        end
    after
        exit(Worker, kill),
        erlang:demonitor(Monitor, [flush]),
        receive {Ref, _} -> ok after 0 -> ok end
    end.

wait_for_blocked_transaction(_Backend, _Context, 0) ->
    error(transaction_not_blocked);
wait_for_blocked_transaction(Backend, Context, Attempts) ->
    case z_db:q1("select exists(select 1 from pg_stat_activity where $1 = any(pg_blocking_pids(pid)))",
        [Backend], Context)
    of
        true -> ok;
        false ->
            timer:sleep(10),
            wait_for_blocked_transaction(Backend, Context, Attempts-1)
    end.

legacy_author_ownership(Admin) ->
    {ok, Group} = m_rsc:insert(#{ <<"category_id">> => acl_user_group }, Admin),
    {ok, User} = m_rsc:insert(#{ <<"category_id">> => person }, Admin),
    {ok, _} = m_edge:insert(User, hasusergroup, Group, Admin),
    rules(Group, [update], Admin),
    Context = z_acl:logon(User, z_context:new(Admin)),
    m_config:set_value(mod_acl_user_groups, author_is_owner, true, Admin),
    try
        {ok, Id} = m_rsc:insert(#{ <<"category_id">> => article }, Admin),
        {ok, _} = m_edge:insert(Id, author, User, Admin),
        ok = m_edge:delete(Id, author, User, Admin),
        ok = m_rsc:delete(Id, Admin),
        ?assertNot(m_backup_revision:can_view(Id, Context)),
        % Put the revoked edge inside the old migration's inference window.
        z_db:q("update backup_edge_log set timestamp = (
                    select modified - interval '1 second' from rsc_gone where id = $1)
                where subject_id = $1 and predicate = 'author' and not is_insert", [Id], Admin),
        {ok, Owned} = m_rsc:insert(#{ <<"category_id">> => article,
            <<"creator_id">> => User }, Admin),
        ok = m_rsc:delete(Owned, Admin),
        z_db:q("update rsc_gone set category_id = null, props_json = null where id = any($1)",
            [[Id, Owned]], Admin),
        start_migration(Admin),
        migrate(0, Admin),
        ?assertNot(m_backup_revision:can_view(Id, Context)),
        ?assertEqual({error, eacces}, m_backup_revision:get_revision(revision(Id, Admin), Context)),
        ?assert(m_backup_revision:can_view(Owned, Context)),
        {ok, Gone} = z_db:qmap_props_row("select * from rsc_gone where id = $1", [Owned], Admin),
        assert_no_person_labels(Gone)
    after
        m_config:set_value(mod_acl_user_groups, author_is_owner, false, Admin)
    end.

person_labels(Admin) ->
    {ok, Group} = m_rsc:insert(#{ <<"category_id">> => acl_user_group }, Admin),
    {ok, User} = m_rsc:insert(#{ <<"category_id">> => person }, Admin),
    {ok, _} = m_edge:insert(User, hasusergroup, Group, Admin),
    rules(Group, [update], [
        {rsc, [{acl_user_group_id, Group}, {actions, [view]},
            {category_id, person}, {content_group_id, default_content_group}]}
    ], Admin),
    Context = z_acl:logon(User, z_context:new(Admin)),
    {ok, Creator} = m_rsc:insert(#{ <<"category_id">> => person,
        <<"title">> => <<"Old creator label">> }, Admin),
    {ok, Deleter} = m_rsc:insert(#{ <<"category_id">> => person,
        <<"title">> => <<"Old deletion actor label">> }, Admin),
    {ok, Followup} = m_rsc:insert(#{ <<"category_id">> => person,
        <<"title">> => <<"Current followup label">>, <<"is_published">> => true }, Admin),
    {ok, Id} = m_rsc:insert(#{ <<"category_id">> => article,
        <<"creator_id">> => Creator }, Admin),
    {ok, _} = m_edge:insert(Id, author, User, Admin),
    m_config:set_value(mod_acl_user_groups, author_is_owner, true, Admin),
    try
        ok = m_rsc:delete(Id, Admin),
        {ok, Gone} = z_db:qmap_props_row("select * from rsc_gone where id = $1", [Id], Admin),
        assert_no_person_labels(Gone),
        ?assert(m_backup_revision:can_view(Id, Context)),
        ok = m_rsc:delete(Creator, Followup, Admin),
        ok = m_rsc:delete(Deleter, Followup, Admin),
        % Existing copied labels remain stored, but must never be used for display.
        OldLabels = #{
            <<"deleter_id">> => Deleter,
            <<"deleted_by">> => #{ <<"title">> => <<"Old deletion actor label">> },
            <<"references">> => (maps:get(<<"references">>, Gone))#{
                <<"creator_id">> => #{ <<"title">> => <<"Old creator label">> }
            }
        },
        z_db:q("update rsc_gone set props_json = props_json || $2::jsonb where id = $1",
            [Id, ?DB_PROPS_JSON(OldLabels)], Admin),
        ?assertEqual({ok, {Followup, []}}, m_rsc_gone:m_get([Creator, <<"followup">>], undefined, Context)),
        ?assertEqual(<<"Current followup label">>, m_rsc:p(Followup, title, Context)),
        {Html0, _} = z_template:render_block_to_iolist(content, <<"admin_backup_deleted.tpl">>, [], Context),
        Html = iolist_to_binary(Html0),
        ?assertEqual(2, length(binary:matches(Html, <<"Current followup label">>))),
        {ok, Followup} = m_rsc:update(Followup, #{ <<"is_published">> => false }, Admin),
        ?assertEqual(undefined, m_rsc:p(Followup, title, Context)),
        {HiddenHtml0, _} = z_template:render_block_to_iolist(content, <<"admin_backup_deleted.tpl">>, [], Context),
        HiddenHtml = iolist_to_binary(HiddenHtml0),
        ?assertEqual(nomatch, binary:match(HiddenHtml, <<"Current followup label">>)),
        ?assertEqual(nomatch, binary:match(HiddenHtml, <<"Old creator label">>)),
        ?assertEqual(nomatch, binary:match(HiddenHtml, <<"Old deletion actor label">>)),
        ?assertEqual(<<"Old deletion actor label">>, z_db:q1(
            "select props_json->'deleted_by'->>'title' from rsc_gone where id = $1", [Id], Admin))
    after
        m_config:set_value(mod_acl_user_groups, author_is_owner, false, Admin)
    end.

assert_no_person_labels(Gone) ->
    ?assertNot(maps:is_key(<<"deleted_by">>, Gone)),
    ?assertEqual(1, maps:get(<<"deleter_id">>, Gone)),
    References = maps:get(<<"references">>, Gone),
    ?assertNot(maps:is_key(<<"creator_id">>, References)),
    ?assertNot(maps:is_key(<<"modifier_id">>, References)).

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
    % First enabling mod_backup on an existing site must also initialize migration.
    ok = z_db:transaction(fun(Ctx) ->
        Version = z_db:q1("select schema_version from module where name = 'mod_backup'", Ctx),
        z_db:q("update module set schema_version = null where name = 'mod_backup'", Ctx),
        ok = mod_backup:manage_schema(install, Ctx),
        z_db:q("update module set schema_version = $1 where name = 'mod_backup'", [Version], Ctx),
        ok
    end, Admin),
    ?assert(z_db:table_exists(backup_gone_migration, Admin)),
    migrate(0, Admin),
    ?assertEqual(0, z_db:q1("select count(*) from rsc_gone where id = $1", [Live], Admin)),
    ?assertEqual(m_rsc:rid(article, Admin), z_db:q1("select category_id from rsc_gone where id = $1", [Id], Admin)),
    {ok, Gone} = z_db:qmap_props_row("select * from rsc_gone where id = $1", [Id], Admin),
    ?assertEqual(<<"Latest">>, maps:get(<<"title">>, Gone)),
    ?assertNot(z_db:column_exists(rsc_gone, acl_migrated, Admin)),
    ?assertNot(z_db:table_exists(backup_gone_migration, Admin)),
    % Reinstalling the datamodel must not recreate a completed migration.
    ok = z_module_manager:reinstall(mod_backup, Admin),
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
    rules(Group, Actions, [], Context).

rules(Group, Actions, ExtraRules, Context) ->
    m_acl_rule:replace_managed([
        {module, [{acl_user_group_id, Group}, {actions, [use]}, {module, mod_admin}]},
        {rsc, [{acl_user_group_id, Group}, {actions, Actions}, {is_owner, true},
               {category_id, article}, {content_group_id, default_content_group}]}
    ] ++ ExtraRules, ?MODULE, Context),
    await_acl_rebuild(Context, 3000).

await_acl_rebuild(_Context, 0) ->
    error(acl_rebuild_timeout);
await_acl_rebuild(Context, Attempts) ->
    % The status call follows this process's rebuild casts. A completion event
    % alone could belong to an older rebuild while our rules are still queued.
    {ok, Status} = mod_acl_user_groups:status(Context),
    case lists:any(fun(Key) -> proplists:get_bool(Key, Status) end,
        [is_rebuilding, is_rebuild_publish, is_rebuild_edit])
    of
        false -> ok;
        true ->
            timer:sleep(10),
            await_acl_rebuild(Context, Attempts-1)
    end.


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
          <<"_admin_backup_diff.tpl">>, <<"_dialog_backup_revert_confirm.tpl">>]),
    lists:foreach(fun({RscId, Text, Class}) ->
        {ButtonHtml, _} = z_template:render_to_iolist(<<"_admin_backup_diff.tpl">>,
            [{a, #{id => 1, rsc_id => RscId}}], Context),
        Button = iolist_to_binary(ButtonHtml),
        ?assertNotEqual(nomatch, binary:match(Button, Text)),
        ?assertNotEqual(nomatch, binary:match(Button, Class))
    end, [{1, <<"Revert to this version...">>, <<"btn-danger">>},
          {undefined, <<"Restore this page...">>, <<"btn-primary">>}]),
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
