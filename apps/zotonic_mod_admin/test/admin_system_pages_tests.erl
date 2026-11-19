%% @doc Read-only rendering checks for the system administration pages.
-module(admin_system_pages_tests).
-include_lib("eunit/include/eunit.hrl").
-export([check/1]).

system_pages_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    check("zotonic_site_testsandbox").

%% Can also be run on a development site; no maintenance actions are executed.
check(Site) ->
    Anonymous = z_context:set_csp_nonce(z_context:set_language(en, z_context:new(list_to_existing_atom(Site)))),
    Context = z_acl:sudo(Anonymous),
    Status = render("admin_status.tpl", Context),
    ?assertNotEqual(nomatch, binary:match(Status, <<"Search indexing">>)),
    ?assertEqual(nomatch, binary:match(Status, <<"Reinstall site datamodel">>)),
    ?assertNotEqual(nomatch, binary:match(Status, <<"Close excess connections">>)),
    Maintenance = render("admin_maintenance.tpl", Context),
    ?assertNotEqual(nomatch, binary:match(Maintenance, <<"Rebuild search indices">>)),
    ?assertNotEqual(nomatch, binary:match(Maintenance, <<"Advanced repair">>)),
    ?assertNotEqual(nomatch, binary:match(Maintenance, <<"Delete generated previews">>)),
    ?assertNotEqual(nomatch, binary:match(Maintenance, <<"Deduplicate media files">>)),
    ?assertEqual(nomatch, binary:match(render("admin_maintenance.tpl", Anonymous), <<"Deduplicate media files">>)),
    ?assertEqual(nomatch, binary:match(render("admin_maintenance.tpl", Anonymous), <<"Delete generated previews">>)),
    ?assertEqual(nomatch, binary:match(Maintenance, <<"Close excess connections">>)),
    Information = render("admin_system_info.tpl", Context),
    ?assertNotEqual(nomatch, binary:match(Information, <<"Zotonic init files">>)),
    ?assertNotEqual(nomatch, binary:match(render("admin_imports.tpl", Context), <<"dropupload">>)),
    ?assertEqual({error, eacces}, m_admin_status:m_get([<<"code_update">>], undefined, Anonymous)),
    ?assertEqual({error, eacces}, m_admin_status:m_get([<<"media_maintenance">>], undefined, Anonymous)),
    ?assertNotEqual(nomatch, binary:match(Maintenance, <<"admin-status-media-maintenance">>)),
    ?assertNotEqual(nomatch, binary:match(Status, <<"admin-status-media-maintenance">>)),
    ?assertEqual(nomatch, binary:match(render("_admin_status_media_maintenance.tpl", Anonymous), <<"Media deduplication">>)),
    ?assertEqual(nomatch, binary:match(render("admin_system_info.tpl", Anonymous), <<"Zotonic init files">>)),
    ?assertEqual(nomatch, binary:match(render("_admin_status_task_queue.tpl", Anonymous), <<"<table">>)),
    ok.

render(Template, Context) ->
    {Html, _} = case Template of
        [$_ | _] -> z_template:render_to_iolist(Template, [], Context);
        _ -> z_template:render_block_to_iolist(content, Template, [], Context)
    end,
    iolist_to_binary(Html).

maintenance_status_test() ->
    C = z_acl:sudo(z_context:set_language(en, z_context:new(zotonic_site_testsandbox))),
    meck:new(z_pivot_rsc, [passthrough, no_link]),
    try
        Result = z_db:transaction(fun(Ctx) ->
            z_db:q("delete from pivot_task_queue where module in
                ('z_media_cafs_migrate','z_media_preview_purge')", Ctx),
            {ok, {Idle, []}} = m_admin_status:m_get([<<"media_maintenance">>], undefined, Ctx),
            ?assertEqual(#{pending => 0, running => false}, maps:get(migration, Idle)),
            {ok, Id} = z_db:insert(pivot_task_queue, #{module => z_media_cafs_migrate,
                function => cleanup, key => <<"status-test">>}, Ctx),
            {ok, _} = z_db:insert(pivot_task_queue, #{module => z_media_preview_purge,
                function => run, key => <<"status-test">>, due => {{2099,1,1},{0,0,0}}}, Ctx),
            meck:expect(z_pivot_rsc, status, fun(_) -> {ok, #{task_id => Id}} end),
            {ok, {Active, []}} = m_admin_status:m_get([<<"media_maintenance">>], undefined, Ctx),
            ?assertEqual(#{pending => 1, running => true}, maps:get(migration, Active)),
            ?assertEqual(#{pending => 1, running => false}, maps:get(previews, Active)),
            Html = render("_admin_status_media_maintenance.tpl", Ctx),
            ?assertNotEqual(nomatch, binary:match(Html, <<"Running">>)),
            ?assertNotEqual(nomatch, binary:match(Html, <<"Queued / waiting">>)),
            z_db:delete(pivot_task_queue, Id, Ctx),
            {ok, {Done, []}} = m_admin_status:m_get([<<"media_maintenance">>], undefined, Ctx),
            ?assertEqual(#{pending => 0, running => false}, maps:get(migration, Done)),
            throw({rollback, checked})
        end, C),
        ?assertEqual({rollback, checked}, Result)
    after
        meck:unload(z_pivot_rsc)
    end.

system_info_without_development_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:set_language(en, z_context:new(zotonic_site_testsandbox))),
    meck:new(m_modules, [passthrough, no_link]),
    meck:new(m_development, [no_link]),
    try
        meck:expect(m_modules, m_get, fun
            ([<<"active">>, <<"mod_development">> | Rest], _, _) -> {ok, {false, Rest}};
            (Path, Msg, C) -> meck:passthrough([Path, Msg, C])
        end),
        Html = render("admin_system_info.tpl", Context),
        ?assertNotEqual(nomatch, binary:match(Html, <<"Site files directory">>)),
        ?assertNotEqual(nomatch, binary:match(Html, <<"Zotonic init files">>)),
        ?assertNotEqual(nomatch, binary:match(Html, <<"Site config files">>)),
        lists:foreach(fun(File) ->
            ?assertNotEqual(nomatch, binary:match(Html, unicode:characters_to_binary(File)))
        end, z_sites_config:config_files(zotonic_site_testsandbox)),
        ?assertEqual(nomatch, binary:match(Html, <<"glyphicon-folder-open">>)),
        {Config, _} = z_template:render_to_iolist("_admin_system_info_config.tpl",
            [{file, <<"/example/erlang.config">>}], Context),
        ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Config), <<"/example/erlang.config">>)),
        ?assertEqual(nomatch, binary:match(iolist_to_binary(Config), <<"data-editor-file">>)),
        ?assertEqual(0, meck:num_calls(m_development, m_get, '_'))
    after
        meck:unload([m_modules, m_development])
    end.
