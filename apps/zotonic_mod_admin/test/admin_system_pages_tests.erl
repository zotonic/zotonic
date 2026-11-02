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
    ?assertEqual(nomatch, binary:match(Maintenance, <<"Close excess connections">>)),
    Information = render("admin_system_info.tpl", Context),
    ?assertNotEqual(nomatch, binary:match(Information, <<"Erlang init files">>)),
    ?assertNotEqual(nomatch, binary:match(render("admin_imports.tpl", Context), <<"dropupload">>)),
    ?assertEqual({error, eacces}, m_admin_status:m_get([<<"code_update">>], undefined, Anonymous)),
    ?assertEqual(nomatch, binary:match(render("admin_system_info.tpl", Anonymous), <<"Erlang init files">>)),
    ?assertEqual(nomatch, binary:match(render("_admin_status_task_queue.tpl", Anonymous), <<"<table">>)),
    ok.

render(Template, Context) ->
    {Html, _} = case Template of
        [$_ | _] -> z_template:render_to_iolist(Template, [], Context);
        _ -> z_template:render_block_to_iolist(content, Template, [], Context)
    end,
    iolist_to_binary(Html).
