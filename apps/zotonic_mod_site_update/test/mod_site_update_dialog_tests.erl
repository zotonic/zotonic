%% @doc Render the deployment preview with real wires and escaped Git metadata.
-module(mod_site_update_dialog_tests).
-include_lib("eunit/include/eunit.hrl").

dialog_test_() -> {timeout, 30, fun dialog/0}.

dialog() ->
    Context = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    ok = z_module_manager:activate_await(mod_site_update, Context),
    z_module_indexer:reindex(Context),
    await_template(Context, 100),
    {ok, _} = z_template:template_module(<<"_dialog_site_deploy.tpl">>, #{}, Context),
    Preview = #{current => #{hash => <<"abc123">>, subject => <<"<script>current</script>">>},
        upstream => <<"origin/main">>, log => [#{hash => <<"def456">>, subject => <<"</pre><script>remote</script>">>,
            author => <<"<script>author</script>">>, date => <<"2026-11-02">>,
            body => <<"<script>body</script>\nMore details">>}],
        can_deploy => true, count => 1},
    Vars = #{target => current, name => <<"Test site">>, preview => Preview},
    {Html, _} = z_template:render_to_iolist("_dialog_site_deploy.tpl", Vars, Context),
    Binary = iolist_to_binary(Html),
    ?assertEqual(nomatch, binary:match(Binary, <<"<script>">>)),
    ?assertNotEqual(nomatch, binary:match(Binary, <<"&lt;script&gt;">>)),
    ?assertNotEqual(nomatch, binary:match(Binary, <<"pre-scrollable">>)),
    ?assertNotEqual(nomatch, binary:match(Binary, <<"<code>def456</code>">>)),
    ?assertNotEqual(nomatch, binary:match(Binary, <<"<details>">>)),
    ?assertNotEqual(nomatch, binary:match(Binary, <<"&lt;script&gt;author&lt;/script&gt;">>)),
    ?assertNotEqual(nomatch, binary:match(Binary, <<"&lt;script&gt;body&lt;/script&gt;">>)),
    ?assertNotEqual(nomatch, binary:match(Binary, <<"Cancel">>)),
    ?assertNotEqual(nomatch, binary:match(Binary, <<"Deploy">>)),
    ?assertEqual(nomatch, binary:match(Binary, <<" disabled">>)),
    {Empty, _} = z_template:render_to_iolist("_dialog_site_deploy.tpl",
        Vars#{preview := Preview#{log := [], can_deploy := false, count := 0}}, Context),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Empty), <<" disabled">>)),
    {Failed, _} = z_template:render_to_iolist("_dialog_site_deploy.tpl",
        Vars#{preview := #{current => #{hash => <<"abc123">>, subject => <<"Current">>}, error => no_upstream}}, Context),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Failed), <<"no upstream">>)),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Failed), <<" disabled">>)),
    {Loading, _} = z_template:render_to_iolist("_dialog_site_deploy_loading.tpl", Vars, Context),
    LoadingBinary = iolist_to_binary(Loading),
    lists:foreach(fun(Text) ->
        ?assertNotEqual(nomatch, binary:match(LoadingBinary, Text))
    end, [<<"spinner.gif">>, <<"Fetching remote changes">>, <<"role=\"status\"">>,
          <<"Cancel">>, <<" disabled">>]),
    module_buttons(Context).

module_buttons(Context) ->
    meck:new(m_site_update, [passthrough, no_link]),
    try
        meck:expect(m_site_update, m_get, fun([<<"user_modules">> | Rest], _, _) ->
            {ok, {#{mod_example => #{repository => "example"}}, Rest}}
        end),
        {Button, _} = z_template:render_to_iolist("_admin_module_buttons.tpl",
            #{module => mod_example}, Context),
        ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Button), <<"module-deploy-mod_example">>)),
        ?assertEqual(nomatch, binary:match(iolist_to_binary(Button), <<"Deploy latest">>)),
        ?assertEqual(0, meck:num_calls(m_site_update, m_get, '_')),
        {Loaded, _} = z_template:render_to_iolist("_admin_module_deploy_button.tpl",
            #{module => mod_example}, Context),
        ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Loaded), <<"Deploy latest">>)),
        {Bundled, _} = z_template:render_to_iolist("_admin_module_buttons.tpl",
            #{module => mod_admin}, Context),
        ?assertEqual(nomatch, binary:match(iolist_to_binary(Bundled), <<"Deploy latest">>)),
        {Anon, _} = z_template:render_to_iolist("_admin_module_deploy_button.tpl",
            #{module => mod_example}, z_acl:anondo(Context)),
        ?assertEqual(nomatch, binary:match(iolist_to_binary(Anon), <<"Deploy latest">>)),
        {Dialog, _} = z_template:render_to_iolist("_dialog_site_deploy.tpl",
            #{target => {module, mod_example}, name => mod_example, is_module => true,
                preview => #{current => #{hash => <<"abc123">>, subject => <<"Current">>}, log => [#{hash => <<"def456">>, subject => <<"Next">>}], can_deploy => true}}, Context),
        ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Dialog), <<"entire Git repository">>))
    after
        meck:unload(m_site_update)
    end.

await_template(_Context, 0) -> error(template_index_timeout);
await_template(Context, Retries) ->
    case z_module_indexer:find(template, <<"_dialog_site_deploy.tpl">>, Context) of
        {ok, _} -> ok;
        {error, enoent} -> timer:sleep(50), await_template(Context, Retries - 1)
    end.
