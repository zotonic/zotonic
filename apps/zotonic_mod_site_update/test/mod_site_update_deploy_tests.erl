%% @doc Admin/status previews must not deploy; confirmation rechecks permission.
-module(mod_site_update_deploy_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_wires/include/mod_wires.hrl").

button_deployment_test() ->
    Modules = [m_site_update, zotonic_filehandler_compile, z_render, z_notifier, z_acl],
    try
        lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
        meck:expect(m_site_update, vcs_site, fun(test_site) -> {git, "/site"} end),
        meck:expect(m_site_update, vcs_module, fun(mod_example) -> {git, "/module-repository"} end),
        meck:expect(m_site_update, vcs_zotonic, fun() -> {git, "/zotonic"} end),
        meck:expect(m_site_update, preview_vcs, fun({git, _}) ->
            {ok, #{current => #{subject => <<"Current">>}, log => [#{subject => <<"Incoming">>}], can_deploy => true}}
        end),
        meck:expect(m_site_update, local_changes, fun({git, _}) -> {ok, <<" M page.tpl\n">>} end),
        meck:expect(m_site_update, reset_vcs, fun({git, _}) -> ok end),
        meck:expect(z_acl, is_admin, fun(_) -> true end),
        meck:expect(z_acl, is_allowed, fun(use, mod_site_update, _) -> true end),
        meck:expect(z_notifier, notify, fun(_, _) -> ok end),
        meck:expect(z_render, growl, fun(Text, C) -> capture({growl, Text}, C) end),
        meck:expect(z_render, growl_error, fun(Text, C) -> capture({error, Text}, C) end),
        meck:expect(z_render, wire, fun(Actions, C) -> capture({wire, Actions}, C) end),
        meck:expect(z_render, dialog, fun(_, Template, Vars, C) -> capture({dialog, Template, Vars}, C) end),
        meck:expect(z_render, update, fun(Id, Render, C) -> capture({update, Id, Render}, C) end),
        Cases = [
            {vcs_up, current, test_site, "/site"},
            {{vcs_up, [{module, mod_example}]}, {module, mod_example}, test_site, "/module-repository"},
            {{vcs_up, [{module, mod_example}]}, {module, mod_example}, zotonic_site_status, "/module-repository"},
            {{vcs_up, [{site, test_site}]}, {site, test_site}, zotonic_site_status, "/site"},
            {{vcs_up, [{zotonic, true}]}, zotonic, zotonic_site_status, "/zotonic"}
        ],
        lists:foreach(fun({Message, Target, Site, Path}) ->
            Context = #context{site = Site, language = [en]},
            meck:reset(zotonic_filehandler_compile),
            meck:reset(m_site_update),
            {dialog, "_dialog_site_deploy_loading.tpl", Vars} =
                render_event(#postback{message = Message}, Context),
            ?assertEqual(Target, proplists:get_value(target, Vars)),
            {dialog, "_dialog_site_reset.tpl", ResetVars} = render_event(
                #postback{message = {vcs_reset, [{target, Target}]}}, Context),
            ?assertEqual(#{changes => <<" M page.tpl\n">>}, proplists:get_value(preview, ResetVars)),
            ?assertNot(meck:called(m_site_update, reset_vcs, ['_'])),
            ?assertMatch({growl, _}, render_event(
                #postback{message = {vcs_reset_confirm, [{target, Target}]}}, Context)),
            ?assert(meck:called(m_site_update, reset_vcs, [{git, Path}])),
            ?assertNot(meck:called(m_site_update, preview_vcs, ['_'])),
            meck:reset(z_render),
            {update, "unique-preview", #render{template = "_dialog_site_deploy.tpl", vars = PreviewVars}} =
                render_event(#postback{message = {vcs_preview,
                    [{target, Target}, {element_id, "unique-preview"}]}}, Context),
            ?assertEqual(#{subject => <<"Current">>}, maps:get(current, proplists:get_value(preview, PreviewVars))),
            ?assertNot(meck:called(z_render, dialog, ['_', '_', '_', '_'])),
            ?assertNot(meck:called(zotonic_filehandler_compile, deploy, ['_', '_'])),
            lists:foreach(fun(Outcome) ->
                meck:reset(zotonic_filehandler_compile),
                meck:expect(zotonic_filehandler_compile, deploy, fun(git, ActualPath) ->
                    ?assertEqual(Path, ActualPath), Outcome
                end),
                Rendered = render_event(
                    #postback{message = {vcs_deploy, [{target, Target}]}}, Context),
                ?assert(meck:called(zotonic_filehandler_compile, deploy, [git, Path])),
                assert_notice(Target, Outcome, Rendered)
            end, [{ok, [<<"Deployed">>]}, {error, compile_failed}])
        end, Cases),
        %% Permissions may be revoked while the preview dialog remains open.
        meck:expect(z_acl, is_admin, fun(_) -> false end),
        meck:expect(z_acl, is_allowed, fun(use, mod_site_update, _) -> false end),
        meck:reset(zotonic_filehandler_compile),
        meck:reset(m_site_update),
        lists:foreach(fun({Message, Target, Site, _}) ->
            Context = #context{site = Site, language = [en]},
            ?assertMatch({error, _}, render_event(#postback{message = Message}, Context)),
            ?assertMatch({error, _}, render_event(
                #postback{message = {vcs_reset, [{target, Target}]}}, Context)),
            ?assertMatch({error, _}, render_event(
                #postback{message = {vcs_reset_confirm, [{target, Target}]}}, Context)),
            {update, "unique-preview", #render{vars = DeniedVars}} = render_event(
                #postback{message = {vcs_preview, [{target, Target}, {element_id, "unique-preview"}]}}, Context),
            ?assertEqual(#{error => eacces}, proplists:get_value(preview, DeniedVars)),
            ?assertMatch({error, _}, render_event(
                #postback{message = {vcs_deploy, [{target, Target}]}}, Context))
        end, Cases),
        ?assertNot(meck:called(m_site_update, preview_vcs, ['_'])),
        ?assertNot(meck:called(m_site_update, reset_vcs, ['_'])),
        ?assertNot(meck:called(m_site_update, local_changes, ['_'])),
        ?assertNot(meck:called(zotonic_filehandler_compile, deploy, ['_', '_']))
    after
        lists:foreach(fun(M) -> meck:unload(M) end, Modules)
    end.

assert_notice(_, {ok, _}, {growl, Text}) ->
    ?assertEqual(<<"Deployment completed.">>, iolist_to_binary(Text));
assert_notice(_, {error, _}, {error, Text}) ->
    ?assertEqual(<<"Deployment failed. Check the server log for details.">>, iolist_to_binary(Text));
assert_notice(_, {ok, _}, {wire, Actions}) ->
    {insert_top, Props} = lists:keyfind(insert_top, 1, Actions),
    ?assertEqual(<<"Deployed">>, iolist_to_binary(proplists:get_value(notice, Props))).

%% Keep the render mocks' context return contract while recording their output.
capture(Rendered, Context) ->
    z_context:set(test_render, Rendered, Context).

render_event(Event, Context) ->
    z_context:get(test_render, mod_site_update:event(Event, Context)).
