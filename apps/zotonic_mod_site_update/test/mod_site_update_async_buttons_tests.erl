%% @doc Repository discovery must not block the postback or run for non-admins.
-module(mod_site_update_async_buttons_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_wires/include/mod_wires.hrl").

async_buttons_test() ->
    Test = self(),
    Admin = #context{site = zotonic_site_testsandbox, user_id = 1},
    Anonymous = #context{site = zotonic_site_testsandbox},
    Mocks = [z_acl, z_context, z_proc, m_site_update, z_notifier],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Mocks),
    try
        meck:expect(z_acl, is_admin, fun
            (#context{user_id = 1}) -> true;
            (#context{user_id = undefined}) -> false
        end),
        meck:expect(z_context, prune_for_async, fun(C) -> C end),
        meck:expect(z_proc, spawn_md, fun(F) -> spawn(F) end),
        meck:expect(m_site_update, m_get, fun([<<"user_modules">>], undefined, #context{user_id = 1}) ->
            Test ! {discovery, self()},
            receive continue -> ok after 2000 -> error(discovery_timeout) end,
            {ok, {#{mod_example => #{repository => "example"}}, []}}
        end),
        meck:expect(z_notifier, first, fun(#page_actions{actions = Actions}, #context{user_id = 1}) ->
            Test ! {actions, Actions},
            ok
        end),
        Event = #postback{message = module_deployment_buttons},
        ?assertEqual(Anonymous, mod_site_update:event(Event, Anonymous)),
        ?assertEqual(0, meck:num_calls(z_proc, spawn_md, '_')),
        %% Discovery waits for us; a synchronous handler would never return here.
        ?assertEqual(Admin, mod_site_update:event(Event, Admin)),
        Worker = receive {discovery, Pid} -> Pid after 1000 -> error(no_discovery) end,
        Worker ! continue,
        receive
            {actions, [{update, Props}]} ->
                ?assertEqual(<<"module-deploy-mod_example">>, proplists:get_value(target, Props)),
                ?assertEqual("_admin_module_deploy_button.tpl", proplists:get_value(template, Props)),
                ?assertEqual(mod_example, proplists:get_value(module, Props))
        after 1000 -> error(no_buttons)
        end
    after
        lists:foreach(fun meck:unload/1, Mocks)
    end.
