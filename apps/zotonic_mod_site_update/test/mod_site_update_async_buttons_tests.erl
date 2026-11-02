%% @doc Repository discovery must not block the postback or run for non-admins.
-module(mod_site_update_async_buttons_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_wires/include/mod_wires.hrl").

async_buttons_test() ->
    Test = self(),
    Admin = #context{site = site_update_async_buttons_test, user_id = 1},
    Anonymous = #context{site = site_update_async_buttons_test},
    Mocks = [m_site_update, z_notifier],
    try
        %% EUnit shares the VM with running sites. Keep their calls untouched.
        lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Mocks),
        meck:expect(m_site_update, m_get, fun
            ([<<"user_modules">>], undefined, #context{site = site_update_async_buttons_test}) ->
                Test ! {discovery, self()},
                receive continue -> ok after 2000 -> error(discovery_timeout) end,
                {ok, {#{mod_example => #{repository => "example"}}, []}};
            (Path, Msg, Context) ->
                meck:passthrough([Path, Msg, Context])
        end),
        meck:expect(z_notifier, first, fun
            (#page_actions{actions = Actions}, #context{site = site_update_async_buttons_test}) ->
                Test ! {actions, Actions},
                ok;
            (Message, Context) ->
                meck:passthrough([Message, Context])
        end),
        %% Exercise the logging process's ACL path while the mocks are installed.
        ?assert(z_acl:is_admin(z_acl:sudo(#context{site = zotonic_site_testsandbox}))),
        Event = #postback{message = module_deployment_buttons},
        ?assertEqual(Anonymous, mod_site_update:event(Event, Anonymous)),
        %% Discovery waits for us; a synchronous handler would never return here.
        ?assertEqual(Admin, mod_site_update:event(Event, Admin)),
        Worker = receive {discovery, Pid} -> Pid after 1000 -> error(no_discovery) end,
        Ref = monitor(process, Worker),
        try
            Worker ! continue,
            receive
                {actions, [{update, Props}]} ->
                    ?assertEqual(<<"module-deploy-mod_example">>, proplists:get_value(target, Props)),
                    ?assertEqual("_admin_module_deploy_button.tpl", proplists:get_value(template, Props)),
                    ?assertEqual(mod_example, proplists:get_value(module, Props))
            after 1000 -> error(no_buttons)
            end,
            receive
                {'DOWN', Ref, process, Worker, Reason} -> ?assertEqual(normal, Reason)
            after 1000 -> error(worker_not_finished)
            end,
            ?assertNot(meck:called(m_site_update, m_get, ['_', '_', Anonymous]))
        after
            %% Never unload a mock while its worker is still executing.
            CleanupRef = monitor(process, Worker),
            exit(Worker, kill),
            receive {'DOWN', CleanupRef, process, Worker, _} -> ok end,
            demonitor(Ref, [flush])
        end
    after
        lists:foreach(fun meck:unload/1, Mocks)
    end.
