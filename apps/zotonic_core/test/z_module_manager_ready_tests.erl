-module(z_module_manager_ready_tests).
-include_lib("eunit/include/eunit.hrl").

%% Readiness must remain readable while the manager cannot handle calls, as
%% observers and schema jobs can need it while the manager waits for them.
cached_readiness_test() ->
    Site = zotonic_site_testsandbox,
    ok = z_sites_manager:await_startup(Site),
    Context = z_context:new(Site),
    ?assertEqual([], z_module_manager:active_not_running(Context)),
    ?assert(z_module_manager:all_running(Context)),
    Manager = z_utils:name_for_site(z_module_manager, Site),
    ok = sys:suspend(Manager),
    Parent = self(),
    Ref = make_ref(),
    Reader = spawn(fun() -> Parent ! {Ref, z_module_manager:all_running(Context)} end),
    try
        receive {Ref, Ready} -> ?assertEqual(true, Ready)
        after 1000 -> error(readiness_requires_manager_call)
        end
    after
        exit(Reader, kill),
        sys:resume(Manager)
    end.

%% The readiness table belongs to the manager and cannot outlive it.
manager_exit_test() ->
    Site = module_readiness_fixture,
    Context = z_context:new(Site),
    ?assertNot(z_module_manager:all_running(Context)),
    Parent = self(),
    Ref = make_ref(),
    {Pid, Monitor} = spawn_monitor(fun() ->
        {ok, _} = z_module_manager:init(Site),
        Parent ! {Ref, initialized},
        receive stop -> ok end
    end),
    try
        receive {Ref, initialized} -> ok
        after 1000 -> error(manager_init_timeout)
        end,
        ?assertNot(z_module_manager:all_running(Context)),
        Pid ! stop,
        receive {'DOWN', Monitor, process, Pid, normal} -> ok
        after 1000 -> error(manager_exit_timeout)
        end,
        ?assertNot(z_module_manager:all_running(Context))
    after
        exit(Pid, kill),
        persistent_term:erase({z_rsc_defaults, Site})
    end.
