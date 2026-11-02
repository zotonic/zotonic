-module(z_module_manager_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

upgrade_await_invalidates_active_modules_test() ->
    Site = module_manager_cache_test,
    CacheName = z_utils:name_for_site(z_depcache, Site),
    ManagerName = z_utils:name_for_site(z_module_manager, Site),
    Context = #context{site = Site, depcache = CacheName},
    ActiveKey = {z_module_manager, active, Site},
    ModuleKey = {z_module_manager, {active, mod_base}, Site},
    {ok, Cache} = depcache:start_link(CacheName, []),
    Parent = self(),
    Manager = spawn_link(fun() ->
        receive
            {'$gen_call', From, upgrade} ->
                %% Observe the cache when the manager receives the upgrade,
                %% not merely after the caller has returned.
                Parent ! {cache_at_upgrade, z_depcache:get(ActiveKey, Context),
                    z_depcache:get(ModuleKey, Context)},
                gen_server:reply(From, ok)
        end
    end),
    try
        true = register(ManagerName, Manager),
        %% Model a reader repopulating the old list before activation commits.
        ok = z_depcache:set(ActiveKey, [], Context),
        ok = z_depcache:set(ModuleKey, false, 3600, [ActiveKey], Context),
        ?assertEqual(ok, z_module_manager:upgrade_await(Context)),
        receive
            {cache_at_upgrade, Active, Module} ->
                ?assertEqual(undefined, Active),
                ?assertEqual(undefined, Module)
        after 1000 ->
            error(no_upgrade)
        end
    after
        unlink(Manager),
        exit(Manager, kill),
        gen_server:stop(Cache)
    end.
