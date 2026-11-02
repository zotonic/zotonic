-module(z_stats_tests).

-include_lib("eunit/include/eunit.hrl").

init_site_before_depcache_test() ->
    Site = z_stats_startup_test,
    Cache = z_utils:name_for_site(z_depcache, Site),
    {ok, _} = application:ensure_all_started(exometer_core),
    ?assertEqual(undefined, whereis(Cache)),
    try
        %% z_site_sup registers statistics before starting the site's cache.
        ?assertEqual(ok, z_stats:init_site(Site)),
        ?assertEqual({ok, [{workers, 0}, {working, 0}]},
            exometer:get_value([site, Site, db, pool])),
        {ok, Pid} = depcache:start_link(Cache, []),
        try
            ok = depcache:set(test_key, <<"cached value">>, Cache),
            ?assertEqual({ok, [{size, depcache:size(Cache)}]},
                exometer:get_value([site, Site, depcache, size])),
            %% Re-registering after startup must remain safe too.
            ?assertEqual(ok, z_stats:init_site(Site))
        after
            gen_server:stop(Pid)
        end
    after
        lists:foreach(
            fun(Path) -> exometer:delete([site, Site | Path]) end,
            [[db, pool], [depcache, size], [broker, session_count], [broker, router_info]])
    end.
