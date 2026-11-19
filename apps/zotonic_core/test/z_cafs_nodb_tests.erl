%% Copyright 2026 Marc Worrell. SPDX-License-Identifier: Apache-2.0
%% @doc Library previews on an actual database-free site must remain local.
-module(z_cafs_nodb_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").

configuration_test() ->
    C = z_context:new(zotonic_site_testsandbox),
    ok = meck:new(m_site, [passthrough, no_link]),
    try
        lists:foreach(fun(Value) ->
            ok = meck:expect(m_site, get, fun
                (dbdatabase, #context{site=zotonic_site_testsandbox}) -> Value;
                (Key, Ctx) -> meck:passthrough([Key, Ctx])
            end),
            ?assertNot(m_cafs:is_enabled(C))
        end, [none, <<"none">>, "none"]),
        lists:foreach(fun(Value) ->
            ok = meck:expect(m_site, get, fun
                (dbdatabase, #context{site=zotonic_site_testsandbox}) -> Value;
                (Key, Ctx) -> meck:passthrough([Key, Ctx])
            end),
            %% An absent pool is not a reason to change storage models.
            ?assert(m_cafs:is_enabled(C#context{db={missing_database_pool,z_db_pgsql}}))
        end, [undefined, <<"configured_database">>])
    after meck:unload(m_site) end.

library_preview_test_() -> {timeout, 120, fun library_preview/0}.
library_preview() ->
    %% The test node deliberately discovers only testsandbox. Start the real
    %% status-site supervisor with its real configuration, outside discovery.
    {ok, [Config]} = file:consult(filename:join(code:priv_dir(zotonic_site_status), "zotonic_site.config")),
    Parent = self(),
    ok = meck:new(z_sites_manager, [passthrough, no_link]),
    ok = meck:expect(z_sites_manager, get_site_config, fun
        (zotonic_site_status) -> {ok, Config};
        (Site) -> meck:passthrough([Site])
    end),
    ok = meck:expect(z_sites_manager, set_site_status, fun
        (zotonic_site_status, running) -> Parent ! status_ready, ok;
        (zotonic_site_status, _) -> ok;
        (Site, Status) -> meck:passthrough([Site, Status])
    end),
    {ok, Sup} = z_site_sup:start_link(zotonic_site_status),
    unlink(Sup),
    try
        receive status_ready -> ok after 30000 -> error(status_startup_timeout) end,
        check_library_preview()
    after
        gen_server:stop(Sup, shutdown, 30000),
        meck:unload(z_sites_manager)
    end.

check_library_preview() ->
    C = z_context:new(zotonic_site_status),
    ?assertNot(z_db:has_connection(C)),
    ?assertNot(m_cafs:is_enabled(C)),
    ?assertEqual({error, nodb}, m_cafs:stage_file(<<"not-read">>, C)),
    ?assertEqual({error, enoent}, m_cafs:lookup(<<"not-read">>, C)),
    Modules = [z_db, m_media, z_notifier, z_media_preview],
    lists:foreach(fun(M) -> ok = meck:new(M, [passthrough, no_link]) end, Modules),
    Count = ets:new(preview_count, [public]),
    ets:insert(Count, {renders, 0}),
    try
        %% Catch accidental reliance on the no-database SQL wrappers.
        lists:foreach(fun(Function) ->
            ok = meck:expect(z_db, Function, fun
                (_, #context{site=zotonic_site_status}) -> error(unexpected_database_query);
                (Sql, Ctx) -> meck:passthrough([Sql, Ctx])
            end),
            ok = meck:expect(z_db, Function, fun
                (_, _, #context{site=zotonic_site_status}) -> error(unexpected_database_query);
                (_, #context{site=zotonic_site_status}, _) -> error(unexpected_database_query);
                (A, B, D) -> meck:passthrough([A, B, D])
            end),
            ok = meck:expect(z_db, Function, fun
                (_, _, #context{site=zotonic_site_status}, _) -> error(unexpected_database_query);
                (Sql, Args, Ctx, Timeout) -> meck:passthrough([Sql, Args, Ctx, Timeout])
            end)
        end, [q, q1]),
        ok = meck:expect(m_media, identify, fun
            (_, #context{site=zotonic_site_status}) -> error(unexpected_medium_identification);
            (File, Ctx) -> meck:passthrough([File, Ctx])
        end),
        ok = meck:expect(z_notifier, first, fun
            (#filestore{}, #context{site=zotonic_site_status}) -> error(unexpected_filestore);
            (Request, Ctx) -> meck:passthrough([Request, Ctx])
        end),
        ok = meck:expect(z_media_preview, convert, fun(Input, Output, Filters, Ctx) ->
            ets:update_counter(Count, renders, 1),
            meck:passthrough([Input, Output, Filters, Ctx])
        end),
        {ok, Url} = z_media_tag:url(<<"lib/images/koe.jpg">>, [{width,57}], C),
        [_, Encoded] = binary:split(Url, <<"/image/">>),
        Path = z_url:url_decode(Encoded),
        Output = filename:join(z_path:media_preview(C), Path),
        file:delete(Output),
        z_file_request:force_stale(Path, C),
        try
            ?assertEqual(z_convert:to_binary(Output), preview(Path, C)),
            ?assert(filelib:is_regular(Output)),
            ?assertEqual(1, ets:lookup_element(Count, renders, 2)),
            ?assertEqual(z_convert:to_binary(Output), preview(Path, C)),
            timer:sleep(2100),
            ?assertEqual(z_convert:to_binary(Output), preview(Path, C)),
            ?assertEqual(1, ets:lookup_element(Count, renders, 2)),
            ?assertNot(z_cafs_preview:expired(Path, C)),
            ok = m_cafs:install(C),
            ok = m_cafs:maintenance(C),
            ok = z_cafs_preview:maintenance(C),
            Cleanup = z_utils:name_for_site(z_media_cleanup_server, C),
            z_media_cleanup_server:cleanup(C),
            Cleanup ! timeout,
            _ = sys:get_state(Cleanup, 30000),
            ?assert(filelib:is_regular(Output)),
            ?assertMatch({error, _}, z_media_tag:url2props(<<Path/binary, ".png">>, C)),
            ?assertMatch({error, _}, z_file_request:lookup_file(<<"lib/images/missing-cafs-test.jpg">>, C)),
            ok = file:delete(Output),
            z_file_request:force_stale(Path, C),
            ?assertEqual(z_convert:to_binary(Output), preview(Path, C)),
            ?assertEqual(2, ets:lookup_element(Count, renders, 2))
        after file:delete(Output), z_file_request:stop(Path, C) end
    after
        lists:foreach(fun meck:unload/1, lists:reverse(Modules)),
        ets:delete(Count)
    end.

preview(Path, C) ->
    {ok, Info} = z_file_request:lookup_file(Path, C),
    {ok, Local} = z_file_request:content_file(Info, C),
    z_convert:to_binary(Local).
