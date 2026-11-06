%% Copyright 2026 Marc Worrell. SPDX-License-Identifier: Apache-2.0
%% @doc Regression tests for shared preview ownership and media operation budgets.
-module(z_media_reliability_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").

error_policy_test() ->
    ?assertEqual(retry, z_media_error:classify({media_runner_processing, <<"worker_failed">>})),
    ?assertEqual(retry, z_media_error:classify({media_runner_processing, <<"job_expired">>})),
    ?assertEqual(terminal, z_media_error:classify({media_runner_processing, <<"command_failed">>})),
    ?assertEqual(pause, z_media_error:classify(media_runner_invalid_result)),
    ?assertEqual(obsolete, z_media_error:classify(stale_medium)).

deadline_and_cleanup_test() ->
    Parent = self(),
    Path = filename:join("/tmp", "media-deadline-" ++ binary_to_list(z_ids:id())),
    ?assertMatch({error, {media_runner_unavailable, operation_timeout}},
        z_media_deadline:run(fun() ->
            Guard = z_media_temp:guard(Path, file),
            ok = file:write_file(Path, <<"partial">>),
            Parent ! {guard, Guard},
            receive never -> ok end
        end, erlang:monotonic_time(millisecond) + 100)),
    Guard = receive {guard, G} -> G after 1000 -> error(no_guard) end,
    Ref = monitor(process, Guard),
    receive {'DOWN', Ref, process, Guard, _} -> ok after 1000 -> error(guard_leaked) end,
    ?assertNot(filelib:is_file(Path)).

expired_install_test() ->
    ?assertEqual({error, {media_runner_unavailable, {download, timeout}}},
        z_media_runner_protocol:unpack(#{<<"status">> => <<"ok">>, <<"stdout">> => <<>>,
            <<"files">> => []}, #{media_runner_deadline => erlang:monotonic_time(millisecond)-1})).

shared_preview_wait_test_() -> {timeout, 30, fun shared_preview_wait/0}.
shared_preview_wait() ->
    C = z_context:new(zotonic_site_testsandbox),
    Path = <<"reliability-", (z_ids:id())/binary, ".jpg">>,
    Parent = self(),
    meck:new(z_file_locate, [passthrough, no_link]),
    meck:expect(z_file_locate, locate_sources, fun(_, _, [P], _) when P =:= Path ->
        Parent ! {resolving, self()},
        receive finish -> [#part_data{data= <<"real preview">>, modified={{2026,1,1},{0,0,0}}}] end
    end),
    try
        ?assertEqual({error, pending}, z_file_request:lookup_file(Path, [], undefined, 20, C)),
        Worker = receive {resolving, W} -> W after 1000 -> error(no_worker) end,
        ?assert(is_process_alive(Worker)),
        spawn(fun() -> Parent ! {lookup, z_file_request:lookup_file(Path, C)} end),
        Worker ! finish,
        receive {lookup, {ok, #z_file_info{size=12}}} -> ok after 5000 -> error(no_preview) end,
        ?assertMatch({ok, _}, z_file_request:lookup_file(Path, C)),
        ?assertEqual(1, meck:num_calls(z_file_locate, locate_sources, '_')),
        %% The resolver keeps process-owned file protections until the entry stops.
        ?assert(is_process_alive(Worker)),
        Pid = z_file_entry:where(Path, C),
        Ref = monitor(process, Worker),
        exit(Pid, kill),
        receive {'DOWN', Ref, process, Worker, _} -> ok after 1000 -> error(worker_leaked) end
    after
        z_file_request:stop(Path, C),
        meck:unload(z_file_locate)
    end.

http_cold_preview_wait_test_() -> {timeout, 15, fun http_cold_preview_wait/0}.
http_cold_preview_wait() ->
    C0 = z_context:new(zotonic_site_testsandbox),
    Path = <<"http-cold-preview-", (z_ids:id())/binary, ".jpg">>,
    %% Include all required cowboy_req:req() fields, even those unused by this controller.
    Req = #{method => <<"GET">>, version => 'HTTP/1.1',
        path => <<"/image/", Path/binary>>, qs => <<>>,
        scheme => <<"https">>, host => <<"localhost">>, port => 443,
        headers => #{}, peer => {{127,0,0,1}, 12345}, sock => {{127,0,0,1}, 443},
        cert => undefined, ref => test_listener, pid => self(), streamid => 1,
        has_body => false, body_length => 0},
    Env = #{cowmachine_remote => <<"127.0.0.1">>, cowmachine_proxy => false,
        cowmachine_forwarded_proto => <<"https">>},
    C = z_context:set([{q, []}, {path, Path}], C0#context{cowreq = Req, cowenv = Env}),
    meck:new(z_file_locate, [passthrough, no_link]),
    meck:expect(z_file_locate, locate_sources, fun(_, _, [P], _) when P =:= Path ->
        %% A cold download/conversion must survive the former five-second limit.
        timer:sleep(5500),
        [#part_data{data = <<"real preview">>, modified = {{2026,1,1},{0,0,0}}}]
    end),
    try
        {Available, Result} = controller_file:service_available(C),
        ?assertEqual(true, Available),
        ?assertMatch(#z_file_info{size = 12}, z_context:get(controller_file, Result)),
        ?assertEqual(1, meck:num_calls(z_file_locate, locate_sources, '_'))
    after
        z_file_request:stop(Path, C),
        meck:unload(z_file_locate)
    end.

preview_error_backoff_test() ->
    C = z_context:new(zotonic_site_testsandbox),
    Path = <<"retry-", (z_ids:id())/binary, ".jpg">>,
    meck:new(z_file_locate, [passthrough, no_link]),
    meck:expect(z_file_locate, locate_sources, fun(_, _, _, _) ->
        throw({file_unavailable, {media_runner_unavailable, timeout}})
    end),
    try
        Error = {error, {media_runner_unavailable, timeout}},
        ?assertEqual(Error, z_file_request:lookup_file(Path, C)),
        ?assertEqual(Error, z_file_request:lookup_file(Path, C)),
        ?assertEqual(1, meck:num_calls(z_file_locate, locate_sources, '_'))
    after
        z_file_request:stop(Path, C),
        meck:unload(z_file_locate)
    end.

orphan_sweep_test() ->
    Dir = filename:join("/tmp", "media-sweep-" ++ binary_to_list(z_ids:id())),
    Orphan = filename:join(Dir, ".download-old"),
    Active = filename:join(Dir, ".download-active"),
    Preview = filename:join(Dir, "real-preview.jpg"),
    ok = file:make_dir(Dir),
    Guard = z_media_temp:guard(Active, file),
    try
        lists:foreach(fun(P) ->
            ok = file:write_file(P, <<"bytes">>),
            ok = file:change_time(P, {{2020,1,1},{0,0,0}})
        end, [Orphan, Active, Preview]),
        %% Synchronize with the guard's registration using a monitor-independent
        %% gproc lookup; the sweep must preserve active artifacts regardless of age.
        await_guard(Active, 100),
        ?assertEqual([], z_media_temp:sweep([Dir], z_context:new(zotonic_site_testsandbox))),
        ?assertNot(filelib:is_file(Orphan)),
        ?assert(filelib:is_file(Active)),
        ?assert(filelib:is_file(Preview))
    after
        z_media_temp:release(Guard),
        file:del_dir_r(Dir)
    end.

await_guard(_Path, 0) -> error(guard_not_registered);
await_guard(Path, N) ->
    case gproc:lookup_pids({p, l, {z_media_temp, filename:absname(z_convert:to_binary(Path))}}) of
        [] -> timer:sleep(10), await_guard(Path, N-1);
        [_] -> ok
    end.

failed_cache_source_test_() -> {timeout, 10, fun failed_cache_source/0}.
failed_cache_source() ->
    C = z_context:new(zotonic_site_testsandbox),
    Path = <<"failed-cache-", (z_ids:id())/binary, ".jpg">>,
    Parent = self(),
    Cache = spawn(fun() -> receive stop -> ok end end),
    meck:new(z_file_locate, [passthrough, no_link]),
    meck:expect(z_file_locate, locate_sources, fun(_, _, _, _) ->
        Parent ! {cache_resolver, self()},
        [#part_cache{cache_pid=Cache, cache_key=Cache,
            cache_monitor=monitor(process, Cache), size=12, mime= <<"image/jpeg">>,
            modified={{2026,1,1},{0,0,0}}}]
    end),
    try
        ?assertMatch({ok, _}, z_file_request:lookup_file(Path, C)),
        Resolver = receive {cache_resolver, P} -> P after 1000 -> error(no_resolver) end,
        Entry = z_file_entry:where(Path, C),
        EntryRef = monitor(process, Entry),
        ResolverRef = monitor(process, Resolver),
        exit(Cache, kill),
        receive {'DOWN', ResolverRef, process, Resolver, _} -> ok
            after 1000 -> error(resolver_leaked) end,
        ?assertEqual({error, pending}, z_file_entry:lookup(Entry)),
        ?assertEqual(1, meck:num_calls(z_file_locate, locate_sources, '_')),
        %% The cooldown retires the entry without another request or download.
        receive {'DOWN', EntryRef, process, Entry, normal} -> ok
            after 3000 -> error(background_retry_loop) end,
        ?assertEqual(1, meck:num_calls(z_file_locate, locate_sources, '_')),
        meck:expect(z_file_locate, locate_sources, fun(_, _, _, _) ->
            [#part_data{data= <<"recovered">>, modified={{2026,1,1},{0,0,0}}}]
        end),
        ?assertMatch({ok, #z_file_info{size=9}}, z_file_request:lookup_file(Path, C))
    after
        exit(Cache, kill),
        z_file_request:stop(Path, C),
        meck:unload(z_file_locate)
    end.
