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
