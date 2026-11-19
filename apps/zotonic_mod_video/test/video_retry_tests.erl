%% Copyright 2026 Marc Worrell. SPDX-License-Identifier: Apache-2.0
%% @doc Conversion failures retain both legacy and CAFS inputs across retry workers.
-module(video_retry_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

retry_test_() -> {timeout, 60, fun retry/0}.
retry() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    case jobs:queue_info(video_jobs) of
        undefined -> jobs:add_queue(video_jobs, []);
        _ -> ok
    end,
    Modules = [m_media, z_video_info, z_exec],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    try
        meck:expect(z_video_info, info, fun(_, _) -> #{} end),
        lists:foreach(fun(Version) -> retry_source(Version, C) end, [v2, v3])
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

retry_source(Version, C) ->
    Nr = z_ids:id(),
    Name = <<"retry-", Nr/binary>>,
    Queue = mod_video:queue_path(Name, C),
    ok = filelib:ensure_dir(Queue),
    ok = file:write_file(Queue, <<"original video bytes">>),
    Owner = <<"video:", Name/binary>>,
    {ok, #{hash := Hash, token := Token}} = m_cafs:stage_file(Queue, C),
    ok = m_cafs:pin(Hash, Owner, C),
    ok = m_cafs:unpin(Hash, Token, C),
    Medium = #{<<"video_processing_nr">> => Nr},
    Upload = #media_upload_preprocess{mime= <<"video/avi">>, medium=#{}},
    V2 = {convert_v2, 1, Medium, Upload, Name, Nr, z_context:pickle(C)},
    Task = case Version of
        v2 -> V2;
        v3 -> {convert_v3, 1, Medium, Upload, Name, Nr, z_context:pickle(C), Hash}
    end,
    meck:expect(m_media, get, fun(1, _) -> Medium; (Id, Ctx) -> meck:passthrough([Id, Ctx]) end),
    meck:expect(m_media, replace_medium, fun(_, _, _, _, _) -> error(unexpected_broken_medium) end),
    meck:expect(z_exec, run, fun(_, _, _, _) ->
        {error, {media_runner_processing, <<"worker_failed">>}}
    end),
    try
        run_worker(Task, C),
        [Retained = {convert_v4, Task, #{attempts := 1, paused := false}}] = task_args(Name, C),
        assert_source(Version, Queue, Hash, Owner, C),
        run_worker(Retained, C),
        [{convert_v4, Task, #{attempts := 2} = Retry}] = task_args(Name, C),
        %% Expiring the automatic retry window pauses without dropping the input.
        run_worker({convert_v4, Task, Retry#{started => erlang:system_time(second)-86401}}, C),
        [Paused = {convert_v4, Task, #{paused := true}}] = task_args(Name, C),
        assert_source(Version, Queue, Hash, Owner, C),
        Calls = meck:num_calls(z_exec, run, '_'),
        run_worker(Paused, C),
        ?assertEqual(Calls, meck:num_calls(z_exec, run, '_')),
        %% A successful render with a failed medium commit must still retain input.
        meck:expect(z_exec, run, fun(_, _, #{write := [Output]}, _) ->
            file:write_file(Output, <<"converted">>), {ok, <<>>}
        end),
        meck:expect(m_media, replace_file, fun(_, _, _, _, _, _) -> {error, database_unavailable} end),
        run_worker(Task, C),
        [{convert_v4, Task, #{paused := true, last_reason := database_unavailable}}] = task_args(Name, C),
        assert_source(Version, Queue, Hash, Owner, C),
        %% Commit succeeds on the next explicit retry.
        meck:expect(m_media, replace_file, fun(_, _, _, _, _, _) -> {ok, 1} end),
        run_worker(Task, C),
        ?assertEqual([], task_args(Name, C)),
        case Version of
            v2 -> ?assertNot(filelib:is_file(Queue));
            v3 -> ?assertEqual(0, pin_count(Hash, Owner, C))
        end,
        %% Paused work is reconciled when its processing identity is replaced.
        meck:expect(m_media, get, fun(1, _) -> #{}; (Id, Ctx) -> meck:passthrough([Id, Ctx]) end),
        run_worker(Paused, C),
        ?assertEqual([], task_args(Name, C))
    after
        mod_video:remove_task(Name, C),
        m_cafs:unpin(Hash, Owner, C),
        file:delete(Queue)
    end.

run_worker(Task, C) ->
    {ok, Pid} = z_video_convert:start_link(Task, C),
    unlink(Pid),
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok;
        {'DOWN', Ref, process, Pid, noproc} -> ok;
        {'DOWN', Ref, process, Pid, Reason} -> error({worker_failed, Reason})
    after 10000 -> error(worker_timeout)
    end.

task_args(Name, C) ->
    case z_db:q1("select props from pivot_task_queue where module='mod_video' and function='convert_task' and key=$1", [Name], C) of
        undefined -> [];
        Props -> maps:get(<<"args">>, Props)
    end.

assert_source(v2, Queue, _, _, _) -> ?assert(filelib:is_file(Queue));
assert_source(v3, _, Hash, Owner, C) -> ?assertEqual(1, pin_count(Hash, Owner, C)).

pin_count(Hash, Owner, C) ->
    z_db:q1("select count(*) from cafs_pin where hash=$1 and owner=$2", [Hash, Owner], C).
