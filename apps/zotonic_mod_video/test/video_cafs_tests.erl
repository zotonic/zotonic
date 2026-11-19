%% Copyright 2026 Marc Worrell. SPDX-License-Identifier: Apache-2.0
%% @doc Exercise a durable CAFS input through the real ffmpeg conversion pipeline.
-module(video_cafs_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

conversion_test_() -> {timeout, 120, fun conversion/0}.

conversion() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_module_manager:activate_await(mod_video, C),
    File = z_tempfile:new(".avi"),
    Command = "ffmpeg -v error -f lavfi -i color=c=blue:s=32x32:r=10 "
        "-t 0.3 -c:v mpeg4 -y " ++ z_filelib:os_filename(File),
    {ok, _} = z_exec:run(Command),
    Parent = self(),
    ok = meck:new(z_video_convert, [passthrough, no_link]),
    try
        %% Hold startup only: the task and durable source must exist before the
        %% worker starts. The subsequent conversion uses the real implementation.
        ok = meck:expect(z_video_convert, start_link, fun(Task, _) ->
            Parent ! {video_task, Task},
            ignore
        end),
        {ok, Id} = m_media:insert_file(#upload{filename= <<"cafs-test.avi">>, tmpfile=File}, C),
        Task = receive {video_task, T} -> T after 5000 -> error(no_video_task) end,
        {convert_v3, Id, _, _, QueueName, _, _, SourceHash} = Task,
        Owner = <<"video:", QueueName/binary>>,
        ?assertEqual(1, z_db:q1("select count(*) from cafs_pin where hash=$1 and owner=$2",
            [SourceHash, Owner], C)),
        ok = meck:unload(z_video_convert),
        {delay, _} = mod_video:convert_task(Task, C),
        try
            Medium = await_video(Id, 600, C),
            #{ <<"filename">> := Name, <<"preview_filename">> := Preview } = Medium,
            ?assertMatch({ok, _}, m_cafs:lookup(Name, C)),
            ?assertMatch({ok, _}, m_cafs:lookup(Preview, C)),
            await_unpin(SourceHash, Owner, 100, C)
        after m_rsc:delete(Id, C) end
    after
        catch meck:unload(z_video_convert),
        file:delete(File),
        z_module_manager:deactivate(mod_video, C)
    end.

await_video(_Id, 0, _C) -> error(video_timeout);
await_video(Id, Retries, C) ->
    case m_media:get(Id, C) of
        #{ <<"is_video_ok">> := true } = Medium -> Medium;
        #{ <<"is_video_broken">> := true } -> error(video_broken);
        _ -> timer:sleep(100), await_video(Id, Retries - 1, C)
    end.

await_unpin(_Hash, _Owner, 0, _C) -> error(pin_not_released);
await_unpin(Hash, Owner, Retries, C) ->
    case z_db:q1("select count(*) from cafs_pin where hash=$1 and owner=$2", [Hash, Owner], C) of
        0 -> ok;
        _ -> timer:sleep(20), await_unpin(Hash, Owner, Retries - 1, C)
    end.
