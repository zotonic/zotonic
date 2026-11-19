-module(z_media_preview_purge_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").
-export([observe_filestore/2]).

permission_test() ->
    ?assertEqual({error, eacces}, m_media:delete_generated_previews(
        z_context:new(zotonic_site_testsandbox))).

queue_test() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    Result = z_db:transaction(fun(Ctx) ->
        Count = z_db:q1("select count(*) from pivot_task_queue
            where module='z_media_preview_purge' and function='run'", Ctx),
        ok = m_media:delete_generated_previews(Ctx),
        ?assertEqual(Count+1, z_db:q1("select count(*) from pivot_task_queue
            where module='z_media_preview_purge' and function='run'", Ctx)),
        throw({rollback, queue_checked})
    end, C),
    ?assertEqual({rollback, queue_checked}, Result).

purge_test_() -> {timeout, 120, fun purge/0}.
purge() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = m_filestore:install(install, C),
    ok = z_notifier:observe(filestore, {?MODULE, observe_filestore, []}, self(), 1, C),
    Tag = z_ids:id(),
    Image = filename:join(code:priv_dir(zotonic_site_testsandbox), "files/archive/koe.jpg"),
    {ok, ImageData} = file:read_file(Image),
    {ok, Id} = m_media:insert_file(#upload{filename= <<"original.txt">>, data=Tag}, C),
    {ok, Poster} = m_media:save_preview(Id, ImageData, <<"image/jpeg">>, C),
    #{<<"filename">> := Original} = m_media:get(Id, C),
    {ok, Source} = m_cafs:lookup(Original, C),
    {ok, PosterHash} = m_cafs:lookup(Poster, C),
    Output = z_tempfile:new(),
    ok = file:write_file(Output, <<"resized-", Tag/binary>>),
    {ok, OutputHash} = m_cafs:derivative(Source, resized, 1,
        fun() -> m_cafs:stage_file(Output, C) end, C),
    RemoteOutput = z_tempfile:new(),
    ok = file:write_file(RemoteOutput, <<"remote-resized-", Tag/binary>>),
    {ok, RemoteHash} = m_cafs:derivative(Source, remote_resized, 1,
        fun() -> m_cafs:stage_file(RemoteOutput, C) end, C),
    {ok, CafsRemoteId} = store(m_cafs:path(RemoteHash), C),
    %% A derivative can be byte-identical to a stored preview original.
    {ok, PosterHash} = m_cafs:derivative(Source, shared_poster, 1,
        fun() -> m_cafs:stage_file(Image, C) end, C),
    {ok, PosterHash} = m_cafs:derivative(Source, newer, 1,
        fun() -> m_cafs:stage_file(Image, C) end, C),
    z_db:q("update cafs_derivative set created=now()+interval '1 day'
        where source_hash=$1 and operations=$2", [Source, term_to_binary(newer)], C),
    Legacy = <<"purge-test/", Tag/binary, ".jpg">>,
    Local = filename:join(z_path:media_preview(C), Legacy),
    Protected = z_media_archive:abspath(<<"preview/purge-test/", Tag/binary, ".jpg">>, C),
    Symlink = <<Local/binary, ".link">>,
    PrivateDir = filename:join(z_path:media_preview(C), <<".preview-", Tag/binary>>),
    Private = filename:join(PrivateDir, <<"working.jpg">>),
    lists:foreach(fun(P) -> ok = z_filelib:ensure_dir(P), ok = file:write_file(P, Tag) end,
        [Local, Protected, Private]),
    ok = file:make_symlink(Protected, Symlink),
    Remote = <<"preview/", Legacy/binary>>,
    ArchivedPoster = <<"archive/", Poster/binary>>,
    {ok, RemoteId} = store(Remote, C),
    {ok, PosterId} = store(ArchivedPoster, C),
    ok = m_filestore:queue(Remote, #{}, C),
    ok = m_filestore:queue(ArchivedPoster, #{}, C),
    Before = z_db:q1("select clock_timestamp()::text", C),
    try
        %% Read-only and unavailable backends retain the task for retry.
        ok = m_config:set_value(mod_filestore, is_read_only, true, C),
        ?assertEqual({error, read_only}, m_filestore:purge_previews(Before, C)),
        ?assertEqual(false, z_db:q1("select is_deleted from filestore where id=$1", [RemoteId], C)),
        ok = m_config:set_value(mod_filestore, is_read_only, false, C),
        ok = run(Before, erlang:system_time(second), cafs, C, 30),
        ?assertNot(filelib:is_regular(Local)),
        ?assert(filelib:is_regular(Protected)),
        ?assert(filelib:is_regular(Private)),
        ?assertNot(filelib:is_regular(z_path:files_subdir(m_cafs:path(OutputHash), C))),
        ?assertEqual({ok, Source}, m_cafs:lookup(Original, C)),
        ?assertEqual({ok, PosterHash}, m_cafs:lookup(Poster, C)),
        ?assertEqual({ok, ImageData}, m_cafs:with_local_file(PosterHash, fun file:read_file/1, C)),
        ?assertEqual(Poster, maps:get(<<"preview_filename">>, m_media:get(Id, C))),
        ?assertEqual([{term_to_binary(newer)}], z_db:q(
            "select operations from cafs_derivative where source_hash=$1", [Source], C)),
        ?assertEqual(true, z_db:q1("select is_deleted from filestore where id=$1", [RemoteId], C)),
        ?assertEqual(true, z_db:q1("select is_deleted from filestore where id=$1", [CafsRemoteId], C)),
        ?assertEqual(false, z_db:q1("select is_deleted from filestore where id=$1", [PosterId], C)),
        ?assertEqual(0, z_db:q1("select count(*) from filestore_queue where path=$1", [Remote], C)),
        ?assertEqual(1, z_db:q1("select count(*) from filestore_queue where path=$1", [ArchivedPoster], C)),
        ok = z_notifier:detach(filestore, self(), C),
        ?assertMatch({delay, 600, _}, z_media_preview_purge:run(Before, 0, remote, C))
    after
        m_config:set_value(mod_filestore, is_read_only, false, C),
        z_notifier:detach(filestore, self(), C),
        m_rsc:delete(Id, C),
        lists:foreach(fun file:delete/1, [Output, RemoteOutput, Local, Protected, Private, Symlink]),
        file:del_dir(PrivateDir),
        z_db:q("delete from filestore where id in ($1,$2,$3)", [RemoteId, PosterId, CafsRemoteId], C),
        z_db:q("delete from filestore_queue where path in ($1,$2)", [Remote, ArchivedPoster], C),
        z_db:q("delete from cafs_derivative where source_hash=$1", [Source], C)
    end.

active_claim_test_() -> {timeout, 60, fun active_claim/0}.
active_claim() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    File = z_tempfile:new(),
    ok = file:write_file(File, z_ids:id()),
    {ok, #{hash := Hash} = Stage} = m_cafs:stage_file(File, C),
    Name = <<"purge-claim/", (z_ids:id())/binary>>,
    ok = m_cafs:attach(Name, Stage, C),
    try
        z_db:q("insert into cafs_derivative(source_hash,operations,renderer_version,claim_token,claim_until)
            values ($1,$2,$3,'working',now()+interval '5 minutes')",
            [Hash, <<"ops">>, <<"version">>], C),
        Before = z_db:q1("select clock_timestamp()::text", C),
        %% Drain other rows but never revoke an active generation claim.
        busy = drain(Before, C, 30),
        ?assertEqual(<<"working">>, z_db:q1("select claim_token from cafs_derivative where source_hash=$1", [Hash], C)),
        z_db:q("update cafs_derivative set claim_until=now()-interval '1 second' where source_hash=$1", [Hash], C),
        ?assertEqual(done, drain(Before, C, 30))
    after
        file:delete(File),
        z_db:q("delete from cafs_derivative where source_hash=$1", [Hash], C),
        m_cafs:retire_filename(Name, C)
    end.

drain(_Before, _C, 0) -> error(purge_did_not_finish);
drain(Before, C, Left) ->
    case z_cafs_preview:purge_batch(Before, C) of
        more -> drain(Before, C, Left-1);
        Result -> Result
    end.

run(_Before, _LocalBefore, _Phase, _C, 0) -> error(purge_did_not_finish);
run(Before, LocalBefore, Phase, C, Left) ->
    case z_media_preview_purge:run(Before, LocalBefore, Phase, C) of
        ok -> ok;
        {delay, _, [Before, LocalBefore, Next]} -> run(Before, LocalBefore, Next, C, Left-1)
    end.

observe_filestore(#filestore{action=Action}=Request, C)
        when Action =:= delete_status; Action =:= purge_previews ->
    mod_filestore:observe_filestore(Request, C);
observe_filestore(_, _) -> undefined.

store(Path, C) ->
    m_filestore:store(Path, 1, <<"s3">>, <<"https://example.test/", Path/binary>>, false, C).
