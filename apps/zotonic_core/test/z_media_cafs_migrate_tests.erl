-module(z_media_cafs_migrate_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").
-export([observe_filestore/2, observe_requested_filestore/4]).

permission_test() ->
    ?assertEqual({error, eacces}, m_media:migrate_to_cafs(
        z_context:new(zotonic_site_testsandbox))).

migration_test_() -> {timeout, 120, fun migration/0}.
migration() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    WasLocalKeep = m_config:get_value(mod_filestore,is_local_keep,C),
    ok = m_config:set_value(mod_filestore,is_local_keep,true,C),
    ok = m_filestore:install(install, C),
    ok = z_notifier:observe(filestore, {?MODULE, observe_filestore, []}, self(), 1, C),
    Tag = z_ids:id(),
    Data = <<"migration-", Tag/binary>>,
    Hash = z_crypto:hex_sha2(Data),
    Names = [<<"migration/", Tag/binary, S/binary>> || S <- [<<"-a">>, <<"-b">>, <<"-poster">>]],
    Files = [z_media_archive:abspath(N, C) || N <- Names],
    lists:foreach(fun(F) -> ok = z_filelib:ensure_dir(F), ok = file:write_file(F, Data) end, Files),
    try
        Result = z_db:transaction(fun(Ctx) ->
            [A, B, Poster] = Names,
            Id = medium(A, undefined, Poster, Ctx),
            Id2 = medium(B, Hash, undefined, Ctx),
            ok = m_media:migrate_to_cafs(Ctx),
            ?assertMatch({delay, 1, _}, z_media_cafs_migrate:run(Id-1, Id2, Ctx)),
            ok = z_media_cafs_migrate:migrate(Id, Ctx),
            ok = z_media_cafs_migrate:migrate(Id2, Ctx),
            lists:foreach(fun(N) -> ?assertEqual({ok, Hash}, m_cafs:lookup(N, Ctx)) end, Names),
            ?assertEqual(Hash, z_db:q1("select digest from medium where id=$1", [Id], Ctx)),
            ?assertEqual(Poster, z_db:q1("select preview_filename from medium where id=$1", [Id], Ctx)),
            ?assert(filelib:is_regular(z_path:files_subdir(m_cafs:path(Hash), Ctx))),
            lists:foreach(fun(N) -> ok = z_media_cafs_migrate:cleanup(N, Hash, Ctx) end, Names),
            lists:foreach(fun(F) -> ?assertNot(filelib:is_regular(F)) end, Files),
            %% Replaying the scan never needs the removed legacy bytes.
            ok = z_media_cafs_migrate:migrate(Id, Ctx),
            %% Unowned files remain outside CAFS, even when local bytes exist.
            Unowned = <<"migration/", Tag/binary, "-unowned">>,
            UnownedId = medium(Unowned, undefined, undefined, Ctx),
            {ok, _} = z_db:update(medium, UnownedId, #{is_deletable_file => false}, Ctx),
            ok = z_media_cafs_migrate:migrate(UnownedId, Ctx),
            ?assertEqual({error, enoent}, m_cafs:lookup(Unowned, Ctx)),
            remote_cases(Tag, Ctx),
            promotion_cases(Tag, Ctx),
            throw({rollback, checked})
        end, C),
        ?assertEqual({rollback, checked}, Result)
    after
        m_config:set_value(mod_filestore,is_local_keep,WasLocalKeep,C),
        z_notifier:detach(filestore, self(), C),
        z_db:q("delete from pivot_task_queue where module='z_media_cafs_migrate'", C),
        lists:foreach(fun file:delete/1, Files),
        file:delete(z_path:files_subdir(m_cafs:path(Hash), C))
    end.

remote_cases(Tag, C) ->
    Name = <<"migration/remote-", Tag/binary>>,
    Hash = z_crypto:hex_sha2(Name),
    Path = <<"archive/", Name/binary>>,
    Target = m_cafs:path(Hash),
    Location = <<"https://example.test/", Name/binary>>,
    Id = medium(Name, Hash, undefined, C),
    {ok, _} = m_filestore:store(Path, 1, <<"s3">>, Location, false, C),
    %% Queued old uploads must finish before we alias or retire their location.
    ok = m_filestore:queue(Path, #{}, C),
    ?assertMatch({delay, 600, _}, z_media_cafs_migrate:migrate(Id, C)),
    z_db:q("delete from filestore_queue where path=$1", [Path], C),
    %% A move-to-local worker may still write the old path: wait for it.
    z_db:q("update filestore set is_move_to_local=true where path=$1", [Path], C),
    ?assertMatch({delay, 600, _}, z_media_cafs_migrate:migrate(Id, C)),
    z_db:q("update filestore set is_move_to_local=false, delete_started=true where path=$1", [Path], C),
    ?assertMatch({delay, 600, _}, z_media_cafs_migrate:migrate(Id, C)),
    z_db:q("update filestore set delete_started=false where path=$1", [Path], C),
    ok = z_media_cafs_migrate:migrate(Id, C),
    ?assertEqual({ok, Hash}, m_cafs:lookup(Name, C)),
    ?assertNot(filelib:is_regular(z_path:files_subdir(Target, C))),
    ?assertEqual(Location, z_db:q1("select location from filestore where path=$1", [Target], C)),
    ok = z_media_cafs_migrate:cleanup(Name, Hash, C),
    ?assertEqual({error, enoent}, m_filestore:lookup(Path, C)),
    ?assertMatch({ok, #{is_deleted := false}}, m_filestore:lookup(Target, C)),
    %% A long legacy name uses a bounded cleanup task key.
    LongName = <<Name/binary, (binary:copy(<<"x">>, 150))/binary>>,
    LongId = medium(LongName, Hash, undefined, C),
    ok = z_media_cafs_migrate:migrate(LongId, C),
    ?assertEqual({ok, Hash}, m_cafs:lookup(LongName, C)),
    %% No checksum and no local bytes: keep the old remote original untouched.
    Unknown = <<Name/binary, "-unknown">>,
    UnknownPath = <<"archive/", Unknown/binary>>,
    UnknownId = medium(Unknown, undefined, undefined, C),
    {ok, _} = m_filestore:store(UnknownPath, 1, <<"s3">>, <<Location/binary, "-unknown">>, false, C),
    ok = z_media_cafs_migrate:migrate(UnknownId, C),
    ?assertEqual({error, enoent}, m_cafs:lookup(Unknown, C)),
    ?assertMatch({ok, _}, m_filestore:lookup(UnknownPath, C)),
    %% Distinct legacy copies are retired only after the target is healthy.
    Old = <<Name/binary, "-duplicate">>,
    OldPath = <<"archive/", Old/binary>>,
    {ok, _} = m_filestore:store(OldPath, 1, <<"s3">>, <<Location/binary, "-duplicate">>, false, C),
    ok = m_filestore:cafs_migrate(cafs_retire, OldPath, Target, C),
    ?assertEqual(true, z_db:q1("select is_deleted from filestore where path=$1", [OldPath], C)),
    Waiting = <<OldPath/binary, "-waiting">>,
    {ok, _} = m_filestore:store(Waiting, 1, <<"s3">>, <<Location/binary, "-waiting">>, false, C),
    ?assertEqual({error, unavailable}, m_filestore:cafs_migrate(cafs_retire,
        Waiting, <<Target/binary, "-missing">>, C)),
    ?assertMatch({ok, #{is_deleted := false}}, m_filestore:lookup(Waiting, C)).

medium(Name, Hash, Poster, C) ->
    {ok, Id} = m_rsc:insert(#{category => media, title => <<"CAFS migration test">>}, C),
    {ok, _} = z_db:insert(medium, #{id => Id, filename => Name,
        digest => Hash, size => 1, is_deletable_file => true,
        preview_filename => Poster, is_deletable_preview => Poster =/= undefined}, C),
    Id.

observe_filestore(#filestore{action=Action}=Request, C)
        when Action =:= cafs_adopt; Action =:= cafs_retire ->
    mod_filestore:observe_filestore(Request, C);
observe_filestore(#filestore{action=lookup}, _) -> error(unexpected_download);
observe_filestore(_, _) -> undefined.

promotion_cases(Tag, C) ->
    lists:foreach(fun(Busy) ->
        Name = <<"migration/promote-", Tag/binary, (atom_to_binary(Busy))/binary>>,
        File = z_media_archive:abspath(Name, C),
        Hash = z_crypto:hex_sha2(Name),
        Target = m_cafs:path(Hash),
        Source = <<"archive/", Name/binary>>,
        Location = <<"https://example.test/", Name/binary>>,
        ok = z_filelib:ensure_dir(File),
        ok = file:write_file(File, Name),
        _Id = medium(Name, undefined, undefined, C),
        {ok, _} = m_filestore:store(Source, byte_size(Name), <<"s3">>, Location, true, C),
        case Busy of true -> ok = m_filestore:queue(Source, #{}, C); false -> ok end,
        try
            {ok, Hash} = m_cafs:promote(Name, File, C),
            ?assertEqual(false, z_db:q1("select upload_pending from cafs where hash=$1", [Hash], C)),
            ?assertEqual(0, z_db:q1("select count(*) from filestore_queue where path=$1", [Target], C)),
            case Busy of
                true ->
                    ?assertMatch({delay,600,_}, z_media_cafs_migrate:cleanup(Name, Hash, C)),
                    ?assert(filelib:is_regular(File)),
                    z_db:q("delete from filestore_queue where path=$1", [Source], C);
                false -> ok
            end,
            ok = z_media_cafs_migrate:cleanup(Name, Hash, C),
            ?assertNot(filelib:is_regular(File)),
            ?assertEqual(Location, z_db:q1("select location from filestore where path=$1", [Target], C)),
            ?assertEqual({error, enoent}, m_filestore:lookup(Source, C)),
            ?assertEqual(false, z_db:q1("select upload_pending from cafs where hash=$1", [Hash], C)),
            preview_cleanup_case(Name, Hash, Busy, C)
        after
            file:delete(File),
            file:delete(z_path:files_subdir(Target, C))
        end
    end, [false, true]).

%% Existing remote previews are retargeted, not uploaded again. Pending legacy
%% uploads postpone retirement, and local bytes survive until adoption succeeds.
preview_cleanup_case(Name, SourceHash, Busy, C) ->
    Path = <<Name/binary, "-preview">>,
    Physical = <<"preview/", Path/binary>>,
    File = z_path:files_subdir(Physical, C),
    Location = <<"https://example.test/", Physical/binary>>,
    ok = z_filelib:ensure_dir(File),
    ok = file:write_file(File, Path),
    {ok, #{hash := Hash, token := Pin}} = m_cafs:stage_original(File, C),
    try
        z_db:q("insert into cafs_derivative (source_hash,operations,renderer_version,output_hash)
            values ($1,$2,$3,$4)", [SourceHash, Path, <<"test">>, Hash], C),
        ok = m_cafs:unpin(Hash, Pin, C),
        {ok, _} = m_filestore:store(Physical, byte_size(Path), <<"s3">>, Location, true, C),
        case Busy of
            true ->
                ok = m_filestore:queue(Physical, #{}, C),
                ?assertMatch({delay,600,_}, z_media_cafs_migrate:cleanup_preview(Path, Hash, true, C)),
                ?assert(filelib:is_regular(File)),
                ?assertEqual(false, z_db:q1("select upload_pending from cafs where hash=$1", [Hash], C)),
                z_db:q("delete from filestore_queue where path=$1", [Physical], C);
            false -> ok
        end,
        ok = z_media_cafs_migrate:cleanup_preview(Path, Hash, true, C),
        ?assertNot(filelib:is_regular(File)),
        ?assertEqual(Location, z_db:q1("select location from filestore where path=$1", [m_cafs:path(Hash)], C)),
        ?assertEqual({error,enoent}, m_filestore:lookup(Physical, C)),
        ?assertEqual(false, z_db:q1("select upload_pending from cafs where hash=$1", [Hash], C)),
        ?assertEqual(0, z_db:q1("select count(*) from filestore_queue where path=$1", [m_cafs:path(Hash)], C)),
        Different = <<Path/binary, "-different">>,
        DifferentPath = <<"preview/", Different/binary>>,
        DifferentFile = z_path:files_subdir(DifferentPath, C),
        DifferentLocation = <<Location/binary, "-different">>,
        ok = file:write_file(DifferentFile, <<"different bytes">>),
        try
            {ok, _} = m_filestore:store(DifferentPath, 15, <<"s3">>, DifferentLocation, true, C),
            ok = z_media_cafs_migrate:cleanup_preview(Different, Hash, false, C),
            ?assertNot(filelib:is_regular(DifferentFile)),
            ?assertEqual(Location, z_db:q1("select location from filestore where path=$1", [m_cafs:path(Hash)], C)),
            ?assertEqual(true, z_db:q1("select is_deleted from filestore where path=$1", [DifferentPath], C))
        after file:delete(DifferentFile) end
    after
        file:delete(File),
        file:delete(z_path:files_subdir(m_cafs:path(Hash), C))
    end.

requested_original_test_() -> {timeout, 60, fun requested_original/0}.
requested_original() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = m_filestore:install(install, C),
    Tag = z_ids:id(),
    Local = <<"requested/", Tag/binary, "-local">>,
    Remote = <<"requested/", Tag/binary, "-remote">>,
    Unowned = <<"requested/", Tag/binary, "-unowned">>,
    Data = <<"requested original ", Tag/binary>>,
    RemoteData = <<Data/binary, " remote">>,
    LocalHash = z_crypto:hex_sha2(Data),
    RemoteHash = z_crypto:hex_sha2(RemoteData),
    LocalFile = z_media_archive:abspath(Local, C),
    UnownedFile = z_media_archive:abspath(Unowned, C),
    ok = z_filelib:ensure_dir(LocalFile),
    ok = file:write_file(LocalFile, Data),
    ok = file:write_file(UnownedFile, Data),
    LocalId = medium(Local, undefined, undefined, C),
    RemoteId = medium(Remote, undefined, undefined, C),
    UnownedId = medium(Unowned, undefined, undefined, C),
    {ok, _} = z_db:update(medium, UnownedId, #{is_deletable_file => false}, C),
    Source = <<"archive/", Remote/binary>>,
    Location = <<"https://example.test/", Remote/binary>>,
    {ok, _} = m_filestore:store(Source, byte_size(RemoteData), <<"s3">>, Location, false, C),
    {ok, CachePid} = filezcache:insert_stream(Location, byte_size(RemoteData), []),
    ok = z_notifier:observe(filestore,
        {?MODULE, observe_requested_filestore, [Source, CachePid]}, self(), 1, C),
    try
        ?assertMatch([#part_file{}], z_file_locate:locate_file(Local, C)),
        ?assertMatch([#part_file{}], z_file_locate:locate_file(Unowned, C)),
        %% Lookup returns before the requested remote download is complete.
        ?assertMatch([#part_cache{}], z_file_locate:locate_file(Remote, C)),
        ?assertEqual({error,enoent}, m_cafs:lookup(Remote, C)),
        ok = filezcache:append_stream(CachePid, RemoteData),
        ok = filezcache:finish_stream(CachePid),
        {ok, {file, _, Downloaded}} = filezcache:lookup_file(Location, [lock]),
        ?assertEqual({ok, RemoteData}, file:read_file(Downloaded)),
        CacheMonitor = erlang:monitor(process,CachePid),
        receive {'DOWN',CacheMonitor,process,CachePid,_} -> ok after 5000 -> error(cache_not_registered) end,
        %% A completed legacy cache remains usable after its download PID exits.
        ?assertMatch({ok,{file,_,_}},z_file_request:cache_file(
            #part_cache{cache_pid=CachePid,cache_key=Location})),
        wait_mapping(Local, LocalHash, 100, C),
        wait_mapping(Remote, RemoteHash, 100, C),
        ?assertEqual(LocalHash, z_db:q1("select digest from medium where id=$1", [LocalId], C)),
        ?assertEqual(RemoteHash, z_db:q1("select digest from medium where id=$1", [RemoteId], C)),
        ?assertEqual(RemoteHash, maps:get(<<"digest">>, m_media:get_by_filename(Remote, C))),
        ?assertEqual({error,enoent}, m_cafs:lookup(Unowned, C)),
        ok = z_media_cafs_migrate:cleanup(Local, LocalHash, C),
        ok = z_media_cafs_migrate:cleanup(Remote, RemoteHash, C),
        ?assertNot(filelib:is_regular(LocalFile)),
        ?assertEqual(Location, z_db:q1("select location from filestore where path=$1", [m_cafs:path(RemoteHash)], C)),
        ?assertEqual(false, z_db:q1("select upload_pending from cafs where hash=$1", [RemoteHash], C)),
        %% Replacing a medium while bytes are in flight must not attach them.
        Stale = m_media:get(RemoteId, C),
        WaitingKey = {?MODULE, Tag, stale},
        {ok, WaitingPid} = filezcache:insert_stream(WaitingKey, undefined, []),
        z_db:q("delete from filename_cafs where filename=$1", [Remote], C),
        {ok, Worker} = z_sidejob:start(z_media_cafs_migrate, promote_requested,
            [Remote, {cache, WaitingKey}, Stale], C),
        Monitor = erlang:monitor(process, Worker),
        {ok, _} = z_db:update(medium, RemoteId, #{filename => <<Remote/binary, "-replaced">>}, C),
        ok = filezcache:append_stream(WaitingPid, RemoteData),
        ok = filezcache:finish_stream(WaitingPid),
        receive {'DOWN', Monitor, process, Worker, normal} -> ok after 5000 -> error(promotion_timeout) end,
        ?assertEqual({error,enoent}, m_cafs:lookup(Remote, C))
    after
        z_notifier:detach(filestore, self(), C),
        lists:foreach(fun(Id) -> m_rsc:delete(Id, C) end, [LocalId,RemoteId,UnownedId]),
        file:delete(LocalFile), file:delete(UnownedFile),
        z_db:q("delete from pivot_task_queue where module='z_media_cafs_migrate'", C)
    end.

observe_requested_filestore(<<"archive/", Name/binary>> = Source, Pid, #filestore{action=lookup,path=Source}, _C) ->
    {ok, {filezcache, Pid, #{created => calendar:universal_time(), size => 1,
        location => <<"https://example.test/", Name/binary>>}}};
observe_requested_filestore(_, _, #filestore{action=lookup}, _) -> undefined;
observe_requested_filestore(_, _, Request, C) -> observe_filestore(Request, C).

wait_mapping(Name, _Hash, 0, C) ->
    Worker = gproc:where({n,l,{z_media_cafs_migrate,requested,z_context:site(C),Name}}),
    Info = case Worker of undefined -> undefined; _ -> process_info(Worker, current_stacktrace) end,
    error({promotion_timeout,Name,Info});
wait_mapping(Name, Hash, N, C) ->
    case m_cafs:lookup(Name, C) of
        {ok, Hash} ->
            case gproc:where({n,l,{z_media_cafs_migrate,requested,z_context:site(C),Name}}) of
                undefined -> ok;
                _ -> timer:sleep(20), wait_mapping(Name, Hash, N-1, C)
            end;
        {error, enoent} -> timer:sleep(20), wait_mapping(Name, Hash, N-1, C)
    end.

atomic_cleanup_test_() -> {timeout,60,fun atomic_cleanup/0}.
atomic_cleanup() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    Name = <<"atomic/",(z_ids:id())/binary>>,
    File = z_tempfile:new(),
    ok = file:write_file(File,Name),
    {ok,#{hash:=Hash}=Staged} = m_cafs:stage_original(File,C),
    Key = z_crypto:hex_sha2(Name),
    try
        Result = z_db:transaction(fun(Ctx) ->
            ok = m_cafs:attach_original(Name,Staged,Ctx),
            ?assertEqual(1,z_db:q1("select count(*) from pivot_task_queue
                where module='z_media_cafs_migrate' and function='cleanup' and key=$1",[Key],Ctx)),
            throw({rollback,checked})
        end,C),
        ?assertEqual({rollback,checked},Result),
        ?assertEqual({error,enoent},m_cafs:lookup(Name,C)),
        ?assertEqual(0,z_db:q1("select count(*) from pivot_task_queue
            where module='z_media_cafs_migrate' and function='cleanup' and key=$1",[Key],C))
    after
        m_cafs:release([{Name,Staged}],C),
        file:delete(File),file:delete(z_path:files_subdir(m_cafs:path(Hash),C))
    end.

offload_pin_test_() -> {timeout,60,fun offload_pin/0}.
offload_pin() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = m_filestore:install(install,C),
    WasLocalKeep = m_config:get_value(mod_filestore,is_local_keep,C),
    ok = m_config:set_value(mod_filestore,is_local_keep,false,C),
    File = z_tempfile:new(),
    ok = file:write_file(File,<<"offload ",(z_ids:id())/binary>>),
    {ok,#{hash:=Hash}=Staged} = m_cafs:stage_original(File,C),
    Path = m_cafs:path(Hash),
    Local = z_path:files_subdir(Path,C),
    Source = <<"archive/offload/",Hash/binary>>,
    Location = <<"https://example.test/offload/",Hash/binary>>,
    try
        {ok,_} = m_filestore:store(Source,filelib:file_size(Local),<<"s3">>,Location,false,C),
        ok = z_db:transaction(fun(Ctx) ->
            _ = z_db:q1("select hash from cafs where hash=$1 for update",[Hash],Ctx),
            m_filestore:cafs_migrate(cafs_adopt,Source,Path,Ctx)
        end,C),
        ?assertEqual(Location,z_db:q1("select location from filestore where path=$1",[Path],C)),
        ?assertEqual(1,z_db:q1("select count(*) from filestore_queue where path=$1",[Path],C)),
        ?assertEqual({error,busy},m_cafs:with_path_pin(Path,fun() ->
            m_cafs:delete_uploaded_file(Path,C)
        end,C)),
        ?assert(filelib:is_regular(Local)),
        ok = m_cafs:release([{<<"offload">>,Staged}],C),
        ok = m_cafs:track(Path,C),
        ?assertEqual({error,busy},m_cafs:with_path_pin(Path,fun() ->
            m_cafs:delete_uploaded_file(Path,C)
        end,C)),
        true = gproc:unreg({p,l,{m_cafs,z_context:site(C),Hash}}),
        ?assertEqual(ok,m_cafs:with_path_pin(Path,fun() ->
            m_cafs:delete_uploaded_file(Path,C)
        end,C)),
        ?assertNot(filelib:is_regular(Local))
    after
        m_config:set_value(mod_filestore,is_local_keep,WasLocalKeep,C),
        z_db:q("delete from filestore_queue where path=$1",[Path],C),
        m_cafs:release([{<<"offload">>,Staged}],C),file:delete(File),file:delete(Local)
    end.
