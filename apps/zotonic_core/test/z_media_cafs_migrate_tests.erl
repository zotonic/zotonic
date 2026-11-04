-module(z_media_cafs_migrate_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").
-export([observe_filestore/2]).

permission_test() ->
    ?assertEqual({error, eacces}, m_media:migrate_to_cafs(
        z_context:new(zotonic_site_testsandbox))).

migration_test_() -> {timeout, 120, fun migration/0}.
migration() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
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
            throw({rollback, checked})
        end, C),
        ?assertEqual({rollback, checked}, Result)
    after
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
