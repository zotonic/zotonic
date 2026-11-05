-module(filestore_download_integrity_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

bulk_download_test_() -> {timeout, 60, fun bulk_download/0}.

bulk_download() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = m_filestore:install(install, C),
    meck:new(filestore_config, [passthrough, no_link]),
    meck:expect(filestore_config, is_local_keep, fun(_) -> true end),
    try
        lists:foreach(fun(Mode) -> check_download(Mode, C) end,
            [valid, move, checksum, truncated, timeout, write_error, stale])
    after meck:unload(filestore_config) end.

check_download(Mode, C) ->
    meck:expect(filestore_config, is_local_keep, fun(_) -> Mode =:= valid end),
    Bytes = z_ids:id(),
    Hash = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    Path = m_cafs:path(Hash),
    File = z_path:files_subdir(Path, C),
    ok = z_filelib:ensure_dir(File),
    {ok, Id} = m_filestore:store(Path, byte_size(Bytes), <<"ftp">>,
        <<"https://example.test/", Path/binary>>, false, C),
    ok = m_filestore:mark_move_to_local(Id, C),
    {ok, #{download_token := Token} = Claim} = m_filestore:claim_download(Id, C),
    Ctx = z_context:set(filestore_download_claim, Claim, C),
    Temp = <<(z_convert:to_binary(File))/binary, ".downloading-", Token/binary>>,
    Send = fun(Data) -> mod_filestore:download_stream(Id, Path, File, Ctx, Data) end,
    try
        ok = Send(stream_start),
        case Mode of
            write_error ->
                ok = file:delete(Temp),
                ok = file:make_dir(Temp),
                ?assertError({download_write, eisdir}, Send(Bytes)),
                ok = file:del_dir(Temp);
            timeout -> ok = Send({error, timeout});
            _ ->
                Data = case Mode of
                    checksum -> binary:copy(<<"!">>, byte_size(Bytes));
                    truncated -> <<"x">>;
                    _ -> Bytes
                end,
                ok = Send(Data),
                case Mode of
                    stale -> z_db:q("update filestore set download_token='newer' where id=$1", [Id], C);
                    _ -> ok
                end,
                ok = Send(eof)
        end,
        {ok, Entry} = z_db:qmap_row("select * from filestore where id=$1", [Id], [{keys, atom}], C),
        ?assertEqual(Mode =:= move, maps:get(is_deleted, Entry)),
        case Mode of
            Good when Good =:= valid; Good =:= move ->
                ?assertEqual({ok, Bytes}, file:read_file(File)),
                ?assertEqual(false, maps:get(is_move_to_local, Entry)),
                ?assertEqual(true, maps:get(is_local, Entry));
            _ ->
                ?assertNot(filelib:is_regular(File)),
                ?assertEqual(true, maps:get(is_move_to_local, Entry)),
                case Mode of
                    stale -> ?assertEqual(<<"newer">>, maps:get(download_token, Entry));
                    _ ->
                        ?assertNotEqual(undefined, maps:get(error, Entry)),
                        {ok, Waiting} = m_filestore:fetch_move_to_local(C),
                        ?assertNot(lists:any(fun(#{id := I}) -> I =:= Id end, Waiting)),
                        z_db:q("update filestore set retry_after=now()-interval '1 second' where id=$1", [Id], C),
                        {ok, Retrying} = m_filestore:fetch_move_to_local(C),
                        ?assert(lists:any(fun(#{id := I}) -> I =:= Id end, Retrying))
                end
        end
    after
        file:delete(Temp), file:delete(File),
        z_db:q("delete from filestore where id=$1", [Id], C)
    end.

adoption_and_stats_test_() -> {timeout, 60, fun adoption_and_stats/0}.
adoption_and_stats() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = m_filestore:install(install, C),
    Bytes = z_ids:id(),
    {ok, Id} = m_media:insert_file(#upload{filename= <<"stats.txt">>, data=Bytes}, #{}, [], C),
    #{<<"filename">> := Name} = m_media:get(Id, C),
    {ok, Hash} = m_cafs:lookup(Name, C),
    Path = m_cafs:path(Hash),
    Old = <<"archive/", Name/binary>>,
    {ok, RemoteId} = m_filestore:store(Old, byte_size(Bytes), <<"ftp">>,
        <<"https://example.test/", Old/binary>>, false, C),
    try
        ok = z_db:transaction(fun(Ctx) ->
            m_filestore:cafs_migrate(cafs_adopt, Old, Path, Ctx)
        end, C),
        {ok, Entry} = m_filestore:lookup(Path, C),
        ?assertEqual(true, maps:get(is_local, Entry)),
        Before = m_filestore:stats(C),
        ok = m_filestore:mark_remote(Path, C),
        After = m_filestore:stats(C),
        ?assertEqual(maps:get(local, Before)-1, maps:get(local, After)),
        ?assertEqual(maps:get(local_size, Before)-byte_size(Bytes), maps:get(local_size, After))
    after
        z_db:q("delete from filestore where id=$1", [RemoteId], C),
        z_db:q("delete from filestore_queue where path=$1", [Path], C),
        m_rsc:delete(Id, C)
    end.
