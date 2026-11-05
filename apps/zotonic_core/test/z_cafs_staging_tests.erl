-module(z_cafs_staging_tests).
-include_lib("eunit/include/eunit.hrl").

%% Mock setup and teardown must not affect the live integration-test site.
staging_test_() ->
    z_test_support:isolated(fun staging/0).

staging() ->
    Root = filename:join("/tmp", "cafs-staging-test-" ++ binary_to_list(z_ids:id())),
    File = filename:join(Root, "source"),
    ok = filelib:ensure_dir(File),
    ok = file:write_file(File, <<"original bytes">>),
    Modules = [m_site, z_path, z_media_archive, z_db, z_crypto],
    try
        meck:new(Modules, [passthrough, no_link]),
        meck:expect(m_site, get, fun(dbdatabase, _) -> test end),
        meck:expect(z_path, files_subdir, fun(Path, _) -> filename:join(Root, Path) end),
        meck:expect(z_media_archive, abspath, fun(_, _) -> File end),
        meck:expect(z_db, q1, fun(_, _, _) -> undefined end),
        %% Stop at the publication boundary; inspect the real copy and hash work.
        meck:expect(z_db, transaction, fun(_, _) -> {error, publication_boundary} end),
        Medium = #{<<"filename">> => <<"upload">>, <<"is_deletable_file">> => true},
        ?assertEqual({error, publication_boundary}, m_cafs:stage_medium(Medium, <<"upload">>, test)),
        ?assertEqual(1, meck:num_calls(z_crypto, hex_sha2_file, '_')),
        ?assertEqual(0, meck:num_calls(z_crypto, hex_sha2_file, [File])),
        meck:reset(z_crypto),
        ?assertEqual({error, publication_boundary}, m_cafs:stage_medium(Medium, test)),
        ?assertEqual(2, meck:num_calls(z_crypto, hex_sha2_file, '_')),
        meck:reset(z_crypto),
        ?assertEqual({error, publication_boundary}, m_cafs:stage_medium(Medium, <<"other-upload">>, test)),
        ?assertEqual(2, meck:num_calls(z_crypto, hex_sha2_file, '_')),
        meck:reset(z_crypto),
        Poster = #{<<"preview_filename">> => <<"upload">>, <<"is_deletable_preview">> => true},
        ?assertEqual({error, publication_boundary}, m_cafs:stage_medium(Poster, <<"upload">>, test)),
        ?assertEqual(2, meck:num_calls(z_crypto, hex_sha2_file, '_')),
        meck:reset(z_db),
        meck:expect(z_crypto, hex_sha2_file, fun(Path) ->
            Result = meck:passthrough([Path]),
            case Path of
                File -> ok = file:write_file(File, <<"changed bytes">>);
                _ -> ok
            end,
            Result
        end),
        ?assertEqual({error, source_changed}, m_cafs:stage_original(File, test)),
        ?assertEqual(0, meck:num_calls(z_db, transaction, '_')),
        ?assertEqual([], filelib:fold_files(Root, ".*\\.staging$", true,
            fun(Path, Acc) -> [Path|Acc] end, []))
    after
        meck:unload(Modules),
        file:del_dir_r(Root)
    end.
