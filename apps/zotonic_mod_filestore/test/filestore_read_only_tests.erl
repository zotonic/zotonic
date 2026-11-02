%% @doc Download-only policy is additive and applies before provider writes.
-module(filestore_read_only_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").

configuration_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    Saved = application:get_env(zotonic_mod_filestore, is_read_only),
    meck:new(m_site, [passthrough, no_link]),
    meck:new(m_config, [passthrough, no_link]),
    try
        application:unset_env(zotonic_mod_filestore, is_read_only),
        meck:expect(m_site, get, fun(mod_filestore, is_read_only, _) -> undefined end),
        meck:expect(m_config, get_value, fun(mod_filestore, _, _) -> undefined end),
        ?assertNot(filestore_config:is_read_only(Context)),
        application:set_env(zotonic_mod_filestore, is_read_only, true),
        meck:expect(m_config, get_value, fun(mod_filestore, _, _) -> false end),
        ?assert(filestore_config:is_read_only(Context)),
        application:set_env(zotonic_mod_filestore, is_read_only, false),
        meck:expect(m_site, get, fun(mod_filestore, is_read_only, _) -> true end),
        ?assert(filestore_config:is_read_only(Context)),
        meck:expect(m_site, get, fun(mod_filestore, is_read_only, _) -> false end),
        meck:expect(m_config, get_value, fun(mod_filestore, _, _) -> <<"true">> end),
        ?assert(filestore_config:is_read_only(Context)),
        ?assertNot(filestore_config:is_upload_enabled(Context)),
        ?assertEqual(<<"false">>, filestore_config:delete_interval(Context))
    after
        meck:unload(m_site),
        meck:unload(m_config),
        restore_env(Saved)
    end.

writes_blocked_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    Saved = application:get_env(zotonic_mod_filestore, is_read_only),
    try
        application:set_env(zotonic_mod_filestore, is_read_only, true),
        ?assertEqual({error, read_only}, filestore_request:upload(<<"missing">>, <<"remote">>, <<"text/plain">>, Context)),
        ?assertEqual({error, read_only}, filestore_request:delete(<<"remote">>, Context)),
        ?assertEqual({error, read_only}, filestore_admin:testcred(Context)),
        ?assertEqual({error, read_only}, mod_filestore:observe_filestore(
            #filestore{action=upload, path= <<"missing">>, mime= <<"text/plain">>}, Context)),
        ?assertEqual({error, read_only}, filestore_uploader:upload_job(
            1, <<"missing">>, {error, enoent}, #{}, Context)),
        lists:foreach(fun(Service) ->
            Cred = #filestore_credentials{
                service = Service,
                service_url = <<"remote">>,
                location = <<"remote/file">>,
                credentials = #{is_read_only => true}
            },
            ?assertEqual({error, read_only}, filestore_request:do_upload(Cred, {data, <<"test">>}, <<"text/plain">>)),
            ?assertEqual({error, read_only}, filestore_request:do_delete(Cred))
        end, [<<"s3">>, <<"ftp">>, <<"webdav">>])
    after
        restore_env(Saved)
    end.

downloads_allowed_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    Saved = application:get_env(zotonic_mod_filestore, is_read_only),
    Modules = [z_notifier, s3filez, ftpfilez, webdavfilez],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    Filename = filename:join("/tmp", "filestore-readonly-" ++ binary_to_list(z_ids:id())),
    try
        application:set_env(zotonic_mod_filestore, is_read_only, true),
        lists:foreach(fun({Service, Mod}) ->
            meck:expect(z_notifier, first, fun(#filestore_credentials_lookup{}, _) ->
                {ok, #filestore_credentials{
                    service = Service,
                    service_url = <<"remote">>,
                    location = <<"remote/file">>,
                    credentials = #{is_read_only => true}
                }}
            end),
            meck:expect(Mod, stream, fun(_, _, {M, F, A}) ->
                apply(M, F, A ++ [stream_start]),
                apply(M, F, A ++ [<<"downloaded">>]),
                apply(M, F, A ++ [eof]),
                ok
            end),
            ?assertEqual(ok, filestore_request:download(Filename, <<"remote">>, Context)),
            ?assertEqual({ok, <<"downloaded">>}, file:read_file(Filename))
        end, [{<<"s3">>, s3filez}, {<<"ftp">>, ftpfilez}, {<<"webdav">>, webdavfilez}])
    after
        file:delete(Filename),
        lists:foreach(fun meck:unload/1, Modules),
        restore_env(Saved)
    end.

restore_env(undefined) -> application:unset_env(zotonic_mod_filestore, is_read_only);
restore_env({ok, Value}) -> application:set_env(zotonic_mod_filestore, is_read_only, Value).
