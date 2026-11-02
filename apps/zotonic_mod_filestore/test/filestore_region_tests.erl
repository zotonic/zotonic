%% @doc Region selection and propagation through filestore credential consumers.
-module(filestore_region_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").

credentials_test() ->
    Base = #{username => <<"key">>, password => <<"secret">>, tls_options => []},
    ?assertEqual(Base, filestore_config:credentials(<<"s3">>, <<"key">>, <<"secret">>, <<>>, [])),
    ?assertEqual(Base, filestore_config:credentials(<<"s3">>, <<"key">>, <<"secret">>, <<" \t ">>, [])),
    ?assertEqual(Base#{signature_version => v4, region => <<"eu-west-1">>},
        filestore_config:credentials(<<"s3">>, <<"key">>, <<"secret">>, <<" eu-west-1 ">>, [])),
    lists:foreach(fun(Service) ->
        ?assertEqual(Base, filestore_config:credentials(Service, <<"key">>, <<"secret">>, <<"eu-west-1">>, []))
    end, [<<"ftp">>, <<"webdav">>]).

region_configuration_test() ->
    Context = #context{site = zotonic_site_testsandbox},
    Saved = [{K, application:get_env(zotonic_mod_filestore, K)} || K <- [s3region, is_config_locked]],
    meck:new(m_config, [passthrough, no_link]),
    try
        application:set_env(zotonic_mod_filestore, is_config_locked, false),
        application:set_env(zotonic_mod_filestore, s3region, <<" eu-central-1 ">>),
        meck:expect(m_config, get_value, fun(mod_filestore, s3region, _) -> undefined end),
        ?assertEqual(<<"eu-central-1">>, filestore_config:s3region(Context)),
        meck:expect(m_config, get_value, fun(mod_filestore, s3region, _) -> <<>> end),
        ?assertEqual(<<>>, filestore_config:s3region(Context)),
        meck:expect(m_config, get_value, fun(mod_filestore, s3region, _) -> <<" eu-west-1 ">> end),
        ?assertEqual(<<"eu-west-1">>, filestore_config:s3region(Context)),
        application:set_env(zotonic_mod_filestore, is_config_locked, true),
        ?assertEqual(<<"eu-central-1">>, filestore_config:s3region(Context))
    after
        meck:unload(m_config),
        lists:foreach(fun
            ({K, undefined}) -> application:unset_env(zotonic_mod_filestore, K);
            ({K, {ok, V}}) -> application:set_env(zotonic_mod_filestore, K, V)
        end, Saved)
    end.

credential_consumers_test() ->
    SavedLock = application:get_env(zotonic_mod_filestore, is_config_locked),
    Modules = [m_config, s3filez],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    try
        application:set_env(zotonic_mod_filestore, is_config_locked, false),
        lists:foreach(fun check_consumers/1, [<<>>, <<"eu-west-1">>])
    after
        lists:foreach(fun meck:unload/1, Modules),
        case SavedLock of
            undefined -> application:unset_env(zotonic_mod_filestore, is_config_locked);
            {ok, Value} -> application:set_env(zotonic_mod_filestore, is_config_locked, Value)
        end
    end.

check_consumers(Region) ->
    Settings = #{
        service => <<"s3">>,
        s3url => <<"https://storage.example/bucket">>,
        s3key => <<"key">>,
        s3secret => <<"secret">>,
        tls_options => [{verify, verify_peer}],
        s3region => Region
    },
    meck:expect(m_config, get_value, fun(mod_filestore, Key, _) -> maps:get(Key, Settings, undefined) end),
    Context = #context{site = zotonic_site_testsandbox},
    Expected = filestore_config:credentials(Context),
    {ok, #filestore_credentials{credentials = Forward}} =
        mod_filestore:observe_filestore_credentials_lookup(
            #filestore_credentials_lookup{path = <<"archive/test.txt">>}, Context),
    {ok, #filestore_credentials{credentials = Reverse}} =
        mod_filestore:observe_filestore_credentials_revlookup(
            #filestore_credentials_revlookup{service = <<"s3">>, location = <<"https://storage.example/bucket/archive/test.txt">>}, Context),
    ?assertEqual(Expected, Forward),
    ?assertEqual(Expected, Reverse),
    % Force bucket creation, then verify every retry/operation keeps the same credentials.
    erase(filestore_region_test_data),
    meck:expect(s3filez, put, fun(Cred, _, {data, Data}) ->
        ?assertEqual(Expected, Cred),
        case put(filestore_region_test_data, Data) of
            undefined -> {error, enoent};
            _ -> ok
        end
    end),
    meck:expect(s3filez, create_bucket, fun(Cred, _) -> ?assertEqual(Expected, Cred), ok end),
    meck:expect(s3filez, get, fun(Cred, _) ->
        ?assertEqual(Expected, Cred),
        {ok, <<"text/plain">>, get(filestore_region_test_data)}
    end),
    meck:expect(s3filez, delete, fun(Cred, _) -> ?assertEqual(Expected, Cred), ok end),
    ?assertEqual(ok, filestore_admin:testcred(Context)),
    erase(filestore_region_test_data).
