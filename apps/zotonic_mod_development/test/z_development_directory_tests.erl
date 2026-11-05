-module(z_development_directory_tests).
-include_lib("eunit/include/eunit.hrl").

%% These mocks replace core modules used by live site workers (including logging).
%% Isolate setup and cleanup as well as the assertions from the integration VM.
directory_access_test_() ->
    z_test_support:isolated(directory_access()).

directory_access() ->
    {setup,
        fun() ->
            meck:new([z_acl, m_site, z_development_editor, m_admin_status, z_crypto], [no_link]),
            meck:new([z_context, z_sites_config], [passthrough, no_link]),
            meck:expect(z_context, site, fun(C) when is_map(C) -> zotonic_site_testsandbox; (C) -> meck:passthrough([C]) end),
            meck:expect(z_sites_config, config_files, fun(zotonic_site_testsandbox) -> [?FILE] end),
            meck:expect(z_acl, is_admin, fun(C) -> maps:get(admin, C, false) end),
            meck:expect(z_acl, is_allowed, fun(use, mod_development, C) -> maps:get(development_access, C, false) end),
            meck:expect(m_site, environment, fun(C) -> maps:get(environment, C, production) end),
            meck:expect(z_development_editor, directory_opener, fun() -> "/usr/bin/open" end),
            meck:expect(z_development_editor, open_directory, fun(Path) -> {opened, Path} end),
            meck:expect(z_crypto, pickle, fun({development_editor_config, _}, _) -> <<"signed-config">> end),
            meck:expect(z_crypto, depickle, fun(<<"config-token">>, _) ->
                {development_editor_config, <<"/not-an-init-config.config">>}
            end),
            meck:expect(m_admin_status, m_get, fun([<<"log_dir">>], undefined, _) ->
                {ok, {<<"/tmp/log directory with spaces">>, []}}
            end)
        end,
        fun(_) -> meck:unload([z_acl, m_site, z_development_editor, m_admin_status, z_crypto, z_context, z_sites_config]) end,
        fun(_) ->
            Admin = #{admin => true, development_access => true, environment => development},
            [
                ?_assertMatch({ok, {#{file := _, token := <<"signed-config">>}, []}},
                    m_development:m_get([<<"config_source">>, unicode:characters_to_binary(?FILE)], undefined, Admin)),
                ?_assertEqual({error, eacces}, m_development:m_get([<<"config_source">>, <<"/etc/passwd">>], undefined, #{})),
                ?_assertEqual({error, enoent}, m_development:m_get([<<"config_source">>, <<"/etc/passwd">>], undefined, Admin)),
                ?_assertEqual({error, eacces}, m_development:m_post([<<"editor">>, <<"open">>],
                    #{payload => #{<<"source">> => <<"config-token">>, <<"editor">> => <<"auto">>}}, Admin)),
                ?_assertEqual({error, eacces}, m_development:m_post([<<"editor">>, <<"open">>],
                    #{payload => #{<<"source">> => <<"config-token">>, <<"editor">> => <<"auto">>}}, Admin#{admin => false})),
                ?_assertEqual({ok, {true, []}}, m_development:m_get([<<"can_open_directory">>], undefined, Admin)),
                ?_assertEqual({opened, <<"/tmp/log directory with spaces">>}, m_development:open_directory(<<"log_dir">>, Admin)),
                ?_assertEqual({error, enoent}, m_development:open_directory(<<"/tmp/arbitrary">>, Admin)),
                ?_assertEqual({error, enoent}, m_development:open_directory(<<"../log_dir">>, Admin)),
                ?_assertEqual({error, eacces}, m_development:open_directory(<<"log_dir">>, Admin#{admin => false})),
                ?_assertEqual({error, eacces}, m_development:open_directory(<<"log_dir">>, Admin#{development_access => false})),
                ?_assertEqual({error, eacces}, m_development:open_directory(<<"log_dir">>, Admin#{environment => production})),
                ?_assertEqual({ok, {false, []}}, m_development:m_get([<<"can_open_directory">>], undefined, #{}))
            ]
        end}.
