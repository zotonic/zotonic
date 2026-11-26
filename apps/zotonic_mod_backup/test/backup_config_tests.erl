-module(backup_config_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

database_backup_timeout_test() ->
    Context = #context{site = zotonic_mod_backup},
    Saved = application:get_all_env(zotonic_mod_backup),
    meck:new(m_config, [passthrough]),
    try
        application:set_env(zotonic_mod_backup, is_config_locked, false),
        application:unset_env(zotonic_mod_backup, database_backup_timeout),
        lists:foreach(
            fun(Value) ->
                meck:expect(m_config, get_value,
                    fun(mod_backup, database_backup_timeout, _) -> Value end),
                ?assertEqual(7200, backup_config:database_backup_timeout(Context))
            end,
            [undefined, <<>>, <<"invalid">>, <<"0">>, -1]),
        meck:expect(m_config, get_value,
            fun(mod_backup, database_backup_timeout, _) -> <<"10800">> end),
        ?assertEqual(10800, backup_config:database_backup_timeout(Context)),
        application:set_env(zotonic_mod_backup, database_backup_timeout, 3600),
        ?assertEqual(10800, backup_config:database_backup_timeout(Context)),
        meck:expect(m_config, get_value,
            fun(mod_backup, database_backup_timeout, _) -> undefined end),
        ?assertEqual(3600, backup_config:database_backup_timeout(Context)),
        application:set_env(zotonic_mod_backup, is_config_locked, true),
        meck:expect(m_config, get_value,
            fun(mod_backup, database_backup_timeout, _) -> <<"10800">> end),
        ?assertEqual(3600, backup_config:database_backup_timeout(Context))
    after
        meck:unload(m_config),
        lists:foreach(
            fun(Key) ->
                case proplists:lookup(Key, Saved) of
                    none -> application:unset_env(zotonic_mod_backup, Key);
                    {Key, Value} -> application:set_env(zotonic_mod_backup, Key, Value)
                end
            end,
            [is_config_locked, database_backup_timeout])
    end.
