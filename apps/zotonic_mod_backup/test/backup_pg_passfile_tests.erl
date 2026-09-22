-module(backup_pg_passfile_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

socket_password_file_test() ->
    DataDir = filename:join("/tmp", "z_pgpass_test_" ++ os:getpid()
        ++ "_" ++ integer_to_list(erlang:unique_integer([positive]))),
    Saved = application:get_env(zotonic, data_dir),
    %% Use an installed application with no legacy priv/files directory.
    Context = #context{site = zotonic_mod_backup},
    try
        application:set_env(zotonic, data_dir, DataDir),
        lists:foreach(
            fun({Host, ExpectedHost}) ->
                Opts = [{dbhost, Host}, {dbport, 5433}, {dbdatabase, "test"},
                        {dbuser, "tester"}, {dbpassword, "test-password"}],
                {ok, Path} = backup_create:pg_passfile(Opts, Context),
                {ok, Contents} = file:read_file(Path),
                Suffix = <<":5433:test:tester:test-password\n">>,
                ?assertEqual(iolist_to_binary([
                    ExpectedHost, Suffix, "localhost", Suffix
                ]), Contents)
            end,
            [{socket, "/run/postgresql"}, {"socket", "/run/postgresql"},
             {<<"socket">>, "/run/postgresql"},
             {"/custom/postgresql", "/custom/postgresql"},
             {<<"/custom/postgresql">>, "/custom/postgresql"}])
    after
        case Saved of
            undefined -> application:unset_env(zotonic, data_dir);
            {ok, Value} -> application:set_env(zotonic, data_dir, Value)
        end,
        file:del_dir_r(DataDir)
    end.
