-module(z_db_pgsql_tests).

-include_lib("eunit/include/eunit.hrl").

libpq_host_test() ->
    lists:foreach(
        fun(Host) ->
            ?assertEqual("/run/postgresql", z_db_pgsql:normalize_host(Host))
        end,
        [socket, "socket", <<"socket">>]),
    lists:foreach(
        fun(Host) ->
            ?assertEqual(Host, z_db_pgsql:normalize_host(Host))
        end,
        ["/custom/postgresql", <<"/custom/postgresql">>, "localhost", "db.example.com", {127, 0, 0, 1}]).

socket_options_test() ->
    lists:foreach(
        fun(Host) ->
            Opts = options(Host, 5433),
            ?assertEqual({local, "/run/postgresql/.s.PGSQL.5433"}, maps:get(host, Opts)),
            ?assertEqual(0, maps:get(port, Opts)),
            ?assertEqual("test", maps:get(database, Opts)),
            ?assertEqual("tester", maps:get(username, Opts))
        end,
        [socket, "socket", <<"socket">>]).

custom_socket_directory_test() ->
    lists:foreach(
        fun(Host) ->
            Opts = options(Host, 5434),
            ?assertEqual({local, "/custom/postgresql/.s.PGSQL.5434"}, maps:get(host, Opts)),
            ?assertEqual(0, maps:get(port, Opts))
        end,
        ["/custom/postgresql", "/custom/postgresql/", <<"/custom/postgresql">>]).

tcp_options_test() ->
    lists:foreach(
        fun(Host) ->
            Opts = options(Host, 5433),
            ?assertEqual(Host, maps:get(host, Opts)),
            ?assertEqual(5433, maps:get(port, Opts))
        end,
        ["localhost", "db.example.com", "127.0.0.1", {127, 0, 0, 1}, {0, 0, 0, 0, 0, 0, 0, 1}]).

global_socket_defaults_test() ->
    Keys = [dbhost, dbport],
    Saved = [{Key, application:get_env(zotonic, Key)} || Key <- Keys],
    EnvKeys = ["ZOTONIC_DBHOST", "ZOTONIC_DBPORT"],
    SavedEnv = [{Key, os:getenv(Key)} || Key <- EnvKeys],
    try
        %% Environment overrides (including CI's database host) take precedence.
        lists:foreach(fun os:unsetenv/1, EnvKeys),
        application:set_env(zotonic, dbhost, "socket"),
        application:set_env(zotonic, dbport, 5435),
        %% A site port of 0 still inherits the global PostgreSQL port for the filename.
        Opts = options(undefined, 0),
        ?assertEqual({local, "/run/postgresql/.s.PGSQL.5435"}, maps:get(host, Opts)),
        ?assertEqual(0, maps:get(port, Opts))
    after
        lists:foreach(
            fun
                ({Key, undefined}) -> application:unset_env(zotonic, Key);
                ({Key, {ok, Value}}) -> application:set_env(zotonic, Key, Value)
            end,
            Saved),
        lists:foreach(
            fun
                ({Key, false}) -> os:unsetenv(Key);
                ({Key, Value}) -> os:putenv(Key, Value)
            end,
            SavedEnv)
    end.

options(Host, Port) ->
    z_db_pgsql:build_connect_options("test", [
        {dbhost, Host},
        {dbport, Port},
        {dbuser, "tester"},
        {dbpassword, "test-password"}
    ]).
