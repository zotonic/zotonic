%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @hidden

-module(z_utils_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").


filter_dot_files_test() ->
    ?assertEqual([], z_utils:filter_dot_files([])),
    ?assertEqual(["/test"], z_utils:filter_dot_files(["/test"])),
    ?assertEqual(["/test"], z_utils:filter_dot_files(["/test", "/test/.foo", "/.tmp"])),
    ?assertEqual(["/test", "/test/it"],
        z_utils:filter_dot_files(["/.tmp", "/test", "/test/it", "/test/.foo"])),

    ok.
