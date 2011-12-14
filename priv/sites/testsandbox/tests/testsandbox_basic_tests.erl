-module(testsandbox_basic_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test if the site-specific testsandbox module is installed and running.
testsandbox_enabled_test() ->
    C = z_context:new(testsandbox),
    ?assertEqual(z_module_manager:active(testsandbox, C), true),
    ok.

%% Test if some default modules are running.
default_modules_test() ->
    C = z_context:new(testsandbox),
    ?assertEqual(z_module_manager:active(mod_base, C), true),
    ?assertEqual(z_module_manager:active(mod_admin, C), true),
    ok.

%% Test if the webserver is running, by looking at the home tpl.
testsandbox_site_up_test() ->
    C = z_context:new(testsandbox),
    X = httpc:request(get, {"http://" ++ z_context:hostname_port(C) ++ "/", []}, [], []),
    ?assertMatch({ok,{{"HTTP/1.1",200,"OK"}, _, _}}, X),
    {ok,{_, _, Body}} = X,
    ?assertMatch({match, _}, re:run(Body, "<title>Zotonic test sandbox</title>")),
    ok.

