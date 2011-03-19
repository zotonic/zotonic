%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(z_depcache_tests).

-include_lib("zotonic.hrl").

-include_lib("eunit/include/eunit.hrl").


%% simple_test() ->
%% 	C = z_context:new(testsandbox),
%% 	z_depcache:flush(test_m_key, C),
%% 	[ test_m(C) || _N <- lists:seq(1,100) ],
%% 	ok.

%% test_m(Context) ->
%%     ?DEBUG(z_depcache:memo(fun test_f/0, test_m_key, Context)).

%% test_f() ->
%%     ?DEBUG(waiting),
%%     receive after 5000 -> y end.


get_set_test() ->
    C = z_context:new(testsandbox),
    z_depcache:flush(C),

    ?assertEqual(undefined, z_depcache:get(test_key, C)),
    z_depcache:set(test_key, 123, C),
    ?assertEqual({ok,123}, z_depcache:get(test_key, C)),
    z_depcache:flush(test_key, C),
    ?assertEqual(undefined, z_depcache:get(test_key, C)).


flush_all_test() ->
    C = z_context:new(testsandbox),
    z_depcache:flush(C),

    z_depcache:set(test_key, 123, C),
    z_depcache:set(test_key2, 123, C),
    z_depcache:set(test_key3, 123, C),
    ?assertEqual({ok,123}, z_depcache:get(test_key, C)),
    ?assertEqual({ok,123}, z_depcache:get(test_key2, C)),
    ?assertEqual({ok,123}, z_depcache:get(test_key3, C)),
    z_depcache:flush(C),
    ?assertEqual(undefined, z_depcache:get(test_key, C)),
    ?assertEqual(undefined, z_depcache:get(test_key2, C)),
    ?assertEqual(undefined, z_depcache:get(test_key3, C)).


get_set_maxage_test() ->
    C = z_context:new(testsandbox),
    z_depcache:flush(C),

    ?assertEqual(undefined, z_depcache:get(xtest_key, C)),

    %% Set a key and hold it for one second.
    z_depcache:set(xtest_key, 123, 1, C),
    ?assertEqual({ok,123}, z_depcache:get(xtest_key, C)),

    %% Let the depcache time out.
    receive after 2000 -> ok end,
    ?assertEqual(undefined, z_depcache:get(xtest_key, C)).


get_set_maxage0_test() ->
    C = z_context:new(testsandbox),
    z_depcache:flush(C),

    ?assertEqual(undefined, z_depcache:get(test_key, C)),

    %% Set a key and hold it for 0 seconds
    z_depcache:set(test_key, 123, 0, C),
    ?assertEqual(undefined, z_depcache:get(test_key, C)).


get_set_depend_test() ->
    C = z_context:new(testsandbox),
    z_depcache:flush(C),

    ?assertEqual(undefined, z_depcache:get(test_key, C)),

    %% Set a key  and hold it for one second.
    z_depcache:set(test_key, 123, 10, [test_key_dep], C),
    ?assertEqual({ok,123}, z_depcache:get(test_key, C)),

    %% flush the dependency; test_key should be gone as well.
    z_depcache:flush(test_key_dep, C),
    ?assertEqual(undefined, z_depcache:get(test_key, C)).


memo_test() ->
    C = z_context:new(testsandbox),
    z_depcache:flush(C),

    IncreaserFun = fun() ->
                           I = case erlang:get(incr) of
                                   undefined -> 1;
                                   Num -> Num + 1
                               end,
                           erlang:put(incr, I),
                           I
                   end,
    
    ?assertEqual(1, z_depcache:memo(IncreaserFun, test_key, C)), % uncached
    ?assertEqual(1, z_depcache:memo(IncreaserFun, test_key, C)), % cached (no increase)
    z_depcache:flush(test_key, C),
    ?assertEqual(2, z_depcache:memo(IncreaserFun, test_key, C)), % uncached again
    ?assertEqual(2, z_depcache:memo(IncreaserFun, test_key, C)). % cached again
