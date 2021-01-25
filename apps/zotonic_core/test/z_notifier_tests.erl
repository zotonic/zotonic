%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(z_notifier_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

-export([observer1/2,
         observer2/2
        ]).


observer1({test_blaat, arg1, arg2}, _Context) ->
    observer1.

observer2({test_blaat, arg1, arg2}, _Context) ->
    observer2.

%% @doc Test z_notifier:observe, z_notifier:get_observers, z_notifier:detach
attach_detach_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    ?assertEqual([], z_notifier:get_observers(test_blaat, Context)),
    z_notifier:observe(test_blaat, {?MODULE, observer1}, 100, Context),
    ?assertEqual([{100, {?MODULE, observer1}, self()}],
                 z_notifier:get_observers(test_blaat, Context)),
    z_notifier:detach(test_blaat, Context),
    ?assertEqual([], z_notifier:get_observers(test_blaat, Context)).


%% @doc Test z_notifier:detach_all/2
detach_all_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    ?assertEqual([], z_notifier:get_observers(test_blaat, Context)),
    z_notifier:observe(test_blaat, {?MODULE, observer1}, 200, Context),
    z_notifier:observe(test_blaat, {?MODULE, observer2}, 100, Context),
    ?assertEqual([
            {100, {?MODULE, observer2}, self()},
            {200, {?MODULE, observer1}, self()}
        ], z_notifier:get_observers(test_blaat, Context)),
    z_notifier:detach_all(self(), Context),
    ?assertEqual([], z_notifier:get_observers(test_blaat, Context)).


%% @doc Test receiving a message using z_notifer:first
z_notifier_first_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    ?assertEqual(undefined, z_notifier:first({test_blaat, arg1, arg2}, Context)),

    z_notifier:observe(test_blaat, {?MODULE, observer1}, 100, Context),
    z_notifier:observe(test_blaat, {?MODULE, observer2}, 200, Context),
    ?assertEqual(observer1, z_notifier:first({test_blaat, arg1, arg2}, Context)),

    z_notifier:detach_all(self(), Context),
    ?assertEqual(undefined, z_notifier:first({test_blaat, arg1, arg2}, Context)).


%% @doc Test receiving a message using z_notifer:map
z_notifier_map_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    ?assertEqual([], z_notifier:map({test_blaat, arg1, arg2}, Context)),

    z_notifier:observe(test_blaat, {?MODULE, observer1}, 100, Context),
    z_notifier:observe(test_blaat, {?MODULE, observer2}, 200, Context),
    ?assertEqual([observer1, observer2], z_notifier:map({test_blaat, arg1, arg2}, Context)),

    z_notifier:detach_all(self(), Context),
    ?assertEqual([], z_notifier:map({test_blaat, arg1, arg2}, Context)).


%% @todo, asynchronous messages:
%% z_notifier:observe
%% z_notifier:observe1
