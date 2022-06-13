%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(z_datetime_tests).

-include_lib("eunit/include/eunit.hrl").

timesince_simple_test() ->
    C = z_context:new(zotonic_site_testsandbox, en),
    ?assertEqual(<<"10 seconds ago">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,45}}, C))),
    ?assertEqual(<<"in 10 seconds">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,25}}, C))),
    ?assertEqual(<<"10 hours ago">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{20,13,45}}, C))),
    ?assertEqual(<<"now">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,35}}, C))),
    ?assertEqual(<<"moments ago">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,37}}, C))).

timesince_no_time_indication_test() ->
    C = z_context:new(zotonic_site_testsandbox, en),
    ?assertEqual(<<"10 seconds">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,45}}, [], C))),
    ?assertEqual(<<"10 seconds">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,25}}, [], C))),
    ?assertEqual(<<"10 hours">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{20,13,45}}, [], C))),
    ?assertEqual(<<"">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,35}}, [], C))),
    ?assertEqual(<<"moments">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,37}}, [], C))).

timesince_user_specified_texts_test() ->
    C = z_context:new(zotonic_site_testsandbox, en),
    ?assertEqual(<<"10 seconds foo">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,45}}, "foo,bar,baz", C))),
    ?assertEqual(<<"baz 10 seconds">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,25}}, "foo,   bar,    baz", C))),
    ?assertEqual(<<"10 hours">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{20,13,45}}, " ,bar,baz", C))),
    ?assertEqual(<<"bar">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,35}}, " ,bar", C))),
    ?assertEqual(<<"moments foo">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,37}}, "foo,bar,baz", C))).

time_add_month_test() ->
    ?assertEqual({{2021,12,1},{0,0,0}}, z_datetime:next_month({{2022,1,1},{0,0,0}}, -1)),
    ?assertEqual({{2022,2,1},{0,0,0}},  z_datetime:next_month({{2022,1,1},{0,0,0}}, 1)),
    ?assertEqual({{2022,2,28},{0,0,0}}, z_datetime:next_month({{2022,1,31},{0,0,0}}, 1)),
    ?assertEqual({{2022,3,31},{0,0,0}}, z_datetime:next_month({{2022,1,31},{0,0,0}}, 2)),
    ?assertEqual({{2022,4,30},{0,0,0}}, z_datetime:next_month({{2022,1,31},{0,0,0}}, 3)),
    ?assertEqual({{2023,1,31},{0,0,0}}, z_datetime:next_month({{2022,1,31},{0,0,0}}, 12)),
    ?assertEqual({{2023,2,28},{0,0,0}}, z_datetime:next_month({{2022,1,31},{0,0,0}}, 13)),
    ?assertEqual({{2024,2,29},{0,0,0}}, z_datetime:next_month({{2022,1,31},{0,0,0}}, 25)).
