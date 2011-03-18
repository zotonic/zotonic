%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(z_datetime_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

timesince_simple_test() ->
    C = z_context:new(testsandbox, en),
    ?assertEqual(<<"10 seconds ago">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,45}}, C))),
    ?assertEqual(<<"in 10 seconds">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,25}}, C))),
    ?assertEqual(<<"10 hours ago">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{20,13,45}}, C))),
    ?assertEqual(<<"now">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,35}}, C))),
    ?assertEqual(<<"moments ago">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,37}}, C))).

timesince_no_time_indication_test() ->
    C = z_context:new(testsandbox, en),
    ?assertEqual(<<"10 seconds">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,45}}, [], C))),
    ?assertEqual(<<"10 seconds">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,25}}, [], C))),
    ?assertEqual(<<"10 hours">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{20,13,45}}, [], C))),
    ?assertEqual(<<"">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,35}}, [], C))),
    ?assertEqual(<<"moments">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,37}}, [], C))).

timesince_user_specified_texts_test() ->
    C = z_context:new(testsandbox, en),
    ?assertEqual(<<"10 seconds foo">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,45}}, "foo,bar,baz", C))),
    ?assertEqual(<<"baz 10 seconds">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,25}}, "foo,   bar,    baz", C))),
    ?assertEqual(<<"10 hours">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{20,13,45}}, " ,bar,baz", C))),
    ?assertEqual(<<"bar">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,35}}, " ,bar", C))),
    ?assertEqual(<<"moments foo">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,37}}, "foo,bar,baz", C))).
