-module(z_datetime_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

timesince_simple_test() ->
    ?assertEqual("10 seconds ago", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,45}}, [])),
    ?assertEqual("in 10 seconds", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,25}}, [])),
    ?assertEqual("10 hours ago", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{20,13,45}}, [])),
    ?assertEqual("now", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,35}}, [])),
    ?assertEqual("moments ago", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,37}}, [])).

timesince_no_time_indication_test() ->
    ?assertEqual("10 seconds", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,45}}, "", [])),
    ?assertEqual("10 seconds", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,25}}, "", [])),
    ?assertEqual("10 hours", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{20,13,45}}, "", [])),
    ?assertEqual("", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,35}}, "", [])),
    ?assertEqual("moments", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,37}}, "", [])).

timesince_user_specified_texts_test() ->
    ?assertEqual("10 seconds foo", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,45}}, "foo,bar,baz", [])),
    ?assertEqual("baz 10 seconds", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,25}}, "foo,   bar,    baz", [])),
    ?assertEqual("10 hours", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{20,13,45}}, " ,bar,baz", [])),
    ?assertEqual("bar", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,35}}, " ,bar", [])),
    ?assertEqual("moments foo", z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,37}}, "foo,bar,baz", [])).
