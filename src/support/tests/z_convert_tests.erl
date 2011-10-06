%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(z_convert_tests).

-include_lib("zotonic.hrl").

-include_lib("eunit/include/eunit.hrl").


-define(assertDatetime(A, B), ?assertEqual(A, z_convert:to_datetime(B))).

convert_date_test() ->
    ?assertEqual({2010,12,12}, z_convert:to_date("2010-12-12")),
    ?assertEqual({2010,12,12}, z_convert:to_date("2010/12/12")),
    ok.

convert_time_test() ->
    ?assertEqual({1,23,0}, z_convert:to_time("01:23:00")),
    ?assertEqual({1,23,0}, z_convert:to_time("01:23")),
    ?assertEqual({1,23,59}, z_convert:to_time("01:23:59")),
    ok.

convert_datetime_test() ->

    %% generic format
    ?assertDatetime({{2010,1,1},{18,23,50}}, "2010-01-01 18:23:50"),
    ?assertDatetime({{2010,1,1},{18,23,0}}, "2010-01-01 18:23"),
    ?assertDatetime({{2010,1,1},{0,0,0}}, "2010-01-01"),

    %% xsd:datetime
    ?assertDatetime({{2010,1,1},{18,29,39}}, "2010-01-01T18:29:39"),
    ?assertDatetime(calendar:universal_time_to_local_time({{2010,1,1},{18,29,39}}), "2010-01-01T18:29:39+00:00"),
    ?assertDatetime(calendar:universal_time_to_local_time({{2010,1,1},{18,29,39}}), "2010-01-01T18:29:39Z"),
    ?assertDatetime(calendar:universal_time_to_local_time({{2010,1,1},{17,29,39}}), "2010-01-01T18:29:39+01:00"),
    ?assertDatetime(calendar:universal_time_to_local_time({{2010,1,1},{20,29,39}}), "2010-01-01T18:29:39-02:00"),

    ?assertDatetime(calendar:universal_time_to_local_time({{2011,10,6},{14,44,0}}), "2011-10-06T16:44:00+0200"),
    ok.

datetime_to_iso_test() ->
    ?assertEqual("2010-09-02T10:11:56Z", z_convert:to_isotime(calendar:universal_time_to_local_time({{2010,9,2},{10,11,56}}))),
    ?assertEqual("2010-09-02T01:01:01Z", z_convert:to_isotime(calendar:universal_time_to_local_time({{2010,9,2},{1,1,1}}))),
    ok.
