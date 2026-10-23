%% @copyright 2026 Marc Worrell
-module(import_feed_schedule_tests).
-include_lib("eunit/include/eunit.hrl").

publisher_hints_test() ->
    Parse = fun(Hints) ->
        Xml = <<"<rss xmlns:sy='http://purl.org/rss/1.0/modules/syndication/'><channel>",
            Hints/binary, "</channel></rss>">>,
        {ok, Feed} = z_import_feed_parse:parse(Xml, <<"https://example.org/feed">>),
        maps:get(refresh_seconds, Feed)
    end,
    ?assertEqual(7200, Parse(<<"<ttl>120</ttl>">>)),
    ?assertEqual(43200, Parse(<<"<sy:updatePeriod>daily</sy:updatePeriod><sy:updateFrequency>2</sy:updateFrequency>">>)),
    ?assertEqual(43200, Parse(<<"<sy:updateFrequency>2</sy:updateFrequency>">>)),
    ?assertEqual(86400, Parse(<<"<sy:updatePeriod>daily</sy:updatePeriod>">>)),
    ?assertEqual(7200, Parse(<<"<ttl>120</ttl><sy:updatePeriod>hourly</sy:updatePeriod>">>)),
    ?assertEqual(0, Parse(<<"<ttl>-1</ttl><sy:updatePeriod>hourly</sy:updatePeriod><sy:updateFrequency>0</sy:updateFrequency>">>)),
    ?assertEqual(0, Parse(<<"<ttl>invalid</ttl><sy:updatePeriod xmlns:sy='urn:other'>daily</sy:updatePeriod>">>)),
    ?assertEqual(0, Parse(<<"<ttl>999999999999999999999</ttl>">>)),
    ?assertEqual(0, Parse(<<>>)).

retry_after_test() ->
    Now = {{2026, 10, 19}, {12, 0, 0}},
    Parse = fun(Value) -> z_import_feed_http:retry_after([{<<"Retry-After">>, Value}], Now) end,
    ?assertEqual(7200, Parse(<<"7200">>)),
    ?assertEqual(0, Parse(<<"0">>)),
    ?assertEqual(7200, Parse(<<"Mon, 19 Oct 2026 14:00:00 GMT">>)),
    ?assertEqual(0, Parse(<<"Mon, 19 Oct 2026 11:00:00 GMT">>)),
    lists:foreach(fun(Value) -> ?assertEqual(undefined, Parse(Value)) end,
        [<<"-1">>, <<"1.5">>, <<"bad date">>, <<"99999999999999">>]),
    ?assertEqual(undefined, z_import_feed_http:retry_after([], Now)),
    ?assertEqual(undefined, z_import_feed_http:retry_after(
        [{"retry-after", "1"}, {"Retry-After", "2"}], Now)).
