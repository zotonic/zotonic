-module(import_feed_http_tests).
-include_lib("eunit/include/eunit.hrl").

conditional_fetch_test() ->
    meck:new(z_fetch, [no_link]),
    try
        Url = <<"https://example.test/feed">>,
        Settings = #{url => Url, http_url => Url, settings_version => <<"v1">>,
            http_settings_version => <<"v1">>, http_etag => <<"\"abc\"">>,
            http_last_modified => <<"Wed, 01 Jan 2020 00:00:00 GMT">>},
        meck:expect(z_fetch, fetch, fun(U, Options, _) ->
            self() ! {headers, proplists:get_value(headers, Options)},
            {error, {304, U, [], 0, <<>>}}
        end),
        ?assertMatch({not_modified, {Url, []}}, z_import_feed_http:get_feed(Settings, undefined)),
        receive {headers, H} -> ?assertEqual(2, length(H)) end,
        lists:foreach(fun(S) ->
            ?assertEqual({error, unexpected_not_modified}, z_import_feed_http:get_feed(S, undefined)),
            receive {headers, []} -> ok after 1000 -> error(conditional_on_forced_fetch) end
        end, [Settings#{settings_version => <<"v2">>}, Settings#{refetch_deleted_once => true},
            maps:without([http_etag, http_last_modified], Settings)]),
        ?assertEqual({error, unexpected_not_modified}, z_import_feed_http:get_feed(
            Settings#{http_url => <<"https://other.test/feed">>}, undefined)),
        receive {headers, _} -> ok end,
        meck:expect(z_fetch, fetch, fun(U, _, _) -> {ok, {U, [], 3, <<"new">>}} end),
        ?assertMatch({ok, {Url, [], 3, <<"new">>}}, z_import_feed_http:get_feed(Settings, undefined))
    after meck:unload(z_fetch) end.

validator_headers_test() ->
    ?assertEqual(#{http_etag => <<"W/\"abc\"">>, http_last_modified => <<"date">>},
        z_import_feed_http:validators([{"ETag", "W/\"abc\""}, {<<"Last-Modified">>, <<"date">>},
            {"authorization", "secret"}])),
    ?assertEqual(#{http_etag => undefined},
        z_import_feed_http:validators([{"etag", "bad\r\ninjected: value"}])).
