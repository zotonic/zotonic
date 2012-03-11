%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(z_utils_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").


url_encode_decode_test() ->
    Url = "index.html?x=y&z=1",
    ?assertEqual(Url, z_utils:url_decode(z_utils:url_encode(Url))).


url_encode_test() ->
    ?assertEqual("foo+bar", z_utils:url_encode("foo bar")),
    ?assertEqual("foo%26bar", z_utils:url_encode("foo&bar")).

url_decode_test() ->
    ?assertEqual("foo&bar", z_utils:url_decode("foo%26bar")).


percent_encode_test() ->
    ?assertEqual("foo%20bar", z_utils:percent_encode("foo bar")),
    ?assertEqual("foo%26bar", z_utils:percent_encode("foo&bar")).

