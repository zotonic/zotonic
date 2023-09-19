%% @hidden

-module(base_filter_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

-define(fequal(A, B),  true = (abs(A - B) < 0.001)).

is_site_url_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),

    true = filter_is_site_url:is_site_url("#foo", C),
    true = filter_is_site_url:is_site_url("/path", C),
    true = filter_is_site_url:is_site_url("https://localhost/", C),
    true = filter_is_site_url:is_site_url("//sandbox.test/", C),

    false = filter_is_site_url:is_site_url("https://example.com/", C),
    false = filter_is_site_url:is_site_url(<<"https://example.com/">>, C),

    false = filter_is_site_url:is_site_url(<<"script:localhost">>, C),

    ok.

round_significant_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),
    1260 = filter_round_significant:round_significant(1256, 3, C),
    1300 = filter_round_significant:round_significant(1256, 2, C),
    1000 = filter_round_significant:round_significant(1256, 1, C),

    ?fequal(1235.0, filter_round_significant:round_significant(1234.56, 4, C)),
    ?fequal(1234.6, filter_round_significant:round_significant(1234.56, 5, C)),
    ?fequal(1200.0, filter_round_significant:round_significant(1234.56, 2, C)),

    ok.

truncatechars_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),
    <<"abcxyz">> = filter_truncatechars:truncatechars(<<"abcd">>, 3, <<"xyz">>, C),
    <<"abcxyz">> = filter_truncatechars:truncatechars("abcd", 3, "xyz", C),
    <<"abc…"/utf8>> = filter_truncatechars:truncatechars(<<"abcd">>, 3, C),
    <<"abcd"/utf8>> = filter_truncatechars:truncatechars(<<"abcd">>, 4, C),
    <<"abcd"/utf8>> = filter_truncatechars:truncatechars(<<"abcd">>, 5, C),
    <<"a&amp;c…"/utf8>> = filter_truncatechars:truncatechars(<<"a&amp;cd">>, 3, C),
    <<"a&#39;c…"/utf8>> = filter_truncatechars:truncatechars(<<"a&#39;cd">>, 3, C),
    ok.

