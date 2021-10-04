%% @hidden

-module(base_filter_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

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
