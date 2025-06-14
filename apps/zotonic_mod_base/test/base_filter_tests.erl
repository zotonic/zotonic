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

escapejson_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),

    <<>> = filter_escapejson:escapejson(undefined, C),
    <<"true">> = filter_escapejson:escapejson(true, C),
    <<"false">> = filter_escapejson:escapejson(false, C),
    <<"atom">> = filter_escapejson:escapejson(atom, C),
    <<"42">> = filter_escapejson:escapejson(42, C),

    <<>> = filter_escapejson:escapejson([], C),
    <<"test1234">> = filter_escapejson:escapejson(["test", "1234"], C),

    <<"\\\"">> = filter_escapejson:escapejson(<<$">>, C),
    <<"Feest">> = filter_escapejson:escapejson({trans,[{en,<<"Feest">>}]}, C),

    <<"2025-04-02T14:29:08Z">> = filter_escapejson:escapejson({{2025,4,2},{14,29,8}}, C),

    <<"This is a \\\"test\\\" string with a backslash \\\\ and new line \\n and tab \\t characters.">> =
        filter_escapejson:escapejson(<<"This is a \"test\" string with a backslash \\ and new line \n and tab \t characters.">>, C),

    ok.

is_a_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_context:new(zotonic_site_testsandbox),
    [1] = filter_is_a:is_a([1], person, Context),
    [] = filter_is_a:is_a([1], text, Context),
    [1,1] = filter_is_a:is_a([1,1,1], person, 2, Context),
    [1,1,1] = filter_is_a:is_a([1,1,1], person, 4, Context),
    [] = filter_is_not_a:is_not_a([1], person, Context),
    [1] = filter_is_not_a:is_not_a([1], text, Context),
    [-1, undefined, null] = filter_is_not_a:is_not_a([-1, undefined, 1, null], person, Context),
    true = filter_is_a:is_a(1, person, Context),
    true = filter_is_a:is_a(<<"1">>, person, Context),
    false = filter_is_a:is_a(<<"1">>, text, Context),
    ok.


merge_tag_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_context:new(zotonic_site_testsandbox),
    <<"The sum is 300">> = filter_merge_tags:merge_tags(<<"The sum is {{ 100 + 200 }}">>, #{}, Context),
    <<"Hello World.">> = filter_merge_tags:merge_tags(<<"Hello {{ a }}.">>, #{ <<"a">> => <<"World">> }, Context),
    <<"Hello &lt;&gt;.">> = filter_merge_tags:merge_tags(<<"Hello {{ a }}.">>, #{ <<"a">> => <<"<>">> }, Context),
    <<"Hello administrator.">> = filter_merge_tags:merge_tags(<<"Hello {{ name }}.">>, #{ <<"id">> => 1 }, Context),
    <<"Hello foo.">> = filter_merge_tags:merge_tags(<<"Hello {{ name }}.">>, #{ <<"id">> => 1, <<"name">> => <<"foo">> }, Context),
    ok.

replace_args_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_context:new(zotonic_site_testsandbox),
    ?assertEqual(<<"Your €300 donation"/utf8>>, unicode:characters_to_binary(filter_replace_args:replace_args(<<"Your €$1 donation"/utf8>>, ["300"], Context))),
    ?assertEqual(<<"Your €10 donation"/utf8>>, unicode:characters_to_binary(filter_replace_args:replace_args(<<"Your $2$1 donation"/utf8>>, ["10", "€"], Context))),
    ?assertEqual(<<"Your €5.95 donation"/utf8>>, unicode:characters_to_binary(filter_replace_args:replace_args(<<"Your $2$1 donation"/utf8>>, ["5.95", <<"€"/utf8>>], Context))),
    ok.
