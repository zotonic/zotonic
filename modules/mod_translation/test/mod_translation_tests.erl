-module(mod_translation_tests).

-include_lib("eunit/include/eunit.hrl").
-include("zotonic.hrl").


url_strip_language_test() ->
    test_url_strip_language1(),
    test_url_strip_language2(),
    test_url_strip_language3().

test_url_strip_language1() ->
    Location = <<"/nl-nl/admin/translation">>,
    Result = mod_translation:url_strip_language(Location),
    Expected = <<"/admin/translation">>,
    ?assertEqual(Expected, Result).

test_url_strip_language2() ->
    Location = <<"/en/admin/translation">>,
    Result = mod_translation:url_strip_language(Location),
    Expected = <<"/admin/translation">>,
    ?assertEqual(Expected, Result).

test_url_strip_language3() ->
    Location = "/nl-nl/admin/translation",
    Result = mod_translation:url_strip_language(Location),
    Expected = "/admin/translation",
    ?assertEqual(Expected, Result).

filter_is_rtl_test() ->
    Context = z_context:new(testsandbox),
    test_filter_is_rtl1(Context),
    test_filter_is_rtl2(Context),
    test_filter_is_rtl3(Context).

test_filter_is_rtl1(Context) ->
    Code = "en",
    Result = filter_is_rtl:is_rtl(Code, Context),
    Expected = false,
    ?assertEqual(Expected, Result).

test_filter_is_rtl2(Context) ->
    Code = <<"ar">>,
    Result = filter_is_rtl:is_rtl(Code, Context),
    Expected = true,
    ?assertEqual(Expected, Result).

test_filter_is_rtl3(Context) ->
    Code = <<"ar-ae">>,
    Result = filter_is_rtl:is_rtl(Code, Context),
    Expected = true,
    ?assertEqual(Expected, Result).
