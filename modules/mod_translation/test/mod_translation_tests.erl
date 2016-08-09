-module(mod_translation_tests).
-mod_depends([mod_translation]).
-include_lib("eunit/include/eunit.hrl").

-export([run_tests/1]).


run_tests(Context) ->
    test_url_strip_language(),
    test_filter_is_rtl(Context).


test_url_strip_language() ->
    test_url_strip_language1(),
    test_url_strip_language2(),
    test_url_strip_language3().

test_url_strip_language1() ->
    Location = <<"/nl-nl/admin/translation">>,
    Result = mod_translation:url_strip_language(Location),
    Expected = <<"/admin/translation">>,
    check_result("test_url_strip_language1", Expected, Result).

test_url_strip_language2() ->
    Location = <<"/en/admin/translation">>,
    Result = mod_translation:url_strip_language(Location),
    Expected = <<"/admin/translation">>,
    check_result("test_url_strip_language2", Expected, Result).

test_url_strip_language3() ->
    Location = "/nl-nl/admin/translation",
    Result = mod_translation:url_strip_language(Location),
    Expected = "/admin/translation",
    check_result("test_url_strip_language3", Expected, Result).


test_filter_is_rtl(Context) ->
    test_filter_is_rtl1(Context),
    test_filter_is_rtl2(Context),
    test_filter_is_rtl3(Context).

test_filter_is_rtl1(Context) ->
    Code = "en",
    Result = filter_is_rtl:is_rtl(Code, Context),
    Expected = false,
    check_result("test_filter_is_rtl1", Expected, Result).

test_filter_is_rtl2(Context) ->
    Code = <<"ar">>,
    Result = filter_is_rtl:is_rtl(Code, Context),
    Expected = true,
    check_result("test_filter_is_rtl2", Expected, Result).

test_filter_is_rtl3(Context) ->
    Code = <<"ar-ae">>,
    Result = filter_is_rtl:is_rtl(Code, Context),
    Expected = true,
    check_result("test_filter_is_rtl3", Expected, Result).


check_result(Name, Expected, Actual) ->
    case Expected =:= Actual of
        true -> lager:info("~p: ok", [Name]);
        false -> lager:error("~p error: expected ~s but got ~p", [Name, Expected, Actual])
    end.
