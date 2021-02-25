-module(z_language_tests).

-include_lib("eunit/include/eunit.hrl").
-include("zotonic.hrl").


-define(PROPERTIES_LANGUAGE_CODE, <<"hr">>).
-define(EXPECTED_PROPERTIES,
    [
        {language, <<"hr">>},
        {name, <<"Hrvatski"/utf8>>},
        {name_en, <<"Croatian"/utf8>>},
        {sublanguages, [
            {<<"hr-ba">>, [
                {language, <<"hr">>},
                {region, <<"BA">>},
                {name, <<"hrvatski - Bosna i Hercegovina"/utf8>>},
                {name_en, <<"Croatian - Bosnia and Herzegovina"/utf8>>}
            ]},
            {<<"hr-hr">>, [
                {language, <<"hr">>},
                {region, <<"HR">>},
                {name, <<"hrvatski - Hrvatska"/utf8>>},
                {name_en, <<"Croatian - Croatia"/utf8>>}
            ]}
        ]}
    ]).

%% z_language:english_name

get_english_name_test() ->
    test_get_english_name_1(),
    test_get_english_name_2().

test_get_english_name_1() ->
    Code = fr,
    Result = z_language:english_name(Code),
    Expected = <<"French">>,
    ?assertEqual(Expected, Result).

test_get_english_name_2() ->
    Code = <<"hr-hr">>,
    Result = z_language:english_name(Code),
    Expected = <<"Croatian - Croatia">>,
    ?assertEqual(Expected, Result).


%% z_language:get_properties

get_properties_test() ->
    test_get_properties_1(),
    test_get_properties_2(),
    test_get_properties_3(),
    test_get_properties_4(),
    test_get_properties_5(),
    test_get_properties_6(),
    test_get_properties_7(),
    test_get_properties_8().

test_get_properties_1() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    ?assertEqual(
        proplists:get_value(language, ?EXPECTED_PROPERTIES),
        maps:get(language, Result)
    ).

test_get_properties_2() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    ?assertEqual(
        proplists:get_value(direction, ?EXPECTED_PROPERTIES),
        maps:get(direction, Result, undefined)
    ).

test_get_properties_3() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    ?assertEqual(
        proplists:get_value(name, ?EXPECTED_PROPERTIES),
        maps:get(name, Result)
    ).

test_get_properties_4() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    ?assertEqual(
        proplists:get_value(name_en, ?EXPECTED_PROPERTIES),
        maps:get(name_en, Result)
    ).

test_get_properties_5() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    ?assertEqual(
        proplists:get_value(script, ?EXPECTED_PROPERTIES),
        maps:get(script, Result)
    ).

test_get_properties_6() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    ?assertEqual(
        length(proplists:get_value(sublanguages, ?EXPECTED_PROPERTIES)),
        maps:size(maps:get(sublanguages, Result, #{}))
    ).

test_get_properties_7() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    [{ExpectedCode, _ExpectedData}|_] = proplists:get_value(sublanguages, ?EXPECTED_PROPERTIES),
    SubLang = maps:get(sublanguages, Result),
    ?assertEqual(
        true,
        maps:is_key(z_convert:to_atom(ExpectedCode), SubLang)
    ).

test_get_properties_8() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    [{ExpectedCode, ExpectedData}|_] = proplists:get_value(sublanguages, ?EXPECTED_PROPERTIES),
    SubLang = maps:get(sublanguages, Result),
    io:format("~p~n~n~n", [ SubLang ]),
    ResultData = maps:get(z_convert:to_atom(ExpectedCode), SubLang),
    ?assertEqual(
        proplists:get_value(region, ExpectedData),
        maps:get(region, ResultData)
    ).


%% z_language:is_valid

is_valid_test() ->
    test_is_valid_1(),
    test_is_valid_2(),
    test_is_valid_3(),
    test_is_valid_4().

test_is_valid_1() ->
    Code = <<"fr-fr">>,
    Result = z_language:is_valid(Code),
    Expected = true,
    ?assertEqual(Expected, Result).

test_is_valid_2() ->
    Code = "es-419",
    Result = z_language:is_valid(Code),
    Expected = true,
    ?assertEqual(Expected, Result).

test_is_valid_3() ->
    Code = nl,
    Result = z_language:is_valid(Code),
    Expected = true,
    ?assertEqual(Expected, Result).

test_is_valid_4() ->
    Code = <<"xxx">>,
    Result = z_language:is_valid(Code),
    Expected = false,
    ?assertEqual(Expected, Result).


%% z_language:is_rtl

is_rtl_test() ->
    test_is_rtl_1(),
    test_is_rtl_2(),
    test_is_rtl_3().

test_is_rtl_1() ->
    Code = "en",
    Result = z_language:is_rtl(Code),
    Expected = false,
    ?assertEqual(Expected, Result).

test_is_rtl_2() ->
    Code = <<"ar">>,
    Result = z_language:is_rtl(Code),
    Expected = true,
    ?assertEqual(Expected, Result).

test_is_rtl_3() ->
    Code = <<"ar-ae">>,
    Result = z_language:is_rtl(Code),
    Expected = true,
    ?assertEqual(Expected, Result).
