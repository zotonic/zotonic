-module(z_language_tests).

-include_lib("eunit/include/eunit.hrl").
-include("zotonic.hrl").


-define(PROPERTIES_LANGUAGE_CODE, <<"zh-hant">>).
-define(EXPECTED_PROPERTIES,
    [
        {language, <<"zh-hant">>},
        {script, <<"Hant">>},
        {name, <<"中國傳統的腳本"/utf8>>},
        {name_en, <<"Chinese (Traditional)"/utf8>>},
        {sublanguages, [
            {<<"zh-hant-hk">>, [
                {language, <<"zh-hant">>},
                {region, <<"HK">>},
                {script, <<"Hant">>},
                {name, <<"香港中國傳統腳本"/utf8>>},
                {name_en, <<"Chinese - Hong Kong (Traditional)"/utf8>>}
            ]},
            {<<"zh-hant-mo">>, [
                {language, <<"zh-hant">>},
                {region, <<"MO">>},
                {script, <<"Hant">>},
                {name, <<"澳門中國人在傳統的腳本"/utf8>>},
                {name_en, <<"Chinese - Macau (Traditional)"/utf8>>}
            ]},
            {<<"zh-hant-tw">>, [
                {language, <<"zh-hant">>},
                {region, <<"TW">>},
                {script, <<"Hant">>},
                {name, <<"台灣中國傳統腳本"/utf8>>},
                {name_en, <<"Chinese - Taiwan (Traditional)"/utf8>>}
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
    Code = <<"zh-hant-hk">>,
    Result = z_language:english_name(Code),
    Expected = <<"Chinese - Hong Kong (Traditional)">>,
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
        proplists:get_value(language, Result)
    ).

test_get_properties_2() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    ?assertEqual(
        proplists:get_value(direction, ?EXPECTED_PROPERTIES),
        proplists:get_value(direction, Result)
    ).

test_get_properties_3() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    ?assertEqual(
        proplists:get_value(name, ?EXPECTED_PROPERTIES),
        proplists:get_value(name, Result)
    ).

test_get_properties_4() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    ?assertEqual(
        proplists:get_value(name_en, ?EXPECTED_PROPERTIES),
        proplists:get_value(name_en, Result)
    ).

test_get_properties_5() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    ?assertEqual(
        proplists:get_value(script, ?EXPECTED_PROPERTIES),
        proplists:get_value(script, Result)
    ).

test_get_properties_6() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    ?assertEqual(
        length(proplists:get_value(sublanguages, ?EXPECTED_PROPERTIES)),
        length(proplists:get_value(sublanguages, Result))
    ).

test_get_properties_7() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    [{ExpectedCode, _ExpectedData}|_] = proplists:get_value(sublanguages, ?EXPECTED_PROPERTIES),
    [{ResultCode, _ResultData}|_] = proplists:get_value(sublanguages, Result),
    ?assertEqual(
        ExpectedCode,
        ResultCode
    ).

test_get_properties_8() ->
    Code = ?PROPERTIES_LANGUAGE_CODE,
    Result = z_language:properties(Code),
    [{_ExpectedCode, ExpectedData}|_] = proplists:get_value(sublanguages, ?EXPECTED_PROPERTIES),
    [{_ResultCode, ResultData}|_] = proplists:get_value(sublanguages, Result),
    ?assertEqual(
        proplists:get_value(region, ExpectedData),
        proplists:get_value(region, ResultData)
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
