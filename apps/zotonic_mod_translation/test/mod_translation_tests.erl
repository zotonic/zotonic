-module(mod_translation_tests).
-moduledoc("
EUnit tests for translation helper behavior.
").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


url_strip_language_test() ->
    test_url_strip_language1(),
    test_url_strip_language2().

test_url_strip_language1() ->
    Location = <<"/zh-hant/admin/translation">>,
    Result = mod_translation:url_strip_language(Location),
    Expected = <<"/admin/translation">>,
    ?assertEqual(Expected, Result).

test_url_strip_language2() ->
    Location = <<"/en/admin/translation">>,
    Result = mod_translation:url_strip_language(Location),
    Expected = <<"/admin/translation">>,
    ?assertEqual(Expected, Result).

% TODO: translations are not loaded for nl, make this work
% filter_translate_test() ->
%     Context = z:c(testsandbox),
%     ?assertEqual(<<"Frans">>, filter_translate:translate(<<"French">>, nl, Context)),
%     ?assertEqual(<<"Frans">>, filter_translate:translate(#trans{ tr = [{en, <<"French">>}] }, nl, Context)).
