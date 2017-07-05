-module(mod_translation_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


url_strip_language_test() ->
    test_url_strip_language1(),
    test_url_strip_language2().

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
