%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(z_string_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").


contains_test() ->
    ?assert(z_string:contains("", "strange case")),
    ?assert(z_string:contains("", "")),
    ?assert(z_string:contains("a", "a")),
    ?assert(z_string:contains("is", "This is text.")),
    ?assert(z_string:contains("This", "This is text.")),
    ?assert(z_string:contains("t.", "This is text.")),
    ?assertNot(z_string:contains("not", "This is text.")),
    ?assertNot(z_string:contains("n", "")),
    ?assertNot(z_string:contains("n", "b")),
    ok.

starts_with_test() ->
    ?assert(z_string:starts_with("", "This is text.")),
    ?assert(z_string:starts_with("", "")),
    ?assert(z_string:starts_with("T", "This is text.")),
    ?assertNot(z_string:starts_with("t", "This is text.")),
    ?assert(z_string:starts_with("This", "This is text.")),
    ?assertNot(z_string:starts_with("Bla", "This is text.")),
    ?assert(z_string:starts_with(["This ", "is"], "This is text.")),
    ?assert(z_string:starts_with(["This ", <<"is">>], "This is text.")),
    ?assertNot(z_string:starts_with(["This ", <<"is not">>], "This is text.")),
    ok.
    
ends_with_test() ->
    ?assert(z_string:ends_with("", "This is text.")),
    ?assert(z_string:ends_with("", "")),
    ?assert(z_string:ends_with(".", "This is text.")),
    ?assertNot(z_string:ends_with("T", "This is text.")),
    ?assert(z_string:ends_with("ext.", "This is text.")),
    ?assert(z_string:ends_with(["is ", "text."], "This is text.")),
    ?assert(z_string:ends_with(["is ", <<"text.">>], "This is text.")),
    ?assertNot(z_string:ends_with(["is ", <<"jpeg.">>], "This is text.")),
    ok.

valid_utf8_test_() ->
    [
        ?_assert(v_utf8(<<>>)),
        ?_assert(v_utf8(<<127>>)),
        ?_assert(v_utf8(<<2#11001111, 2#10000000>>)),
        ?_assert(v_utf8(<<2#11011111, 2#10111111>>)),

        ?_assert(v_utf8(<<2#11101000, 2#10000000, 2#10000000>>)),
        ?_assert(v_utf8(<<2#11101111, 2#10111111, 2#10111101>>)),

        ?_assert(v_utf8(<<2#11110100, 2#10000000, 2#10000000, 2#10000000>>)),
        ?_assert(v_utf8(<<2#11110000, 2#10111111, 2#10111111, 2#10111111>>)),

        ?_assertNot(v_utf8(<<128>>)),

        ?_assertNot(v_utf8(<<2#11100000, 2#10000000>>)),
        ?_assertNot(v_utf8(<<2#11000000, 2#11000000>>)),

        ?_assertNot(v_utf8(<<2#11110000, 2#10000000, 2#10000000>>)),
        ?_assertNot(v_utf8(<<2#11100000, 2#11000000, 2#10000000>>)),
        ?_assertNot(v_utf8(<<2#11100000, 2#10000000, 2#11000000>>)),

        ?_assertNot(v_utf8(<<2#11111000, 2#10000000, 2#10000000, 2#10000000>>))
    ].

v_utf8(Bin) ->
    z_string:sanitize_utf8(Bin) =:= Bin.
