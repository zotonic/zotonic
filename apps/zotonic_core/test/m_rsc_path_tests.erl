%% @hidden
-module(m_rsc_path_tests).

-include_lib("eunit/include/eunit.hrl").

invalid_page_path_test_() ->
    Paths = [ <<"/foo", C, "bar">> || C <- lists:seq(0, 31) ]
        ++ [<<"/", 255>>, <<"/", 16#e2, 16#82>>],
    % An undefined context ensures that invalid paths never reach the database.
    [ ?_assertEqual(
        {error, {illegal_page_path, Path, unicode}},
        m_rsc:page_path_to_id(Path, undefined))
      || Path <- Paths ].
