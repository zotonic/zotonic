%% @hidden
-module(m_rsc_path_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

invalid_page_path_test_() ->
    Paths = [ <<"/foo", C, "bar">> || C <- lists:seq(0, 31) ]
        ++ [<<"/", 255>>, <<"/", 16#e2, 16#82>>],
    % A context without a site ensures that invalid paths never reach the database.
    [ ?_assertEqual(
        {error, {illegal_page_path, Path, unicode}},
        m_rsc:page_path_to_id(Path, #context{}))
      || Path <- Paths ].

unicode_page_path_test_() ->
    % Long paths exercise conversion and normalization without querying a database.
    Paths = [lists:duplicate(100, 16#e9), lists:duplicate(100, 16#1f600)],
    [ ?_assertEqual(
        {error, {illegal_page_path, unicode:characters_to_binary([$/ | Path]), length}},
        m_rsc:page_path_to_id(Path, #context{}))
      || Path <- Paths ].

invalid_unicode_list_test() ->
    Path = [$/, 16#d800],
    ?assertEqual(
        {error, {illegal_page_path, Path, unicode}},
        m_rsc:page_path_to_id(Path, #context{})).

unicode_model_path_test() ->
    Segment = lists:duplicate(100, 16#1f600),
    Path = [<<"parent">>, Segment],
    ?assertEqual(
        {error, {illegal_page_path,
            unicode:characters_to_binary(["/parent/", Segment]), length}},
        m_rsc:m_get([<<"-">>, <<"lookup">>, <<"page_path">> | Path], undefined, #context{})).

unicode_uri_test_() ->
    % These are not resource URIs, so no site or database is needed.
    [ ?_assertEqual(undefined, m_rsc:uri_lookup(Path, #context{}))
      || Path <- [[99, 97, 102, 233], [16#1f600], [16#d800]] ].
