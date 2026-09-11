%% @hidden
-module(m_rsc_update_unicode_tests).

-include_lib("eunit/include/eunit.hrl").

unicode_slug_test_() ->
    [ ?_assertEqual(<<"caf", 233/utf8>>, m_rsc_update:to_slug([99, 97, 102, 233])),
      ?_assertEqual(<<16#4e2d/utf8>>, m_rsc_update:to_slug([16#4e2d])),
      ?_assertEqual(<<"hello-", 16#1f600/utf8>>, m_rsc_update:to_slug("hello " ++ [16#1f600])) ].

unicode_page_path_test_() ->
    [ ?_assertEqual(<<"/caf%C3%A9">>,
        m_rsc_update:normalize_page_path([47, 99, 97, 102, 233])),
      ?_assertEqual(<<"/%F0%9F%98%80">>,
        m_rsc_update:normalize_page_path([32, 47, 16#1f600, 47, 32])),
      ?_assertEqual(<<"/caf%C3%A9">>,
        m_rsc_update:normalize_page_path(<<"/caf", 233/utf8>>)) ].
