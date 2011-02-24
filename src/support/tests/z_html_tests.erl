%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(z_html_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").


link_elements_test() ->
    ?assertEqual([[{"rel", "foo"}]], z_html:scrape_link_elements("<p>This is text.<link rel=\"foo\" /></p>")),
    ?assertEqual([[{"rel", "Foo"}]], z_html:scrape_link_elements("<p>This is text.<LINK REL=\"Foo\" /></p>")),
    ok.
