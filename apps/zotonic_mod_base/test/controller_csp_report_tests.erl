-module(controller_csp_report_tests).

-include_lib("eunit/include/eunit.hrl").

extension_report_test_() ->
    [
        ?_assert(controller_csp_report:is_extension_report(#{ Key => Url }))
        || Key <- [ <<"blockedURL">>, <<"sourceFile">> ],
           Scheme <- [
               <<"safari-extension:">>, <<"safari-web-extension:">>,
               <<"chrome-extension:">>, <<"moz-extension:">>,
               <<"ms-browser-extension:">>
           ],
           Url <- [ Scheme, <<Scheme/binary, "//extension-id/script.js">> ]
    ].

site_report_test_() ->
    [
        ?_assertNot(controller_csp_report:is_extension_report(Report))
        || Report <- [
            #{},
            #{ <<"blockedURL">> => <<"inline">>, <<"sourceFile">> => <<"https://example.com/app.js">> },
            #{ <<"blockedURL">> => <<"eval">> },
            #{ <<"blockedURL">> => <<"https://example.com/chrome-extension://script.js">> },
            #{ <<"blockedURL">> => <<"data:text/javascript,alert(1)">> },
            #{ <<"blockedURL">> => <<"blob:https://example.com/id">> },
            #{ <<"blockedURL">> => null, <<"sourceFile">> => [] },
            #{ <<"blockedURL">> => #{}, <<"sourceFile">> => 123 },
            #{ <<"sample">> => <<"chrome-extension://extension-id/script.js">> }
        ]
    ].

extension_source_test() ->
    ?assert(controller_csp_report:is_extension_report(#{
        <<"blockedURL">> => <<"inline">>,
        <<"sourceFile">> => <<"safari-web-extension://extension-id/script.js">>
    })),
    % Filtering happens before context access or notification delivery.
    ?assertEqual(ok, controller_csp_report:handle_report(undefined, #{
        <<"type">> => <<"csp-violation">>,
        <<"url">> => <<"https://example.com/">>,
        <<"body">> => #{ <<"blockedURL">> => <<"chrome-extension://extension-id/">> }
    }, undefined)).
