-module(mailinglist_delivery_detail_tests).
-include_lib("eunit/include/eunit.hrl").

legacy_details_test() ->
    lists:foreach(fun(Reason) ->
        Text = <<"550 5.7.1 <recipient@example.org>: rejected \"spam\"\nSee https://example.org/info">>,
        Legacy = iolist_to_binary(io_lib:format("~p", [{Reason, Text}])),
        ?assertEqual(Text, format(Legacy))
    end, [bounce, retry, smtphost, error, illegal_address, sender_disabled]).

plain_and_malformed_test() ->
    lists:foreach(fun(Text) -> ?assertEqual(Text, format(Text)) end, [
        undefined,
        <<"Language mismatch">>,
        <<"550 5.1.1 User unknown">>,
        <<"{bounce,<<\"unterminated>>}">>,
        <<"{bounce,<<\"text\",unknown_atom_from_smtp>>}">>,
        <<"{unknown_wrapper,<<\"text\">>}">>
    ]).

html_remains_text_test() ->
    %% The template must escape this output, just like any raw SMTP diagnostic.
    ?assertEqual(<<"<script>alert(1)</script>">>,
        format(<<"{bounce,<<\"<script>alert(1)</script>\">>}">>)).

format(Text) ->
    filter_mailinglist_delivery_detail:mailinglist_delivery_detail(Text, undefined).


nested_and_missing_details_test() ->
    lists:foreach(fun({Detail, Expected}) ->
        ?assertEqual(Expected, z_mailinglist_delivery_detail:format(Detail)),
        Legacy = iolist_to_binary(io_lib:format("~p", [Detail])),
        ?assertEqual(Expected, format(Legacy))
    end, [
        {{smtphost, {error, timeout}}, <<"The connection to the mail server timed out.">>},
        {{retry, {error, closed}}, <<"The mail server closed the connection.">>},
        {{error, {error, nxdomain}}, <<"The mail server domain could not be found.">>},
        {{error, undefined}, <<"Delivery failed without further details.">>},
        {{smtphost, undefined}, <<"Delivery failed without further details.">>},
        {{bounce, undefined}, <<"Delivery was rejected.">>}
    ]).
