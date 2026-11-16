%% @copyright 2026 Marc Worrell
%% @doc Classify delivery policy failures without judging recipient validity.
-module(z_email_delivery_status).
-moduledoc("""
Recognize sender blocklisting and spam-policy failures in SMTP diagnostics.
These failures describe a message or sender, not a nonexistent recipient mailbox.
Matching is case-insensitive and folds whitespace in multiline SMTP responses.
Explicit complaints, unsubscribes and recipient suppression take precedence.
An unrecognized diagnostic remains subject to the existing recipient checks.
""").
-export([policy_failure/1]).

-spec policy_failure(Status) -> Classification when
    Status :: term(),
    Classification :: rbl | spam | gmail | false.
policy_failure({error, Status}) ->
    policy_failure(Status);
policy_failure({bounce, Status}) ->
    policy_failure(Status);
policy_failure(Status) when is_binary(Status) ->
    Text = re:replace(z_string:to_lower(Status), <<"\\s+">>, <<" ">>, [global, {return, binary}]),
    case contains_any(Text, [
        <<"not delivering to a user who marked your messages as spam">>,
        <<"not delivering to previously bounced address">>,
        <<"unsubscribed">>,
        <<"spam complaint">>,
        <<"recipient blocked by zotonic module">>,
        <<"unknown user">>, <<"user unknown">>, <<"no such user">>,
        <<"does not exist">>, <<"not a known user">>
    ]) of
        true -> false;
        false -> policy_failure_text(Text)
    end;
policy_failure(_) ->
    false.

policy_failure_text(Text) ->
    Patterns = [
        {gmail, [
            <<"https://support.google.com/mail/?p=unsolicitedmessageerror">>,
            <<"to reduce the amount of spam sent to gmail">>
        ]},
        {rbl, [
            <<"is in an rbl">>,
            <<"an rbl has blocked the sender">>,
            <<"blocked using bl.spamcop.net">>,
            <<"blocked using zen.spamhaus.org">>,
            <<"get removed from dnsbls">>,
            <<"sender ip address is blacklisted">>
        ]},
        {spam, [
            <<"mail appeared to be spam or forged">>,
            <<"message appeared to be unwanted by recipient">>
        ]}
    ],
    match_policy(Patterns, Text).

match_policy([], _Text) ->
    false;
match_policy([{Class, Patterns} | Rest], Text) ->
    case contains_any(Text, Patterns) of
        true -> Class;
        false -> match_policy(Rest, Text)
    end.

contains_any(Text, Patterns) ->
    lists:any(fun(Pattern) -> binary:match(Text, Pattern) =/= nomatch end, Patterns).
