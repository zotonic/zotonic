%% @copyright 2026 Marc Worrell
%% @doc Sender-policy failures must remain delivery failures without harming addresses.
-module(email_policy_failure_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([observers/0]).

%% Synthetic addresses/hosts replace the identifying parts of the reported
%% diagnostics. Keep the wording, whitespace and blocklist names intact.
examples() ->
    [
        {rbl, <<"550 \"JunkMail rejected - outbound.example.org\n[192.0.2.1]:62234 is in an RBL: \"">>},
        {rbl, <<"554 5.7.1 5.7.1 Service unavailable; Client host [192.0.2.1] blocked using bl.spamcop.net; Blocked - see https://www.spamcop.net/bl.shtml?192.0.2.1">>},
        {rbl, <<"550 5.7.1 5.7.1 <alice@example.org>: Recipient address rejected: Mail appeared to be SPAM or forged. Ask your Mail/DNS-Administrator to correct HELO and DNS MX settings or to get removed from DNSBLs; please relay via your ISP (lists.example.org)">>},
        {spam, <<"554 5.7.1 5.7.1 Message appeared to be unwanted by recipient. See https://postmaster.routing.net">>},
        {rbl, <<"554 5.7.1 5.7.1 Service unavailable; Client host [192.0.2.2] blocked using zen.spamhaus.org">>},
        {rbl, <<"550 5.7.1 5.7.1 <bob@example.org>: Recipient address rejected: Mail appeared to be SPAM or forged. Ask your Mail/DNS-Administrator to correct HELO and DNS MX settings or to get removed from DNSBLs; please relay via your ISP (lists.example.org)">>},
        {rbl, <<"550 \"JunkMail rejected - outbound.example.org\n[192.0.2.1]:62722 is in an RBL:\nBlocked - see https://www.spamcop.net/bl.shtml?192.0.2.1\"">>},
        {rbl, <<"554 5.7.1 5.7.1 Service unavailable; Client host [192.0.2.1] blocked using bl.spamcop.net; Blocked - see https://www.spamcop.net/bl.shtml?192.0.2.1">>},
        {rbl, <<"554 5.7.1 5.7.1 Service unavailable; Client host [192.0.2.1] blocked using bl.spamcop.net; Blocked - see https://www.spamcop.net/bl.shtml?192.0.2.1">>},
        {rbl, <<"550 An RBL has blocked the sender's IP address.">>},
        {rbl, <<"550 Sophos Anti Spam Engine has blocked this Email because the sender IP\nAddress is blacklisted.">>}
    ].

classification_test_() ->
    [{integer_to_list(N), fun() ->
        ?assertEqual(Class, z_email_delivery_status:policy_failure(Text)),
        ?assertEqual(Class, z_email_delivery_status:policy_failure(z_string:to_upper(Text))),
        ?assertEqual(Class, z_email_delivery_status:policy_failure({bounce, Text})),
        ?assertEqual(Class, z_email_delivery_status:policy_failure({error, Text}))
    end} || {N, {Class, Text}} <- lists:enumerate(examples())].

recipient_failures_test_() ->
    [?_assertEqual(false, z_email_delivery_status:policy_failure(Text)) || Text <- [
        undefined,
        <<"550 5.1.1 <missing@example.org>: User unknown">>,
        <<"550 5.7.1 Access denied">>,
        <<"Not delivering to a user who marked your messages as spam">>,
        <<"Not delivering to previously bounced address">>,
        <<"Not delivering to unsubscribed address">>,
        <<"Recipient blocked by Zotonic module">>,
        <<"550 User unknown; is in an RBL">>,
        <<"Spam complaint; message appeared to be unwanted by recipient">>
    ]].

gmail_test() ->
    ?assertEqual(gmail, z_email_delivery_status:policy_failure(
        <<"550-5.7.1 To reduce the amount of spam sent to Gmail, this message has been blocked.">>
    )),
    ?assertEqual(gmail, z_email_delivery_status:policy_failure(
        <<"550 5.7.1 https://support.google.com/mail/?p=UnsolicitedMessageError">>
    )).

%% Mocks are VM-global; isolate them from any running site and its workers.
observers_test_() ->
    {timeout, 30, fun() ->
        {ok, Peer, _} = peer:start_link(#{connection => standard_io, args => ["-pa" | code:get_path()]}),
        try peer:call(Peer, ?MODULE, observers, [], 25000)
        after peer:stop(Peer)
        end
    end}.

observers() ->
    Context = #context{},
    Self = self(),
    Modules = [z_db, z_depcache, z_notifier, m_mailinglist_run, m_identity],
    try
        lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
        meck:expect(z_db, q, fun(_, _, _) -> error(unexpected_address_change) end),
        meck:expect(z_db, transaction, fun(_, _) -> error(unexpected_address_change) end),
        meck:expect(m_mailinglist_run, message, fun(Id, State, Final, Retry, Detail, _) ->
            Self ! {delivery, Id, State, Final, Retry, Detail}, ok
        end),
        lists:foreach(fun({_Class, Text}) ->
            %% Include repeated nonfinal attempts and final SMTP/asynchronous errors.
            lists:foreach(fun({Final, Reason, Expected}) ->
                Event = #email_failed{message_nr = <<"test-message">>,
                    recipient = <<"recipient@example.org">>, is_final = Final,
                    retry_ct = 3, reason = Reason, status = Text},
                lists:foreach(fun(_) ->
                    ok = mod_email_status:observe_email_failed(Event, Context)
                end, lists:seq(1, 6)),
                ok = m_email_status:mark_failed(<<"recipient@example.org">>, Final, {bounce, Text}, Context),
                ok = mod_mailinglist:observe_email_failed(Event, Context),
                receive
                    {delivery, <<"test-message">>, Expected, Final, 3, {Reason, Text}} -> ok
                after 1000 -> error(missing_delivery_failure)
                end
            end, [{false, retry, <<"retrying">>}, {true, smtphost, <<"failed">>}, {true, bounce, <<"bounced">>}])
        end, examples()),
        Exhausted = #email_failed{message_nr = <<"exhausted-message">>,
            recipient = <<"recipient@example.org">>, is_final = true,
            retry_ct = 11, reason = retry, status = <<"Retries exceeded: 550 An RBL has blocked the sender IP">>},
        ok = mod_email_status:observe_email_failed(Exhausted, Context),
        ok = mod_mailinglist:observe_email_failed(Exhausted, Context),
        receive
            {delivery, <<"exhausted-message">>, <<"failed">>, true, 11, {retry, <<"Retries exceeded: 550 An RBL has blocked the sender IP">>}} -> ok
        after 1000 -> error(missing_exhausted_delivery)
        end,
        ?assertEqual(0, meck:num_calls(z_db, q, '_')),
        ?assertEqual(0, meck:num_calls(z_db, transaction, '_')),
        %% Existing recipient errors still invalidate and immediately suppress
        %% known-bad mailboxes/complaints. Use an existing address-status row.
        meck:expect(m_identity, normalize_key, fun(email, Email) -> Email end),
        meck:expect(z_db, transaction, fun(F, Ctx) -> F(Ctx) end),
        meck:expect(z_db, q, fun(Sql, Args, _) ->
            case {string:find(Sql, "select is_valid"), string:find(Sql, "select recent_error")} of
                {nomatch, nomatch} -> Self ! {address_update, Args}, 1;
                {nomatch, _} -> [{undefined}];
                {_, _} -> [{true, false, undefined, 0, false}]
            end
        end),
        meck:expect(z_depcache, flush, fun(_, _) -> ok end),
        meck:expect(z_notifier, notify, fun(#email_status{is_valid=false}, _) -> ok end),
        lists:foreach(fun(Text) ->
            ok = m_email_status:mark_failed(<<"recipient@example.org">>, true, Text, Context),
            receive
                {address_update, [<<"recipient@example.org">>, true, Text, 5, _]} -> ok
            after 1000 -> error(recipient_not_suppressed)
            end
        end, [<<"550 5.1.1 User unknown">>, <<"Not delivering to a user who marked your messages as spam">>]),
        relay_round_trip(Context)
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.


%% Exercise the real queue, relay observer, JSON payload, receiver model and
%% delivery-report handler. Only external site services are mocked in this peer.
relay_round_trip(Context) ->
    export_queue_helpers(),
    Self = self(),
    Dir = filename:join("/tmp", "email_policy_queue_" ++ integer_to_list(erlang:unique_integer([positive]))),
    application:load(mnesia),
    application:set_env(mnesia, dir, Dir),
    ok = mnesia:create_schema([node()]),
    {ok, _} = application:ensure_all_started(mnesia),
    ExtraModules = [z_context, z_pivot_rsc, z_acl, z_auth],
    try
        lists:foreach(fun(M) -> meck:new(M, [no_link]) end, ExtraModules),
        meck:expect(z_context, depickle, fun(_) -> Context end),
        meck:expect(z_context, depickle_site, fun(_) -> policy_test end),
        meck:expect(z_acl, is_allowed, fun(_, _, _) -> true end),
        meck:expect(z_acl, user, fun(_) -> 1 end),
        meck:expect(z_auth, is_auth, fun(_) -> true end),
        meck:expect(z_pivot_rsc, insert_task, fun(_, _, _, [_, _, _, 0, Report], _) ->
            Self ! {webhook, Report}, ok
        end),
        meck:expect(z_db, q, fun(Sql, Args, _) ->
            case {string:find(Sql, "from email_relay"), string:find(Sql, "select is_valid"),
                  string:find(Sql, "select recent_error")} of
                {nomatch, nomatch, nomatch} -> Self ! {address_update, Args}, 1;
                {nomatch, nomatch, _} -> [{undefined}];
                {nomatch, _, _} -> [{true, false, undefined, 0, false}];
                {_, _, _} -> [{<<"https://sender.example.org/status">>, <<"sender-message">>}]
            end
        end),
        meck:expect(z_notifier, notify, fun
            (#email_failed{} = Event, _) -> Self ! {failure, Event}, ok;
            (#email_status{}, _) -> ok;
            (#zlog{}, _) -> ok
        end),
        %% Fresh install, followed by an old queue upgrade with mail retained.
        ok = z_email_server:create_email_queue(),
        Fields = mnesia:table_info(email_queue, attributes),
        {atomic, ok} = mnesia:delete_table(email_queue),
        {atomic, ok} = mnesia:create_table(email_queue, [{attributes, lists:droplast(Fields)}]),
        Email = <<"recipient@example.org">>,
        Old = {email_queue, <<"old-message">>, {0,0,0}, 0, Email,
               #email{to=Email}, {0,0,0}, undefined, Context},
        mnesia:dirty_write(Old),
        ok = z_email_server:create_email_queue(),
        ?assertEqual([erlang:append_element(Old, undefined)], mnesia:dirty_read(email_queue, <<"old-message">>)),
        ok = z_email_server:create_email_queue(),
        ?assertEqual(Fields, mnesia:table_info(email_queue, attributes)),
        mnesia:dirty_delete(email_queue, <<"old-message">>),
        true = register(z_email_server, self()),
        lists:foreach(fun({Diagnostic, IsPolicy}) ->
            Row = {email_queue, <<"relay-message">>, {0,0,0}, 11, Email,
                   #email{to=Email}, {0,0,0}, undefined, Context, undefined},
            mnesia:dirty_write(Row),
            %% Both delayed RBL retries and automatic retry scheduling retain errors.
            Delay = case IsPolicy of true -> 720; false -> undefined end,
            ok = z_email_server:update_emailq_delay(<<"relay-message">>, Delay, Diagnostic),
            [Updated] = mnesia:dirty_read(email_queue, <<"relay-message">>),
            ?assertEqual(Diagnostic, element(10, Updated)),
            ok = z_email_server:delete_failed_messages(#{policy_test => running}),
            Event = receive {failure, E} -> E after 1000 -> error(no_exhaustion) end,
            ?assertMatch(#email_failed{reason=retry, is_final=true}, Event),
            check_address_effect(Event, IsPolicy, Context),
            %% Relay sends permanent_failure and the sender reconstructs reason=bounce.
            ok = mod_email_relay:observe_email_failed(Event, Context),
            Report = receive {webhook, R} -> R after 1000 -> error(no_webhook) end,
            Payload = #{<<"type">> => <<"delivery_report">>, <<"report">> => Report},
            Decoded = z_json:decode(z_json:encode(Payload)),
            SenderRow = setelement(2, setelement(4, Row, 0), <<"sender-message">>),
            mnesia:dirty_write(SenderRow),
            ok = m_email_relay:m_post([<<"status">>], #{payload => Decoded}, Context),
            Cast = receive {'$gen_cast', C} -> C after 1000 -> error(no_delivery_cast) end,
            {noreply, test_state} = z_email_server:handle_cast(Cast, test_state),
            Received = receive {failure, E1} -> E1 after 1000 -> error(no_delivery_report) end,
            ?assertMatch(#email_failed{reason=bounce, is_final=true}, Received),
            ?assertEqual(Event#email_failed.status, Received#email_failed.status),
            check_address_effect(Received, IsPolicy, Context),
            ok = mod_mailinglist:observe_email_failed(Received, Context),
            ExpectedDetail = Received#email_failed.status,
            receive
                {delivery, <<"sender-message">>, <<"bounced">>, true, _, {bounce, ExpectedDetail}} -> ok
            after 1000 -> error(missing_relay_delivery)
            end,
            ?assertEqual([], mnesia:dirty_read(email_queue, <<"relay-message">>)),
            ?assertEqual([], mnesia:dirty_read(email_queue, <<"sender-message">>))
        end, [{Text, true} || {_, Text} <- examples()] ++ [
            {<<"450 Mailbox temporarily unavailable">>, false},
            {undefined, false}
        ])
    after
        case whereis(z_email_server) of Self -> unregister(z_email_server); _ -> ok end,
        application:stop(mnesia),
        mnesia:delete_schema([node()]),
        file:del_dir_r(Dir),
        lists:foreach(fun meck:unload/1, ExtraModules)
    end.

check_address_effect(Event, true, Context) ->
    Calls = meck:num_calls(z_db, q, '_'),
    ok = mod_email_status:observe_email_failed(Event, Context),
    ?assertEqual(Calls, meck:num_calls(z_db, q, '_'));
check_address_effect(Event, false, Context) ->
    ok = mod_email_status:observe_email_failed(Event, Context),
    receive
        {address_update, [<<"recipient@example.org">>, true, _, 1, _]} -> ok
    after 1000 -> error(non_policy_failure_not_counted)
    end.

%% Expose queue internals only inside this isolated test VM; production exports
%% remain unchanged. This lets the regression cover actual persistence/expiry.
export_queue_helpers() ->
    {z_email_server, Beam, Filename} = code:get_object_code(z_email_server),
    {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} = beam_lib:chunks(Beam, [abstract_code]),
    Exports = {attribute, 0, export, [{create_email_queue, 0}, {update_emailq_delay, 3}, {delete_failed_messages, 1}]},
    [File, Module | Rest] = Forms,
    {ok, z_email_server, Binary} = compile:forms([File, Module, Exports | Rest], [binary]),
    {module, z_email_server} = code:load_binary(z_email_server, Filename, Binary).
