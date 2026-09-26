-module(websub_lifecycle_tests).
-export([oauth_fetch_options/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

lifecycle_test_() -> {timeout, 60, fun lifecycle/0}.

lifecycle() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),
    ok = z_module_manager:upgrade_await(C),
    A = z_acl:logon(1, C),
    Topic = <<"https://websub-source.test/id/123">>,
    Payload = payload(Topic, 1, <<"Initial">>),
    {ok, {Id, _}} = m_rsc_import:import(Payload, [{import_edges, 0}], A),
    ok = meck:new(z_websub_discovery, [passthrough]),
    ok = meck:expect(z_websub_discovery, discover, fun(Url, _) ->
        {ok, #{topic => Url, hubs => [<<"https://hub.test/">>]}}
    end),
    ok = meck:new(z_websub_http, [passthrough]),
    ok = meck:expect(z_websub_http, post_form, fun(_, _, _) -> {ok, accepted} end),
    try
        ?assertEqual({error, eacces}, m_websub:subscribe(Id, C)),
        ?assertEqual(ok, m_websub:subscribe(Id, A)),
        ?assertEqual(ok, m_websub:subscribe(Id, A)),
        ?assertEqual(1, z_db:q1("select count(*) from websub_import where local_rsc_id=$1", [Id], A)),
        ok = z_websub_subscription:process(A),
        {ImportId, Token, Secret} = z_db:q_row("select id, callback_token, secret from websub_import where local_rsc_id=$1", [Id], A),
        ?assertEqual({error, no_intent}, z_websub_subscription:verify(<<"unknown">>, Topic, <<"subscribe">>, 600, A)),
        ?assertEqual({error, no_intent}, z_websub_subscription:verify(Token, <<"https://other.test/">>, <<"subscribe">>, 600, A)),
        ?assertEqual({error, no_intent}, z_websub_subscription:verify(Token, Topic, <<"unsubscribe">>, undefined, A)),
        VerifyContext = z_context:set_q([{<<"token">>, Token}, {<<"hub.topic">>, Topic},
            {<<"hub.mode">>, <<"subscribe">>}, {<<"hub.lease_seconds">>, <<"600">>},
            {<<"hub.challenge">>, <<"challenge_123">>}], C#context{cowreq = websub_test_support:request(#{method => <<"GET">>})}),
        ?assertMatch({false, _}, controller_websub:malformed_request(VerifyContext)),
        ?assertMatch({false, _}, controller_websub:malformed_request(z_context:set_q(<<"hub.challenge">>, <<"a+b/c.d-e_f=">>, VerifyContext))),
        {<<"challenge_123">>, VerifiedContext} = controller_websub:process(<<"GET">>, undefined, undefined, VerifyContext),
        ?assertEqual(<<"nosniff">>, maps:get(<<"x-content-type-options">>, maps:get(resp_headers, VerifiedContext#context.cowreq))),
        ?assertMatch({true, _}, controller_websub:malformed_request(z_context:set_q(<<"hub.lease_seconds">>, <<"0">>, VerifyContext))),
        ?assertMatch({true, _}, controller_websub:malformed_request(z_context:set_q(<<"hub.challenge">>, <<"<script>">>, VerifyContext))),
        ?assertEqual({error, no_intent}, z_websub_subscription:verify(Token, Topic, <<"subscribe">>, 600, A)),
        ?assertMatch(#{is_active := true}, z_websub_subscription:status(Id, A)),
        ok = z_websub_subscription:denied(Token, Topic, <<"late denial">>, A),
        ?assertMatch(#{is_active := true, is_enabled := true}, z_websub_subscription:status(Id, A)),
        ?assertEqual(true, z_db:q1("select next_check < lease from websub_import where id=$1", [ImportId], A)),
        z_db:q("delete from websub_import_queue where import_id=$1", [ImportId], A),

        % A signed delivery maps to the existing resource and preserves import options.
        Push = payload(Topic, 2, <<"Updated">>),
        Body = z_json:encode(Push),
        Signature = <<"sha256=", (binary:encode_hex(crypto:mac(hmac, sha256, Secret, Body)))/binary>>,
        CallbackContext = z_context:set_q(<<"token">>, Token, C),
        ?assertEqual({error, no_subscription}, m_websub:handle_push_notification(Push, Body, <<"sha256=00">>, CallbackContext)),
        ?assertEqual(ok, m_websub:handle_push_notification(Push, Body, Signature, CallbackContext)),
        ok = m_websub:process_import_queue(A),
        ?assertEqual(<<"Updated">>, m_rsc:p(Id, title, A)),
        {ok, Saved} = m_rsc_import:get_import_status(Id, A),
        ?assertEqual(0, proplists:get_value(import_edges, maps:get(<<"options">>, Saved))),
        ?assertEqual(2, z_db:q1("select last_import_version from websub_import where id=$1", [ImportId], A)),

        % Renewing does not interrupt the confirmed lease; a failed POST retains it.
        z_db:q("update websub_import set next_check=now() where id=$1", [ImportId], A),
        meck:expect(z_websub_http, post_form, fun(_, _, _) -> {error, timeout} end),
        ok = z_websub_subscription:process(A),
        ?assertMatch(#{is_active := true, last_error := _}, z_websub_subscription:status(Id, A)),
        {RenewId, RenewToken} = z_db:q_row("select id, callback_token from websub_import where replaces_id=$1", [ImportId], A),
        ?assertNotEqual(Token, RenewToken),
        ?assertEqual(ok, z_websub_subscription:verify(RenewToken, Topic, <<"subscribe">>, 1200, A)),
        ?assertEqual(false, z_db:q1("select is_enabled from websub_import where id=$1", [ImportId], A)),

        % Stopping clears queued updates immediately but still accepts unsubscribe verification.
        ok = m_websub:unsubscribe(Id, A),
        ?assertEqual(0, z_db:q1("select count(*) from websub_import_queue where import_id=$1", [ImportId], A)),
        ?assertEqual({error, no_intent}, z_websub_subscription:verify(Token, Topic, <<"subscribe">>, 600, A)),
        ?assertEqual({error, no_subscription}, m_websub:handle_push_notification(Push, Body, Signature, CallbackContext)),
        meck:expect(z_websub_http, post_form, fun(_, _, _) -> {ok, accepted} end),
        ok = z_websub_subscription:process(A),
        ?assertEqual(ok, z_websub_subscription:verify(RenewToken, Topic, <<"unsubscribe">>, undefined, A)),
        ?assertEqual(false, z_db:q1("select is_enabled from websub_import where id=$1", [RenewId], A)),
        ?assertMatch(#{is_enabled := false}, z_websub_subscription:status(Id, A)),

        % Restart, denied subscription, and permission changes.
        ok = m_websub:subscribe(Id, A),
        ok = z_websub_subscription:process(A),
        RestartToken = z_db:q1("select callback_token from websub_import where local_rsc_id=$1 order by id desc limit 1", [Id], A),
        ?assertNotEqual(RenewToken, RestartToken),
        ok = z_websub_subscription:denied(RestartToken, Topic, <<"not allowed">>, A),
        ?assertMatch(#{is_enabled := false}, z_websub_subscription:status(Id, A)),
        render_options(Id, A),
        {ok, _} = m_rsc:update(Id, #{<<"is_authoritative">> => true}, A),
        ?assertEqual({error, eacces}, m_websub:subscribe(Id, A)),
        ?assertEqual({error, eacces}, z_websub_subscription:status(Id, C)),
        export_expiry(A)
    after
        meck:unload(z_websub_http),
        meck:unload(z_websub_discovery),
        z_db:q("delete from websub_import where local_rsc_id=$1", [Id], A),
        m_rsc:delete(Id, A)
    end.

render_options(Id, Context) ->
    {Html, _} = z_template:render_to_iolist("_dialog_rsc_import_options.tpl", [{id, Id}, {import_options, [{import_edges, 1}]}], Context),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Html), <<"z_import_edges">>)),
    {Status, _} = z_template:render_to_iolist("_admin_rsc_import_status.tpl", [{id, Id}], Context),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Status), <<"Start automatic updates">>)).

export_expiry(Context) ->
    Callback = <<"https://callback.test/">>,
    Topic = <<"https://source.test/id/1">>,
    ok = m_websub:update_export(Callback, Topic, 1, 0, undefined, Context),
    ok = m_websub:queue_push(1, 10000, Context),
    ?assertEqual(0, z_db:q1("select count(*) from websub_push_queue q join websub_export e on e.id=q.export_id where e.callback_url=$1", [Callback], Context)),
    ok = m_websub:update_export(Callback, Topic, 1, 600, undefined, Context),
    z_db:q("update websub_export set is_error=true where callback_url=$1", [Callback], Context),
    ok = m_websub:queue_push(1, 10000, Context),
    ?assertEqual(1, z_db:q1("select count(*) from websub_push_queue q join websub_export e on e.id=q.export_id where e.callback_url=$1", [Callback], Context)),
    ok = m_websub:delete_export(Callback, Topic, Context).

payload(Topic, Version, Title) ->
    #{<<"uri">> => Topic, <<"uri_template">> => <<"https://websub-source.test/id/:id">>,
      <<"is_a">> => [<<"text">>], <<"version">> => Version,
      <<"resource">> => #{<<"title">> => Title, <<"is_published">> => true}}.

import_opt_in_test_() -> {timeout, 30, fun import_opt_in/0}.

import_opt_in() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    Topic = <<"https://websub-source.test/id/opt-in">>,
    {ok, {Id, _}} = m_rsc_import:import(payload(Topic, 1, <<"Opt in">>), [{is_subscribe, true}], C),
    try
        await_subscription(Id, C, 100),
        {ok, Status} = m_rsc_import:get_import_status(Id, C),
        ?assertNot(proplists:is_defined(is_subscribe, maps:get(<<"options">>, Status))),
        ?assertMatch(#{is_enabled := true}, z_websub_subscription:status(Id, C)),
        ok = m_websub:unsubscribe(Id, C),
        {ok, {Id, _}} = m_rsc_import:import(payload(Topic, 2, <<"One-off update">>), [], C),
        ?assertMatch(#{is_enabled := false}, z_websub_subscription:status(Id, C)),
        ok = z_websub_subscription:install(C),
        ?assertMatch(#{is_enabled := false}, z_websub_subscription:status(Id, C))
    after
        z_db:q("delete from websub_import where local_rsc_id=$1", [Id], C),
        m_rsc:delete(Id, C)
    end.

await_subscription(_, _, 0) -> error(subscription_not_created);
await_subscription(Id, Context, N) ->
    case z_db:q1("select id from websub_import where local_rsc_id=$1", [Id], Context) of
        undefined -> timer:sleep(10), await_subscription(Id, Context, N - 1);
        _ -> ok
    end.

publisher_delivery_test_() -> {timeout, 30, fun publisher_delivery/0}.

publisher_delivery() -> publisher_delivery(0).
publisher_private_delivery_test_() -> {timeout, 30, fun() -> publisher_delivery(2) end}.

publisher_delivery(Visibility) ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => text, <<"title">> => <<"WebSub publisher">>,
        <<"is_published">> => Visibility =:= 0}, C),
    Topic = m_websub:topic_url(Id, C),
    Uri = m_rsc:uri(Id, C),
    Callback = <<"https://callback.test/resource">>,
    Secret = <<"delivery-test-secret">>,
    case Visibility of
        0 -> ok;
        _ -> ?assertMatch({error, _}, m_rsc_export:full(Id, z_acl:anondo(C)))
    end,
    TopicContext = z_context:set_q(<<"id">>, integer_to_binary(Id), C#context{cowreq = websub_test_support:request(#{})}),
    {TopicBody, _} = controller_websub_topic:process(<<"GET">>, undefined, undefined, TopicContext),
    ok = meck:new(z_websub_http, [passthrough]),
    try
        meck:expect(z_websub_http, fetch, fun(post, Url, Body, Options, _) ->
            ?assertEqual(Callback, Url),
            ?assertEqual(z_json:decode(TopicBody), z_json:decode(Body)),
            Headers = proplists:get_value(headers, Options),
            ?assertMatch({ok, #{topic := Topic}}, z_websub_discovery:links(Topic, Headers, <<>>, undefined)),
            ?assert(m_websub:verify_push_signature(proplists:get_value(<<"x-hub-signature">>, Headers), Secret, Body)),
            ?assertMatch(#{<<"status">> := <<"ok">>, <<"result">> := #{<<"id">> := Id, <<"uri">> := Uri}}, z_json:decode(Body)),
            {ok, {binary_to_list(Url), [], 0, <<>>}}
        end),
        ok = m_websub:update_export(Callback, Topic, Id, 600, Secret, C),
        Version = m_rsc:p(Id, version, C),
        ok = m_websub:queue_push(Id, Version, C),
        ok = m_websub:process_push_queue(C),
        ?assertEqual(Version, z_db:q1("select last_push_version from websub_export where callback_url=$1", [Callback], C)),
        ?assert(meck:called(z_websub_http, fetch, [post, Callback, '_', '_', '_']))
    after
        meck:unload(z_websub_http),
        m_websub:delete_export(Callback, Topic, C),
        m_rsc:delete(Id, C)
    end.

identity_and_topic_test_() -> {timeout, 30, fun identity_and_topic/0}.
identity_and_topic() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    Uri = <<"https://identity.test/id/123">>,
    Topic = <<"https://identity.test/feed/123.json">>,
    {ok, {Id, _}} = m_rsc_import:import(payload(Uri, 1, <<"Original">>), [], C),
    ok = meck:new(z_websub_discovery, [passthrough]),
    ok = meck:expect(z_websub_discovery, discover, fun(Uri0, _) ->
        ?assertEqual(Uri, Uri0), {ok, #{topic => Topic, hubs => [<<"https://identity.test/hub">>]}}
    end),
    ok = meck:new(z_websub_http, [passthrough]),
    ok = meck:expect(z_websub_http, post_form, fun(_, Form, _) ->
        ?assertEqual(Topic, proplists:get_value(<<"hub.topic">>, Form)), {ok, accepted}
    end),
    try
        ok = m_websub:subscribe(Id, C),
        ok = z_websub_subscription:process(C),
        {ImportId, Token} = z_db:q_row("select id, callback_token from websub_import where local_rsc_id=$1", [Id], C),
        ?assertEqual(Uri, z_db:q1("select source_uri from websub_import where id=$1", [ImportId], C)),
        ok = z_websub_subscription:verify(Token, Topic, <<"subscribe">>, 600, C),
        z_db:q("delete from websub_import_queue where import_id=$1", [ImportId], C),
        % Changing the source invalidates queued updates from the old subscription.
        {ok, _} = m_rsc:update(Id, #{<<"uri">> => <<"https://new.test/id/1">>}, C),
        m_websub:queue_import(ImportId, 2, payload(Uri, 2, <<"Stale">>), C),
        m_websub:process_import_queue(C),
        ?assertEqual(<<"Original">>, m_rsc:p(Id, title, C)),
        ?assertEqual(<<"https://new.test/id/1">>, m_rsc:p(Id, uri, C)),
        {ok, _} = m_rsc:update(Id, #{<<"uri">> => Uri}, C),
        z_db:q("update websub_import set is_enabled=true, is_unsubscribed=false where id=$1", [ImportId], C),
        % An untrusted refetch cannot replace the selected resource with another identity.
        m_websub:queue_import(ImportId, 2, payload(<<"https://other.test/id/999">>, 2, <<"Wrong">>), C),
        m_websub:process_import_queue(C),
        ?assertEqual(<<"Original">>, m_rsc:p(Id, title, C)),
        ?assertEqual(Uri, m_rsc:p(Id, uri, C)),
        ?assertMatch(#{is_enabled := false, last_error := <<"resource_identity_changed">>}, z_websub_subscription:status(Id, C))
    after
        meck:unload(z_websub_http), meck:unload(z_websub_discovery),
        z_db:q("delete from websub_import where local_rsc_id=$1", [Id], C), m_rsc:delete(Id, C)
    end.

redirect_credentials_test() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    ok = meck:new(z_websub_http, [passthrough]),
    try
        meck:expect(z_websub_http, fetch, fun
            (get, <<"https://source.test/id/1">> = U, _, _, Ctx) ->
                ?assertEqual(1, z_acl:user(Ctx)),
                {error, {303, U, [{"location", "https://other.test/data"}], 0, <<>>}};
            (get, <<"https://other.test/data">> = U, _, _, Ctx) ->
                ?assertEqual(undefined, z_acl:user(Ctx)),
                {error, {302, U, [{"location", "https://source.test/back"}], 0, <<>>}};
            (get, <<"https://source.test/back">> = U, _, _, Ctx) ->
                ?assertEqual(undefined, z_acl:user(Ctx)), {ok, {U, [], 2, <<"{}">>}}
        end),
        ?assertEqual({ok, #{}}, z_websub_fetch_zotonic:fetch_json(<<"https://source.test/id/1">>, C)),
        meck:expect(z_websub_http, fetch, fun(get, U, _, _, _) ->
            {error, {302, U, [{"location", "http://source.test/plain"}], 0, <<>>}}
        end),
        ?assertEqual({error, insecure_redirect}, z_websub_http:get(<<"https://source.test/id/1">>, [], C))
    after meck:unload(z_websub_http) end.

verification_admission_test() ->
    C = z_context:new(zotonic_site_testsandbox),
    % Keep queued tasks invisible to workers until they are removed by this test.
    ok = z_db:transaction(fun(Ctx) ->
        z_db:q("delete from pivot_task_queue where module=$1 and function=$2", [controller_websub, task_verify], Ctx),
        z_db:q("update websub_request_limit set requests=0, window_start=now() where id=1", Ctx),
        Q = [{<<"hub.mode">>, <<"unsubscribe">>}, {<<"hub.topic">>, m_websub:topic_url(1, Ctx)},
             {<<"hub.callback">>, <<"https://callback.test/">>}, {<<"hub.lease_seconds">>, <<"ignored">>}],
        Req = z_context:set_q(Q, Ctx#context{cowreq = websub_test_support:request(#{method => <<"POST">>, headers => #{}, peer => {{127,0,0,1}, 12345}, path => <<"/.zotonic/websub">>, qs => <<>>})}),
        ?assertMatch({false, _}, controller_websub:malformed_request(Req)),
        PostReq = Req#context{cowreq = undefined},
        ?assertMatch({{halt, 202}, _}, controller_websub:process(<<"POST">>, undefined, undefined, PostReq)),
        ?assertMatch({{halt, 202}, _}, controller_websub:process(<<"POST">>, undefined, undefined, PostReq)),
        ?assertEqual(1, z_db:q1("select count(*) from pivot_task_queue where module=$1 and function=$2",
            [controller_websub, task_verify], Ctx)),
        z_db:q("update websub_request_limit set requests=120 where id=1", Ctx),
        Other = z_context:set_q(<<"hub.callback">>, <<"https://other.test/">>, PostReq),
        ?assertMatch({{halt, 503}, _}, controller_websub:process(<<"POST">>, undefined, undefined, Other)),
        z_db:q("delete from pivot_task_queue where module=$1 and function=$2", [controller_websub, task_verify], Ctx),
        z_db:q("update websub_request_limit set requests=0 where id=1", Ctx),
        ok
    end, C).

%% Exercise the real mod_oauth2 observer; only token storage and network I/O
%% are mocked. Options are prepared by z_fetch, including OAuth2 authorization.
oauth_fetch_options(Event, Context) ->
    mod_oauth2:observe_url_fetch_options(Event, Context).

oauth_transport_test() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    Observer = {?MODULE, oauth_fetch_options},
    z_notifier:observe(url_fetch_options, Observer, 1, C),
    ok = meck:new(m_oauth2_consumer, [passthrough]),
    ok = meck:new(z_websub_http, [passthrough]),
    ok = meck:new(z_url_fetch, [passthrough]),
    try
        meck:expect(m_oauth2_consumer, find_token, fun(1, <<"source.test">>, _) ->
            {ok, <<"oauth-websub-test">>}
        end),
        meck:expect(z_websub_http, destination, fun(Url) ->
            {ok, uri_string:parse(Url), {93,184,215,14}}
        end),
        meck:expect(z_url_fetch, fetch, fun(Method, Url, _, Options) ->
            ?assertEqual(false, proplists:get_value(autoredirect, Options)),
            ?assertEqual(false, proplists:get_value(insecure, Options)),
            case {Method, Url} of
                {get, <<"https://source.test/id/123">>} ->
                    ?assertEqual(<<"Bearer oauth-websub-test">>, proplists:get_value(authorization, Options)),
                    {error, {303, Url, [{"location", "/representation"}], 0, <<>>}};
                {get, <<"https://source.test/representation">>} ->
                    ?assertEqual(<<"Bearer oauth-websub-test">>, proplists:get_value(authorization, Options)),
                    {error, {303, Url, [{"location", "https://other.test/export"}], 0, <<>>}};
                {get, <<"https://other.test/export">>} ->
                    ?assertEqual(undefined, proplists:get_value(authorization, Options)),
                    {error, {303, Url, [{"location", "https://source.test/back"}], 0, <<>>}};
                {get, <<"https://source.test/back">>} ->
                    ?assertEqual(undefined, proplists:get_value(authorization, Options)),
                    {ok, {binary_to_list(Url), [], 2, <<"{}">>}};
                {post, <<"https://source.test/hub">>} ->
                    ?assertEqual(<<"Bearer oauth-websub-test">>, proplists:get_value(authorization, Options)),
                    {ok, {binary_to_list(Url), [], 0, <<>>}}
            end
        end),
        ?assertMatch({ok, {"https://source.test/back", _, _, <<"{}">>}},
            z_websub_http:get(<<"https://source.test/id/123">>, [], C)),
        ?assertMatch({ok, _}, z_websub_http:post_form(<<"https://source.test/hub">>,
            [{<<"hub.mode">>, <<"subscribe">>}], C)),
        ?assert(meck:called(m_oauth2_consumer, find_token, [1, <<"source.test">>, '_'])),
        ?assertNot(meck:called(m_oauth2_consumer, find_token, ['_', <<"other.test">>, '_']))
    after
        z_notifier:detach(url_fetch_options, C),
        meck:unload(z_url_fetch), meck:unload(z_websub_http), meck:unload(m_oauth2_consumer)
    end.

%% Exhausting retries for an old notification must not delete a newer update
%% queued while the HTTP request was in flight.
queue_race_test() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => text, <<"is_published">> => true}, C),
    {ok, Id} = m_rsc:update(Id, #{<<"title">> => <<"Latest representation">>}, C),
    Callback = <<"https://callback.test/queue-race">>,
    Topic = m_websub:topic_url(Id, C),
    ok = m_websub:update_export(Callback, Topic, Id, 600, undefined, C),
    ok = meck:new(z_websub_http, [passthrough]),
    try
        % Even a queued version older than the resource must trigger delivery.
        ok = m_websub:queue_push(Id, 1, C),
        z_db:q("update websub_push_queue set retry_count=8 where local_rsc_id=$1", [Id], C),
        meck:expect(z_websub_http, fetch, fun(post, Callback0, Body, Options, _) ->
            ?assertEqual(Callback, Callback0),
            ?assertEqual(65536, proplists:get_value(max_length, Options)),
            ?assertMatch(#{<<"result">> := #{<<"resource">> := #{<<"title">> := <<"Latest representation">>}}}, z_json:decode(Body)),
            ok = m_websub:queue_push(Id, 100, C),
            {error, timeout}
        end),
        ok = m_websub:process_push_queue(C),
        ?assertEqual({100, 0}, z_db:q_row("select version, retry_count from websub_push_queue where local_rsc_id=$1", [Id], C)),
        ?assert(meck:called(z_websub_http, fetch, [post, Callback, '_', '_', '_']))
    after
        meck:unload(z_websub_http), m_rsc:delete(Id, C)
    end.

disabled_import_user_test() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    Uri = <<"https://source.test/id/disabled-user">>,
    {ok, {Id, _}} = m_rsc_import:import(payload(Uri, 1, <<"Original">>), [], C),
    InvalidTopicContext = z_context:set_q([
        {<<"hub.mode">>, <<"subscribe">>}, {<<"hub.callback">>, <<"https://callback.test/">>},
        {<<"hub.topic">>, m_websub:topic_url(Id, C)}], C#context{cowreq = websub_test_support:request(#{method => <<"POST">>})}),
    ?assertMatch({true, _}, controller_websub:malformed_request(InvalidTopicContext)),
    ok = m_websub:subscribe(Id, C),
    ImportId = z_db:q1("select id from websub_import where local_rsc_id=$1", [Id], C),
    z_db:q("update websub_import set is_unsubscribed=false where id=$1", [ImportId], C),
    ok = m_websub:queue_import(ImportId, 3, undefined, C),
    ok = m_websub:queue_import(ImportId, 2, payload(Uri, 2, <<"Unauthorized update">>), C),
    % An older pushed payload must not replace a newer pending refetch.
    ?assertEqual({3, undefined}, z_db:q_row("select version, payload from websub_import_queue where import_id=$1", [ImportId], C)),
    ok = meck:new(z_auth, [passthrough]),
    try
        meck:expect(z_auth, is_enabled, fun(_, _) -> false end),
        ?assertEqual({error, eacces}, m_websub:subscribe(Id, C)),
        ok = m_websub:process_import_queue(C),
        ?assertEqual(<<"Original">>, m_rsc:p(Id, title, C)),
        ok = z_websub_subscription:process(C),
        ?assertMatch(#{is_enabled := false}, z_websub_subscription:status(Id, C))
    after
        meck:unload(z_auth),
        z_db:q("delete from websub_import where local_rsc_id=$1", [Id], C), m_rsc:delete(Id, C)
    end.

fetch_error_redaction_test() ->
    ok = meck:new(z_websub_http, [passthrough]),
    try
        meck:expect(z_websub_http, fetch, fun(_, U, _, _, _) ->
            {error, {500, U, [{"set-cookie", "private-cookie"}], 20, <<"private-response">>}}
        end),
        ?assertEqual({error, {http_status, 500}}, z_websub_discovery:discover(<<"https://source.test/">>, #context{})),
        ?assertEqual({error, {http_status, 500}}, z_websub_fetch_zotonic:fetch_json(<<"https://source.test/">>, #context{}))
    after meck:unload(z_websub_http) end.

invalid_delivery_version_test() ->
    C = z_context:new(zotonic_site_testsandbox),
    lists:foreach(fun(V) ->
        P = #{<<"uri">> => <<"https://source.test/id/1">>, <<"version">> => V},
        ?assertEqual({error, missing_version}, m_websub:handle_push_notification(P, <<>>, undefined, C))
    end, [#{}, [], <<"bad">>, -1, 2147483648, <<"999999999999999999999">>]).

%% A cookie-authenticated request with a bogus Authorization header must not
%% authorize delivery of private content. Exercise admission through the controller.
cookie_authorization_test() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    ok = meck:new(z_module_manager, [passthrough]),
    ok = meck:new(m_oauth2, [passthrough]),
    ok = meck:new(z_pivot_rsc, [passthrough]),
    try
        meck:expect(z_module_manager, active, fun
            (mod_oauth2, _) -> true;
            (M, Ctx) -> meck:passthrough([M, Ctx])
        end),
        meck:expect(m_oauth2, decode_bearer_token, fun(_, _) -> {error, unknown_token} end),
        meck:expect(z_pivot_rsc, insert_task, fun(controller_websub, task_verify, _, Args, _) ->
            ?assertEqual(undefined, lists:last(Args)),
            {ok, 1}
        end),
        Q = [{<<"hub.mode">>, <<"subscribe">>}, {<<"hub.topic">>, m_websub:topic_url(1, C)},
            {<<"hub.callback">>, <<"https://callback.test/cookie-auth">>}],
        Req = z_context:set_q(Q, C#context{cowenv = #{cowmachine_remote => <<"127.0.0.1">>, cowmachine_forwarded_proto => <<"https">>},
            cowreq = websub_test_support:request(#{method => <<"POST">>, path => <<"/.zotonic/websub">>, qs => <<>>,
            peer => {{127,0,0,1}, 12345}, headers => #{<<"authorization">> => <<"Bearer bogus">>}})}),
        ?assertMatch({{halt, 202}, _}, controller_websub:process(<<"POST">>, undefined, undefined, Req)),
        ?assert(meck:called(m_oauth2, decode_bearer_token, [<<"bogus">>, '_']))
    after
        meck:unload(z_pivot_rsc), meck:unload(m_oauth2), meck:unload(z_module_manager)
    end.

self_subscription_test() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    {ok, LocalId} = m_rsc:insert(#{<<"category_id">> => text, <<"name">> => <<"websub_self_test">>,
        <<"is_published">> => true}, C),
    {ok, CopyId} = m_rsc:insert(#{<<"category_id">> => text, <<"is_authoritative">> => false,
        <<"uri">> => m_rsc:uri(LocalId, C)}, C),
    ok = meck:new(z_websub_discovery, [passthrough]),
    ok = meck:new(z_websub_http, [passthrough]),
    try
        ?assertEqual({error, eacces}, m_websub:subscribe(LocalId, C)),
        ?assertEqual({error, eacces}, m_websub:subscribe(CopyId, C)),
        ?assertEqual(0, z_db:q1("select count(*) from websub_import where local_rsc_id in ($1,$2)", [LocalId, CopyId], C)),
        {ok, CopyId} = m_rsc:update(CopyId, #{<<"uri">> => <<"https://external.test/id/123">>}, C),
        meck:expect(z_websub_discovery, discover, fun(_, _) ->
            {ok, #{topic => m_websub:topic_url(LocalId, C), hubs => [<<"https://external.test/hub">>]}}
        end),
        meck:expect(z_websub_http, post_form, fun(_, _, _) -> error(self_subscription_posted) end),
        ok = m_websub:subscribe(CopyId, C),
        ok = z_websub_subscription:process(C),
        ?assertMatch(#{is_enabled := false, last_error := <<"self_subscription">>}, z_websub_subscription:status(CopyId, C)),
        ?assertNot(meck:called(z_websub_http, post_form, ['_', '_', '_'])),
        % Stored self-topics must be stopped before renewal; discovering a
        % self-topic during renewal must also stop the old active callback.
        lists:foreach(fun({StoredTopic, ExpectedRows}) ->
            z_db:q("delete from websub_import where local_rsc_id=$1", [CopyId], C),
            ok = m_websub:subscribe(CopyId, C),
            z_db:q("update websub_import set hub_url=$2, topic_url=$3, "
                "is_unsubscribed=false, lease=now()+interval '1 hour', "
                "next_check=now(), pending_mode=null where local_rsc_id=$1",
                [CopyId, <<"https://external.test/hub">>, StoredTopic], C),
            ok = z_websub_subscription:process(C),
            ?assertEqual(ExpectedRows, z_db:q1("select count(*) from websub_import where local_rsc_id=$1", [CopyId], C)),
            ?assertEqual(0, z_db:q1("select count(*) from websub_import where local_rsc_id=$1 and is_enabled", [CopyId], C)),
            ?assertMatch(#{last_error := <<"self_subscription">>}, z_websub_subscription:status(CopyId, C)),
            ?assertNot(meck:called(z_websub_http, post_form, ['_', '_', '_']))
        end, [{m_websub:topic_url(LocalId, C), 1}, {<<"https://external.test/topic">>, 2}])
    after
        meck:unload(z_websub_http), meck:unload(z_websub_discovery),
        z_db:q("delete from websub_import where local_rsc_id=$1", [CopyId], C),
        m_rsc:delete(CopyId, C), m_rsc:delete(LocalId, C)
    end.

authority_change_test() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => text, <<"is_published">> => true}, C),
    Topic = m_websub:topic_url(Id, C),
    Callback = <<"https://callback.test/authority-change">>,
    try
        ok = m_websub:update_export(Callback, Topic, Id, 600, undefined, C),
        ok = m_websub:queue_push(Id, 1, C),
        {ok, Id} = m_rsc:update(Id, #{<<"is_authoritative">> => false, <<"uri">> => <<"https://external.test/id/123">>}, C),
        ok = m_websub:queue_push(Id, 2, C),
        ?assertEqual(0, z_db:q1("select count(*) from websub_export where local_rsc_id=$1", [Id], C)),
        ?assertEqual(0, z_db:q1("select count(*) from websub_push_queue where local_rsc_id=$1", [Id], C)),
        ?assertEqual({error, not_authoritative}, m_websub:update_export(Callback, Topic, Id, 600, undefined, C)),
        Req = z_context:set_q([{<<"hub.mode">>, <<"unsubscribe">>},
            {<<"hub.topic">>, Topic}, {<<"hub.callback">>, Callback}], C#context{cowreq = websub_test_support:request(#{method => <<"POST">>})}),
        ?assertMatch({false, _}, controller_websub:malformed_request(Req)),
        ?assertMatch({true, _}, controller_websub:malformed_request(z_context:set_q(<<"hub.mode">>, <<"subscribe">>, Req)))
    after m_rsc:delete(Id, C) end.

%% Database authority, not a stale resource cache, controls publication.
authority_cache_test() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => text}, C),
    Topic = m_websub:topic_url(Id, C),
    Callback = <<"https://callback.test/authority-cache">>,
    ok = m_websub:update_export(Callback, Topic, Id, 600, undefined, C),
    ok = meck:new(m_rsc, [passthrough]),
    try
        meck:expect(m_rsc, p_no_acl, fun
            (R, is_authoritative, _) when R =:= Id -> false;
            (R, K, Ctx) -> meck:passthrough([R, K, Ctx])
        end),
        ok = m_websub:queue_push(Id, 1, C),
        ?assertEqual(1, z_db:q1("select count(*) from websub_export where local_rsc_id=$1", [Id], C)),
        z_db:q("update rsc set is_authoritative=false where id=$1", [Id], C),
        meck:expect(m_rsc, p_no_acl, fun
            (R, is_authoritative, _) when R =:= Id -> true;
            (R, K, Ctx) -> meck:passthrough([R, K, Ctx])
        end),
        ?assertEqual({error, not_authoritative}, m_websub:update_export(Callback, Topic, Id, 600, undefined, C)),
        ok = m_websub:queue_push(Id, 2, C),
        ?assertEqual(0, z_db:q1("select count(*) from websub_export where local_rsc_id=$1", [Id], C))
    after meck:unload(m_rsc), m_rsc:delete(Id, C) end.

collection_update_test_() ->
    {timeout, 30, fun() ->
        collection_update(false),
        collection_update(true)
    end}.

collection_update(SubscribeParts) ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    RootUri = <<"https://collection.test/id/root">>,
    AUri = <<"https://collection.test/id/a">>,
    BUri = <<"https://collection.test/id/b">>,
    {ok, {A, _}} = m_rsc_import:import(payload(AUri, 1, <<"Existing item">>), [], C),
    Options = [{import_edges, 1}, {is_subscribe_haspart, SubscribeParts}],
    {ok, {Root, _}} = m_rsc_import:import(collection_payload(RootUri, 1, [AUri]), Options, C),
    ok = meck:new(z_websub_fetch_zotonic, [passthrough]),
    try
        meck:expect(z_websub_fetch_zotonic, fetch_json, fun(BUri0, _) when BUri0 =:= BUri ->
            {ok, #{<<"status">> => <<"ok">>, <<"result">> => payload(BUri, 1, <<"New item">>)}}
        end),
        ok = m_websub:subscribe(Root, C),
        ImportId = z_db:q1("select id from websub_import where local_rsc_id=$1", [Root], C),
        z_db:q("update websub_import set is_unsubscribed=false, lease=now()+interval '1 hour', "
            "next_check=null where id=$1", [ImportId], C),
        ok = m_websub:task_import_referred(ImportId, #{AUri => A}, 1, C),
        ok = m_websub:queue_import(ImportId, 2, collection_payload(RootUri, 2, [AUri, BUri]), C),
        ok = m_websub:process_import_queue(C),
        B = m_rsc:uri_lookup(BUri, C),
        ?assert(is_integer(B)),
        ?assertEqual([A, B], m_edge:objects(Root, haspart, C)),
        ?assertNot(m_rsc_import:is_imported(B, C)),
        ?assert(z_db:q1("select count(*) from pivot_task_queue where module=$1 and function=$2",
            [m_websub, task_import_referred], C) > 0),
        ok = m_websub:task_import_referred(ImportId, #{AUri => A, BUri => B}, 1, C),
        ?assert(m_rsc_import:is_imported(B, C)),
        ?assertEqual(<<"New item">>, m_rsc:p(B, title, C)),
        ?assertEqual(<<"Existing item">>, m_rsc:p(A, title, C)),
        {ok, BStatus} = m_rsc_import:get_import_status(B, C),
        ?assertNot(proplists:get_bool(is_subscribe_haspart, maps:get(<<"options">>, BStatus))),
        ExpectedSubscriptions = case SubscribeParts of true -> 2; false -> 0 end,
        ?assertEqual(ExpectedSubscriptions, z_db:q1(
            "select count(*) from websub_import where local_rsc_id in ($1,$2) and is_enabled", [A, B], C)),
        ok = m_websub:queue_import(ImportId, 3, collection_payload(RootUri, 3, [BUri, AUri]), C),
        ok = m_websub:process_import_queue(C),
        ?assertEqual([B, A], m_edge:objects(Root, haspart, C)),
        ok = m_websub:queue_import(ImportId, 4, collection_payload(RootUri, 4, [BUri]), C),
        ok = m_websub:process_import_queue(C),
        ?assertEqual([B], m_edge:objects(Root, haspart, C)),
        ?assert(m_rsc:exists(A, C)),
        ok = m_websub:unsubscribe(Root, C),
        ?assertEqual(ExpectedSubscriptions, z_db:q1(
            "select count(*) from websub_import where local_rsc_id in ($1,$2) and is_enabled", [A, B], C))
    after
        meck:unload(z_websub_fetch_zotonic),
        z_db:q("delete from pivot_task_queue where module=$1 and function=$2", [m_websub, task_import_referred], C),
        lists:foreach(fun(Uri) ->
            case m_rsc:uri_lookup(Uri, C) of
                undefined -> ok;
                Id ->
                    z_db:q("delete from websub_import where local_rsc_id=$1", [Id], C),
                    m_rsc:delete(Id, C)
            end
        end, [RootUri, AUri, BUri])
    end.

collection_payload(Uri, Version, Members) ->
    (payload(Uri, Version, <<"Collection">>))#{
        <<"edges">> => #{
            <<"haspart">> => #{
                <<"predicate">> => #{<<"name">> => <<"haspart">>},
                <<"objects">> => [
                    #{<<"object_id">> => #{<<"uri">> => Member, <<"is_a">> => [<<"text">>]}}
                    || Member <- Members
                ]
            }
        }
    }.

edge_publication_test() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => collection}, C),
    try
        ok = m_websub:update_export(<<"https://callback.test/edges">>, m_websub:topic_url(Id, C), Id, 600, undefined, C),
        lists:foreach(fun(Notify) ->
            Before = m_rsc:p(Id, version, C),
            ok = Notify(C),
            After = m_rsc:p(Id, version, C),
            ?assert(After > Before),
            ?assertEqual(After, z_db:q1("select version from websub_push_queue where local_rsc_id=$1", [Id], C))
        end, [
            fun(Ctx) -> mod_websub:observe_edge_insert(#edge_insert{subject_id = Id, edge_id = 1}, Ctx) end,
            fun(Ctx) -> mod_websub:observe_edge_update(#edge_update{subject_id = Id, edge_id = 1}, Ctx) end,
            fun(Ctx) -> mod_websub:observe_edge_delete(#edge_delete{subject_id = Id, edge_id = 1}, Ctx) end
        ])
    after m_rsc:delete(Id, C) end.

shallow_collection_import_test() ->
    C = z_context:set(websub_safe_import, true,
        z_acl:logon(1, z_context:new(zotonic_site_testsandbox))),
    RootUri = <<"https://depth.test/id/root">>,
    ChildUri = <<"https://depth.test/id/child">>,
    GrandchildUri = <<"https://depth.test/id/grandchild">>,
    ok = meck:new(z_websub_fetch_zotonic, [passthrough]),
    try
        meck:expect(z_websub_fetch_zotonic, fetch_json, fun(Uri, _) ->
            Data = case Uri of
                RootUri -> collection_payload(RootUri, 1, [ChildUri]);
                ChildUri -> collection_payload(ChildUri, 1, [GrandchildUri])
            end,
            {ok, #{<<"status">> => <<"ok">>, <<"result">> => Data}}
        end),
        {ok, {Root, _}} = m_rsc_import:import_uri_recursive(RootUri,
            [{import_edges, 1}, {is_subscribe_haspart, true}], C),
        Child = m_rsc:uri_lookup(ChildUri, C),
        ?assertEqual([Child], m_edge:objects(Root, haspart, C)),
        ?assert(m_rsc_import:is_imported(Child, C)),
        ?assertEqual([], m_edge:objects(Child, haspart, C)),
        ?assertEqual(undefined, m_rsc:uri_lookup(GrandchildUri, C)),
        {ok, Status} = m_rsc_import:get_import_status(Child, C),
        ChildOptions = maps:get(<<"options">>, Status),
        ?assertEqual(0, proplists:get_value(import_edges, ChildOptions)),
        ?assertNot(proplists:get_bool(is_subscribe_haspart, ChildOptions))
    after
        meck:unload(z_websub_fetch_zotonic),
        lists:foreach(fun(Uri) ->
            case m_rsc:uri_lookup(Uri, C) of
                undefined -> ok;
                Id -> m_rsc:delete(Id, C)
            end
        end, [RootUri, ChildUri, GrandchildUri])
    end.

%% Exercise real cron ticks and the unique site worker, not process/1 directly.
automatic_renewal_test_() ->
    {timeout, 30, fun automatic_renewal/0}.

automatic_renewal() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    Uri = <<"https://renewal.test/id/automatic">>,
    Data = payload(Uri, 1, <<"Automatic renewal">>),
    {ok, {Id, _}} = m_rsc_import:import(Data, [], C),
    TestPid = self(),
    ok = meck:new(z_websub_discovery, [passthrough]),
    ok = meck:new(z_websub_http, [passthrough]),
    ok = meck:new(z_websub_fetch_zotonic, [passthrough]),
    try
        meck:expect(z_websub_discovery, discover, fun(Uri0, _) when Uri0 =:= Uri ->
            {ok, #{topic => Uri, hubs => [<<"https://renewal.test/hub">>]}}
        end),
        meck:expect(z_websub_fetch_zotonic, fetch_json, fun(Uri0, _) when Uri0 =:= Uri ->
            {ok, #{<<"status">> => <<"ok">>, <<"result">> => Data}}
        end),
        meck:expect(z_websub_http, post_form, fun(_, Form, _) ->
            Callback = proplists:get_value(<<"hub.callback">>, Form),
            Mode = proplists:get_value(<<"hub.mode">>, Form),
            {ImportId, Token} = z_db:q_row(
                "select id, callback_token from websub_import where callback_url=$1", [Callback], C),
            ok = z_websub_subscription:verify(Token, Uri, Mode, 6, C),
            case Mode of
                <<"subscribe">> -> TestPid ! {automatic_lease, ImportId, Token};
                <<"unsubscribe">> -> ok
            end,
            {ok, accepted}
        end),
        ok = m_websub:subscribe(Id, C),
        {FirstId, FirstToken} = await_automatic_lease(),
        ?assertEqual(true, z_db:q1(
            "select next_check < lease from websub_import where id=$1", [FirstId], C)),
        {NextId, NextToken} = await_automatic_lease(),
        ?assertNotEqual(FirstId, NextId),
        ?assertNotEqual(FirstToken, NextToken),
        ?assertEqual(FirstId, z_db:q1(
            "select replaces_id from websub_import where id=$1", [NextId], C)),
        ?assertEqual(false, z_db:q1(
            "select is_enabled from websub_import where id=$1", [FirstId], C)),
        ?assertEqual(true, z_db:q1(
            "select lease > now() and next_check < lease from websub_import where id=$1", [NextId], C)),
        ?assertMatch(#{is_enabled := true, is_active := true}, z_websub_subscription:status(Id, C))
    after
        z_db:q("delete from websub_import where local_rsc_id=$1", [Id], C),
        m_rsc:delete(Id, C),
        meck:unload(z_websub_fetch_zotonic),
        meck:unload(z_websub_http),
        meck:unload(z_websub_discovery)
    end.

await_automatic_lease() ->
    receive
        {automatic_lease, Id, Token} -> {Id, Token}
    after 10000 ->
        error(automatic_renewal_not_triggered)
    end.
