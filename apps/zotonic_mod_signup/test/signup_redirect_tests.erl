-module(signup_redirect_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

-export([redirect/2]).

normalize_redirect_test() ->
    Context = context(),
    lists:foreach(fun(Page) ->
        ?assertEqual(undefined, m_signup:normalize_redirect(Page, Context))
    end, [undefined, <<>>, "", <<"  ">>, <<"#reload">>, "#reload", #{}, 42, [invalid]]),
    ?assertEqual(local_url(<<"/welcome?q=1">>, Context),
        m_signup:normalize_redirect(" /welcome?q=1 ", Context)),
    lists:foreach(fun(Page) ->
        ?assertEqual(local_url(<<"/">>, Context), m_signup:normalize_redirect(Page, Context))
    end, [<<"https://example.invalid/">>, <<"//example.invalid/">>]),
    ScriptUrl = m_signup:normalize_redirect(<<"javascript:alert(1)">>, Context),
    ?assert(z_context:is_site_url(ScriptUrl, Context)),
    ?assertEqual(nomatch, binary:match(ScriptUrl, <<"javascript:">>)).

redirect_precedence_test() ->
    Context = context(),
    {ok, UserId} = m_rsc:insert(#{<<"category_id">> => person}, Context),
    UserContext = z_acl:logon(UserId, Context),
    try
        Requested = local_url(<<"/welcome">>, Context),
        ?assertEqual(Requested, m_signup:confirm_redirect(<<"/welcome">>, UserContext)),
        ?assertEqual(local_url(m_rsc:p(UserId, <<"page_url">>, UserContext), Context),
            m_signup:confirm_redirect(UserContext)),
        ok = z_notifier:observe(signup_confirm_redirect, {?MODULE, redirect}, 1, Context),
        OverrideContext = z_context:set(signup_test_redirect, <<"/override">>, UserContext),
        ?assertEqual(local_url(<<"/override">>, Context),
            m_signup:confirm_redirect(<<"/welcome">>, OverrideContext)),
        ?assertEqual(Requested, m_signup:confirm_redirect(<<"/welcome">>, UserContext)),
        Anonymous = z_acl:anondo(Context),
        ?assertEqual(local_url(z_dispatcher:url_for(home, Anonymous), Anonymous),
            m_signup:confirm_redirect(<<"/welcome">>, Anonymous))
    after
        z_notifier:detach(signup_confirm_redirect, Context),
        m_rsc:delete(UserId, Context)
    end.

redirect(#signup_confirm_redirect{request_page = RequestPage}, Context) ->
    ?assertEqual(local_url(<<"/welcome">>, Context), RequestPage),
    z_context:get(signup_test_redirect, Context).

confirmation_destination_test() ->
    Context = context(),
    {ok, UserId} = m_rsc:insert(#{<<"category_id">> => person, <<"is_published">> => false}, Context),
    try
        Email = <<"signup-redirect@example.com">>,
        {ok, Id} = m_identity:insert(UserId, email, Email, Context),
        {ok, VerifiedId} = m_identity:insert(UserId, email, <<"signup-verified@example.com">>,
            [{is_verified, true}], Context),
        OriginalProps = #{unrelated => {preserve, <<"value">>}},
        ok = m_identity:set_propb(Id, OriginalProps, Context),
        % Exercise real database serialization, including the set_propb regression.
        ?assertEqual(OriginalProps, identity_props(Id, Context)),
        Page = local_url(<<"/welcome?from=signup">>, Context),
        ok = m_signup:store_confirm_redirect(UserId, Page, Context),
        ?assertEqual(OriginalProps#{signup_ready_page => Page}, identity_props(Id, Context)),
        ?assertEqual(undefined, identity_props(VerifiedId, Context)),
        {ok, OldKey} = m_identity:set_verify_key(Id, Context),
        {ok, Key} = m_identity:set_verify_key(Id, Context),
        ?assertEqual({error, unknown_key}, m_signup:confirm(OldKey, Context)),
        ?assertEqual(OriginalProps#{signup_ready_page => Page}, identity_props(Id, Context)),
        % A fresh, anonymous context represents confirmation in another browser.
        OtherBrowser = z_context:new(zotonic_site_testsandbox),
        ?assertEqual({ok, {UserId, <<"email">>, Email, Page}}, m_signup:confirm(Key, OtherBrowser)),
        ?assertEqual(OriginalProps, identity_props(Id, Context)),
        ?assertEqual(true, z_db:q1("select is_verified from identity where id = $1", [Id], Context)),
        ?assertEqual(undefined, z_db:q1("select verify_key from identity where id = $1", [Id], Context)),
        ?assertEqual(true, m_rsc:p_no_acl(UserId, <<"is_published">>, Context)),
        ?assertEqual(true, m_rsc:p_no_acl(UserId, <<"is_verified_account">>, Context)),
        ?assertEqual({error, unknown_key}, m_signup:confirm(Key, OtherBrowser))
    after
        m_rsc:delete(UserId, Context)
    end.

legacy_properties_and_missing_destination_test() ->
    Context = context(),
    {ok, UserId} = m_rsc:insert(#{<<"category_id">> => person}, Context),
    try
        {ok, Id} = m_identity:insert(UserId, email, <<"signup-proplist@example.com">>, Context),
        Props = [{unrelated, {preserve, 42}}],
        ok = m_identity:set_propb(Id, Props, Context),
        ok = m_signup:store_confirm_redirect(UserId, <<"/first">>, Context),
        ok = m_signup:store_confirm_redirect(UserId, <<"/second">>, Context),
        ?assertEqual([{signup_ready_page, local_url(<<"/second">>, Context)} | Props],
            identity_props(Id, Context)),
        % A new signup without a destination must not inherit an earlier one.
        ok = m_signup:store_confirm_redirect(UserId, <<"#reload">>, Context),
        ?assertEqual(Props, identity_props(Id, Context)),
        {ok, Key} = m_identity:set_verify_key(Id, Context),
        ?assertMatch({ok, {UserId, <<"email">>, _, undefined}}, m_signup:confirm(Key, Context)),
        ?assertEqual(Props, identity_props(Id, Context))
    after
        m_rsc:delete(UserId, Context)
    end.

signup_api_destination_test() ->
    Context = context(),
    SignupProps = [
        {ready_page, <<"/after-signup">>},
        {identity, {email, <<"signup-api-redirect@example.com">>, false, false}}
    ],
    {ok, UserId} = mod_signup:signup(#{<<"title">> => <<"Signup redirect test">>}, SignupProps, true, Context),
    try
        [Identity] = m_identity:get_rsc_by_type(UserId, email, Context),
        Props = proplists:get_value(propb, Identity),
        ?assertEqual(local_url(<<"/after-signup">>, Context), maps:get(signup_ready_page, Props))
    after
        m_rsc:delete(UserId, Context)
    end.

concurrent_confirmation_test() ->
    Context = context(),
    {ok, UserId} = m_rsc:insert(#{<<"category_id">> => person}, Context),
    try
        {ok, Id} = m_identity:insert(UserId, email, <<"signup-race@example.com">>, Context),
        ok = m_signup:store_confirm_redirect(UserId, <<"/welcome">>, Context),
        {ok, Key} = m_identity:set_verify_key(Id, Context),
        Parent = self(),
        Ref = make_ref(),
        Confirm = fun() ->
            receive go ->
                Result = m_signup:confirm(Key, z_context:new(zotonic_site_testsandbox)),
                Parent ! {Ref, Result}
            end
        end,
        Workers = [spawn_link(Confirm), spawn_link(Confirm)],
        lists:foreach(fun(Pid) -> Pid ! go end, Workers),
        Results = [receive {Ref, R} -> R after 4000 -> timeout end || _ <- Workers],
        ?assertEqual(1, length([ok || {ok, _} <- Results])),
        ?assertEqual(1, length([error || {error, unknown_key} <- Results]))
    after
        m_rsc:delete(UserId, Context)
    end.

signup_template_test() ->
    Context = context(),
    {ok, _} = application:ensure_all_started(template_compiler),
    Filename = filename:join(code:priv_dir(zotonic_mod_signup), "templates/_signup_with_email_step1.tpl"),
    {ok, Source} = file:read_file(Filename),
    ?assertMatch({ok, _}, template_compiler:compile_binary(Source,
        unicode:characters_to_binary(Filename), [{runtime, z_template_compiler_runtime}], Context)).

context() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    z_acl:sudo(z_context:new(zotonic_site_testsandbox)).

local_url(Page, Context) ->
    z_context:site_url(Page, Context).

identity_props(Id, Context) ->
    z_db:q1("select propb from identity where id = $1", [Id], Context).
