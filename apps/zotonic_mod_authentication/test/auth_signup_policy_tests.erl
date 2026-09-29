-module(auth_signup_policy_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

pending_signup_policy_test() ->
    Modules = [m_identity, z_notifier],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    try
        meck:expect(m_identity, lookup_by_type_and_key, fun(_, _, _) -> undefined end),
        meck:expect(m_identity, insert, fun(_, _, _, _, _) -> {ok, 123} end),
        meck:expect(m_identity, ensure_username_pw, fun(_, _) -> ok end),
        Auth = #auth_validated{
            service = test_service,
            service_uid = <<"provider:subject">>,
            is_signup_confirmed = true
        },
        lists:foreach(
            fun({Original, Current, ExpectedCalls}) ->
                meck:reset(m_identity),
                meck:expect(z_notifier, first, fun
                    (#signup{}, _) -> {ok, 42};
                    (#auth_ensure_username_pw{service = test_service, service_uid = <<"provider:subject">>}, _) -> Current
                end),
                ?assertEqual({ok, 42}, mod_authentication:observe_auth_validated(
                    Auth#auth_validated{ensure_username_pw = Original}, #context{})),
                ?assertEqual(ExpectedCalls, meck:num_calls(m_identity, ensure_username_pw, '_'))
            end,
            [{true, false, 0}, {false, true, 1}, {true, undefined, 1}, {false, undefined, 0}]),
        meck:reset(m_identity),
        ?assertEqual({ok, undefined}, mod_authentication:observe_auth_validated(
            Auth#auth_validated{is_connect = true}, #context{})),
        ?assertEqual(0, meck:num_calls(m_identity, ensure_username_pw, '_'))
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.
