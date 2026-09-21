%% @hidden
-module(z_ids_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

sign_key_test_() ->
    [
        {atom_to_list(Name), {timeout, 15, fun() -> check_sign_key(Name, Length) end}}
        || {Name, Length} <- [{sign_key, 50}, {sign_key_simple, 40}]
    ].

check_sign_key(Name, Length) ->
    Previous = application:get_env(zotonic_core, Name),
    ok = meck:new(m_config, [no_link]),
    ok = meck:new(crypto, [passthrough, no_link]),
    try
        ok = application:unset_env(zotonic_core, Name),
        ok = meck:expect(m_config, get_value, fun(site, _, _) -> undefined end),
        ok = meck:expect(m_config, set_value, fun(site, _, _, _) -> ok end),
        %% Widen the generation window so competing initializers overlap.
        ok = meck:expect(crypto, strong_rand_bytes, fun(N) ->
            timer:sleep(20),
            <<(erlang:unique_integer([positive])):(N * 8)>>
        end),
        Parent = self(),
        Workers = [
            spawn_monitor(fun() ->
                receive go -> ok end,
                Key = z_ids:Name(test_context),
                Parent ! {self(), Key}
            end)
            || _ <- lists:seq(1, 32)
        ],
        [Pid ! go || {Pid, _} <- Workers],
        Keys = try
            [receive_key(Worker) || Worker <- Workers]
        after
            lists:foreach(fun({Pid, Ref}) ->
                exit(Pid, kill),
                erlang:demonitor(Ref, [flush])
            end, Workers)
        end,
        [Key] = lists:usort(Keys),
        ?assertEqual(Length, byte_size(Key)),
        ?assertEqual({ok, Key}, application:get_env(zotonic_core, Name)),
        ?assertEqual(1, meck:num_calls(crypto, strong_rand_bytes, '_')),
        ?assertEqual(1, meck:num_calls(m_config, set_value, [site, Name, Key, test_context])),
        ?assertEqual(Key, z_ids:Name(test_context)),
        %% Site configuration must still take precedence over the fallback.
        ok = meck:expect(m_config, get_value, fun(site, _, _) -> <<"site-key">> end),
        ?assertEqual(<<"site-key">>, z_ids:Name(test_context))
    after
        meck:unload(crypto),
        meck:unload(m_config),
        case Previous of
            undefined -> application:unset_env(zotonic_core, Name);
            {ok, Value} -> application:set_env(zotonic_core, Name, Value)
        end
    end.

receive_key({Pid, Ref}) ->
    receive
        {Pid, Key} ->
            receive
                {'DOWN', Ref, process, Pid, normal} -> Key
            after 1000 ->
                error(worker_exit_timeout)
            end;
        {'DOWN', Ref, process, Pid, Reason} ->
            error({worker_failed, Reason})
    after 10000 ->
        error(worker_timeout)
    end.

-endif.
