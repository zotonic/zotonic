%% @hidden
-module(z_ids_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

sign_key_test_() ->
    [
        {atom_to_list(Name) ++ ": " ++ Description,
            {timeout, 15, fun() -> check_sign_key(Name, Length, InitialValue) end}}
        || {Name, Length} <- [{sign_key, 50}, {sign_key_simple, 40}],
           {Description, InitialValue} <- [{"empty", <<>>}, {"missing", undefined}]
    ].

check_sign_key(Name, Length, InitialValue) ->
    Previous = application:get_env(zotonic_core, Name),
    Config = ets:new(sign_key_config, [public, set]),
    ets:insert(Config, {Name, InitialValue}),
    ok = meck:new(m_config, [no_link]),
    ok = meck:new(crypto, [passthrough, no_link]),
    try
        ok = application:unset_env(zotonic_core, Name),
        ok = meck:expect(m_config, get_value, fun(site, KeyName, _) ->
            ets:lookup_element(Config, KeyName, 2)
        end),
        ok = meck:expect(m_config, set_value, fun(site, KeyName, Value, _) ->
            ets:insert(Config, {KeyName, Value}),
            ok
        end),
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
        ?assertEqual(Key, ets:lookup_element(Config, Name, 2)),
        ?assertEqual(1, meck:num_calls(crypto, strong_rand_bytes, '_')),
        ?assertEqual(1, meck:num_calls(m_config, set_value, [site, Name, Key, test_context])),
        ?assertEqual(Key, z_ids:Name(test_context)),
        %% Site configuration must still take precedence over the fallback.
        ok = application:set_env(zotonic_core, Name, <<"fallback-key">>),
        ets:insert(Config, {Name, <<"site-key">>}),
        ?assertEqual(<<"site-key">>, z_ids:Name(test_context))
    after
        meck:unload(crypto),
        meck:unload(m_config),
        ets:delete(Config),
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
