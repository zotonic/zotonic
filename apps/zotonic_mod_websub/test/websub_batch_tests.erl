%% @hidden
-module(websub_batch_tests).
-include_lib("eunit/include/eunit.hrl").

batching_test_() -> {timeout, 30, fun batching/0}.

batching() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    {ok, Server} = z_module_manager:whereis(mod_websub, C),
    ok = sys:suspend(Server),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => text, <<"is_published">> => true}, C),
    Topic = m_websub:topic_url(Id, C),
    Callback = <<"https://callback.test/batching">>,
    Saved = [{Key, m_config:get_value(mod_websub, Key, C)}
        || Key <- [push_quiet_seconds, push_deadline_seconds]],
    ok = meck:new(mod_websub, [passthrough]),
    ok = meck:new(z_websub_http, [passthrough]),
    try
        meck:expect(mod_websub, is_editor_active, fun(_, _) -> false end),
        meck:expect(z_websub_http, fetch, fun(post, Url, _, _, _) ->
            {ok, {binary_to_list(Url), [], 0, <<>>}}
        end),
        m_config:delete(mod_websub, push_quiet_seconds, C),
        m_config:delete(mod_websub, push_deadline_seconds, C),
        ?assertEqual({10, 300}, m_websub:push_delays(C)),
        ok = m_websub:update_export(Callback, Topic, Id, 600, undefined, C),
        ok = m_websub:queue_push(Id, 1, C),
        ?assertEqual(10, z_db:q1("select extract(epoch from due-created)::int
            from websub_push_queue where local_rsc_id=$1", [Id], C)),
        ok = m_websub:process_push_queue(C),
        ?assertNot(meck:called(z_websub_http, fetch, ['_', '_', '_', '_', '_'])),

        %% A newer version resets quiet time without resetting the first-change clock.
        z_db:q("update websub_push_queue set created=now()-interval '30 seconds',
            due=now()-interval '1 second' where local_rsc_id=$1", [Id], C),
        Created = z_db:q1("select created from websub_push_queue where local_rsc_id=$1", [Id], C),
        ok = m_websub:queue_push(Id, 2, C),
        ?assertEqual(Created, z_db:q1("select created from websub_push_queue where local_rsc_id=$1", [Id], C)),
        Due = z_db:q1("select due from websub_push_queue where local_rsc_id=$1", [Id], C),
        ok = m_websub:queue_push(Id, 2, C),
        ok = m_websub:queue_push(Id, 1, C),
        ?assertEqual(Due, z_db:q1("select due from websub_push_queue where local_rsc_id=$1", [Id], C)),
        ?assertEqual(1, z_db:q1("select count(*) from websub_push_queue where local_rsc_id=$1", [Id], C)),

        %% Active editing extends quiet time, but the original deadline wins.
        meck:expect(mod_websub, is_editor_active, fun(_, _) -> true end),
        make_due(Id, C),
        ok = m_websub:process_push_queue(C),
        ?assertNot(meck:called(z_websub_http, fetch, ['_', '_', '_', '_', '_'])),
        ?assert(z_db:q1("select due > now() from websub_push_queue where local_rsc_id=$1", [Id], C)),
        z_db:q("update websub_push_queue set created=now()-interval '301 seconds'
            where local_rsc_id=$1", [Id], C),
        ok = m_websub:queue_push(Id, 3, C),
        ok = m_websub:process_push_queue(C),
        ?assert(meck:called(z_websub_http, fetch, [post, Callback, '_', '_', '_'])),
        ?assertEqual(0, z_db:q1("select count(*) from websub_push_queue where local_rsc_id=$1", [Id], C)),
        ?assertEqual(3, z_db:q1("select last_push_version from websub_export where local_rsc_id=$1", [Id], C)),

        ?assertEqual(1, meck:num_calls(z_websub_http, fetch, [post, Callback, '_', '_', '_'])),
        %% Without an active editor, the quiet period alone releases the next batch.
        meck:expect(mod_websub, is_editor_active, fun(_, _) -> false end),
        ok = m_websub:queue_push(Id, 4, C),
        make_due(Id, C),
        ok = m_websub:process_push_queue(C),
        ?assertEqual(4, z_db:q1("select last_push_version from websub_export where local_rsc_id=$1", [Id], C)),
        meck:expect(mod_websub, is_editor_active, fun(_, _) -> true end),

        %% Configuration takes effect for a new batch. Retries keep their backoff.
        m_config:set_value(mod_websub, push_quiet_seconds, <<"30">>, C),
        m_config:set_value(mod_websub, push_deadline_seconds, <<"5">>, C),
        ok = m_websub:queue_push(Id, 5, C),
        ?assertEqual(5, z_db:q1("select extract(epoch from due-created)::int
            from websub_push_queue where local_rsc_id=$1", [Id], C)),
        z_db:q("update websub_push_queue set retry_count=1 where local_rsc_id=$1", [Id], C),
        make_due(Id, C),
        ok = m_websub:process_push_queue(C),
        ?assertEqual(5, z_db:q1("select last_push_version from websub_export where local_rsc_id=$1", [Id], C)),
        m_config:set_value(mod_websub, push_quiet_seconds, <<"invalid">>, C),
        m_config:set_value(mod_websub, push_deadline_seconds, <<"-1">>, C),
        ?assertEqual({10, 300}, m_websub:push_delays(C))
    after
        meck:unload(z_websub_http),
        meck:unload(mod_websub),
        lists:foreach(fun
            ({Key, undefined}) -> m_config:delete(mod_websub, Key, C);
            ({Key, Value}) -> m_config:set_value(mod_websub, Key, Value, C)
        end, Saved),
        m_rsc:delete(Id, C),
        sys:resume(Server)
    end.

presence_test_() -> {timeout, 30, fun presence/0}.

presence() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => text}, C),
    ok = meck:new(z_module_manager, [passthrough]),
    try
        %% The external module need not be installed to test its documented protocol.
        meck:expect(z_module_manager, active, fun
            (mod_presence, _) -> true;
            (Module, Ctx) -> meck:passthrough([Module, Ctx])
        end),
        Msg = #{topic => [<<"presence">>, <<"status">>, <<"mod_admin">>, integer_to_binary(Id)],
            payload => #{<<"status">> => 4, <<"unique_id">> => <<"tab-one">>, <<"user_id">> => 1}},
        BadId = Msg#{topic => [<<"presence">>, <<"status">>, <<"mod_admin">>, <<"2147483648">>]},
        ok = mod_websub:'mqtt:presence/status/mod_admin/+'(BadId, C),
        ok = mod_websub:'mqtt:presence/status/mod_admin/+'(Msg, z_acl:anondo(C)),
        ?assertNot(mod_websub:is_editor_active(Id, C)),
        ok = mod_websub:'mqtt:presence/status/mod_admin/+'(Msg#{retain => true}, C),
        ?assertNot(mod_websub:is_editor_active(Id, C)),
        ok = mod_websub:'mqtt:presence/status/mod_admin/+'(Msg, C),
        ?assert(mod_websub:is_editor_active(Id, C)),
        Other = Msg#{payload => #{<<"status">> => 4, <<"unique_id">> => <<"tab-two">>}},
        ok = mod_websub:'mqtt:presence/status/mod_admin/+'(Other, C),
        Gone = Msg#{payload => #{<<"status">> => 0, <<"unique_id">> => <<"tab-one">>}},
        ok = mod_websub:'mqtt:presence/status/mod_admin/+'(Gone, C),
        ?assert(mod_websub:is_editor_active(Id, C)),
        Idle = Other#{payload => #{<<"status">> => 2, <<"unique_id">> => <<"tab-two">>}},
        ok = mod_websub:'mqtt:presence/status/mod_admin/+'(Idle, C),
        ?assertNot(mod_websub:is_editor_active(Id, C)),
        ok = mod_websub:'mqtt:presence/status/mod_admin/+'(Msg, C),
        timer:sleep(21000),
        ?assertNot(mod_websub:is_editor_active(Id, C)),
        meck:expect(z_module_manager, active, fun
            (mod_presence, _) -> false;
            (Module, Ctx) -> meck:passthrough([Module, Ctx])
        end),
        ok = mod_websub:'mqtt:presence/status/mod_admin/+'(Msg, C),
        ?assertNot(mod_websub:is_editor_active(Id, C))
    after
        meck:unload(z_module_manager),
        m_rsc:delete(Id, C)
    end.

make_due(Id, Context) ->
    z_db:q("update websub_push_queue set due=now() where local_rsc_id=$1", [Id], Context),
    ok.
