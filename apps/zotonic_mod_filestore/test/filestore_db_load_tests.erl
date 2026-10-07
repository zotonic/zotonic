%% @doc Busy pools defer queue polling and already queued upload sidejobs.
-module(filestore_db_load_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

busy_pool_test() ->
    Modules = [z_db_pool, z_sidejob, m_filestore, filestore_config],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    try
        meck:expect(z_db_pool, is_low_load, fun(_) -> false end),
        meck:expect(z_sidejob, space, fun() -> 100 end),
        meck:expect(m_filestore, fetch_queue, fun(_, _) -> error(queue_polled) end),
        meck:expect(m_filestore, fetch_move_to_local, fun(_, _) -> error(queue_polled) end),
        meck:expect(m_filestore, fetch_deleted, fun(_, _, _) -> error(queue_polled) end),
        meck:expect(m_filestore, lookup, fun(_, _) -> error(upload_started) end),
        meck:expect(m_filestore, dequeue, fun(_, _) -> error(queue_entry_removed) end),
        Context = #context{site = zotonic_site_testsandbox},
        ?assertEqual({error, busy}, mod_filestore:next_batch(10, Context)),
        ?assertEqual({error, busy}, filestore_uploader:upload_job(
            1, <<"archive/test.jpg">>, {error, enoent}, #{}, Context)),
        ?assertEqual([], meck:history(m_filestore)),
        % The next tick resumes queue polling after foreground load drops.
        meck:expect(z_db_pool, is_low_load, fun(_) -> true end),
        meck:expect(filestore_config, is_upload_enabled, fun(_) -> true end),
        meck:expect(filestore_config, delete_interval, fun(_) -> <<"false">> end),
        meck:expect(m_filestore, fetch_queue, fun(_, _) -> {ok, []} end),
        meck:expect(m_filestore, fetch_move_to_local, fun(_, _) -> {ok, []} end),
        ?assertEqual(ok, mod_filestore:next_batch(10, Context)),
        ?assertEqual(1, meck:num_calls(m_filestore, fetch_queue, '_')),
        ?assertEqual(1, meck:num_calls(m_filestore, fetch_move_to_local, '_')),
        lists:foreach(fun(M) -> ?assert(meck:validate(M)) end, Modules)
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

batch_sidejob_test_() ->
    {timeout, 15, fun batch_sidejob/0}.

batch_sidejob() ->
    {ok, _} = application:ensure_all_started(sidejob),
    case catch z_sidejob:usage() of
        {'EXIT', _} -> z_sidejob:init();
        _ -> ok
    end,
    Modules = [z_db_pool, m_filestore, filestore_config],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    Parent = self(),
    Context1 = #context{site = filestore_batch_test},
    Context2 = #context{site = filestore_batch_other_test},
    {ok, Server1} = gen_server:start(mod_filestore, [{context, Context1}], []),
    {ok, Server2} = gen_server:start(mod_filestore, [{context, Context2}], []),
    try
        meck:expect(z_db_pool, is_low_load, fun(_) -> true end),
        meck:expect(filestore_config, is_upload_enabled, fun(_) -> true end),
        meck:expect(filestore_config, delete_interval, fun(_) -> <<"false">> end),
        meck:expect(m_filestore, fetch_move_to_local, fun(_, _) -> {ok, []} end),
        meck:expect(m_filestore, fetch_queue, fun(_, Context) ->
            Parent ! {batch_started, z_context:site(Context), self()},
            receive
                crash -> error(batch_failed);
                finish -> {ok, []}
            after 5000 -> error(batch_not_released)
            end
        end),
        gen_server:cast(Server1, next_batch),
        Worker1 = batch_started(filestore_batch_test),
        ?assertNotEqual(Server1, Worker1),
        % The server remains responsive while the batch is blocked. A repeated
        % tick cannot start another batch, but another site can run independently.
        gen_server:cast(Server1, next_batch),
        ?assertMatch({ok, _}, gen_server:call(Server1, batch_size)),
        gen_server:cast(Server2, next_batch),
        Worker2 = batch_started(filestore_batch_other_test),
        receive
            {batch_started, filestore_batch_test, _} -> error(duplicate_batch_started)
        after 0 -> ok
        end,
        ?assertMatch({batch_failed, _}, finish_batch(Worker1, crash)),
        ?assertMatch({ok, _}, gen_server:call(Server1, batch_size)),
        % A failed batch releases its unique name and the next tick can retry.
        gen_server:cast(Server1, next_batch),
        Worker3 = batch_started(filestore_batch_test),
        ?assertNotEqual(Worker1, Worker3),
        ?assertEqual(normal, finish_batch(Worker3, finish)),
        ?assertEqual(normal, finish_batch(Worker2, finish)),
        ?assertMatch({ok, _}, gen_server:call(Server1, batch_size))
    after
        gen_server:stop(Server1),
        gen_server:stop(Server2),
        lists:foreach(fun(Context) ->
            Name = z_utils:name_for_site('sidejob_unique$mod_filestore_next_batch', Context),
            case whereis(Name) of
                undefined -> ok;
                Pid -> finish_batch(Pid, kill)
            end
        end, [Context1, Context2]),
        lists:foreach(fun meck:unload/1, Modules)
    end.

batch_started(Site) ->
    receive
        {batch_started, Site, Pid} -> Pid
    after 2000 -> error(batch_not_started)
    end.

finish_batch(Pid, Action) ->
    Ref = monitor(process, Pid),
    case Action of
        kill -> exit(Pid, kill);
        _ -> Pid ! Action
    end,
    receive
        {'DOWN', Ref, process, Pid, Reason} -> Reason
    after 2000 -> error(batch_not_finished)
    end.
