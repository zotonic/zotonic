%% @hidden
-module(websub_scheduler_tests).
-include_lib("eunit/include/eunit.hrl").

scheduler_test_() ->
    {timeout, 30, fun scheduler/0}.

scheduler() ->
    Context = z_context:new(zotonic_site_testsandbox_scheduler),
    Parent = self(),
    Tag = make_ref(),
    ok = meck:new(z_sidejob, [passthrough]),
    meck:expect(z_sidejob, start_site_unique,
        fun(Name, Module, Function, Args, Ctx) ->
            case z_context:site(Ctx) of
                zotonic_site_testsandbox_scheduler ->
                    [Server] = Args,
                    Worker = spawn(fun() -> fake_worker(Server) end),
                    Parent ! {Tag, started, Worker},
                    {ok, Worker};
                _ -> meck:passthrough([Name, Module, Function, Args, Ctx])
            end
        end),
    {ok, Server} = mod_websub:start_link([{context, Context}]),
    try
        %% Startup reconciles once; idle fast ticks never start another worker.
        poll(Server),
        First = started(Tag),
        finish(Server, First, undefined),
        lists:foreach(fun(_) -> poll(Server) end, lists:seq(1, 10)),
        assert_no_start(Tag),

        %% A signal during a batch must survive its completion report.
        gen_server:cast(Server, {websub_queue_changed, Context}),
        poll(Server),
        Second = started(Tag),
        gen_server:cast(Server, {websub_queue_changed, Context}),
        poll(Server),
        assert_no_start(Tag),
        finish(Server, Second, undefined),
        poll(Server),
        Third = started(Tag),
        finish(Server, Third, undefined),

        %% Slow reconciliation catches work even without an enqueue signal.
        gen_server:cast(Server, slow_poll),
        Fourth = started(Tag),
        %% A future due time avoids polling until it expires.
        finish(Server, Fourth, erlang:monotonic_time(second) + 3600),
        poll(Server),
        assert_no_start(Tag),
        gen_server:cast(Server, slow_poll),
        Fifth = started(Tag),
        %% A due retry or remaining batch runs on the next fast tick.
        finish(Server, Fifth, erlang:monotonic_time(second)),
        poll(Server),
        Sixth = started(Tag),

        %% A crash before reporting completion retains pending work.
        exit(Sixth, kill),
        await_idle(Server),
        poll(Server),
        Seventh = started(Tag),
        finish(Server, Seventh, undefined),

        %% Overload must retain the signal for a later attempt.
        meck:expect(z_sidejob, start_site_unique, fun(Name, Module, Function, Args, Ctx) ->
            case z_context:site(Ctx) of
                zotonic_site_testsandbox_scheduler ->
                    Parent ! {Tag, overloaded},
                    {error, overload};
                _ -> meck:passthrough([Name, Module, Function, Args, Ctx])
            end
        end),
        gen_server:cast(Server, {websub_queue_changed, Context}),
        poll(Server),
        receive {Tag, overloaded} -> ok after 1000 -> error(no_attempt) end,
        poll(Server),
        receive {Tag, overloaded} -> ok after 1000 -> error(lost_wakeup) end
    after
        gen_server:stop(Server),
        meck:unload(z_sidejob)
    end.

%% The notifier must deliver only committed queue signals.
queue_signal_transaction_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    z_notifier:observe(websub_queue_changed, self(), Context),
    try
        ok = z_db:transaction(fun(Ctx) ->
            ok = mod_websub:queue_changed(Ctx),
            assert_no_signal(),
            ok
        end, Context),
        receive
            {'$gen_cast', {websub_queue_changed, _}} -> ok
        after 1000 -> error(missing_commit_signal)
        end,
        _ = z_db:transaction(fun(Ctx) ->
            ok = mod_websub:queue_changed(Ctx),
            throw(rollback)
        end, Context),
        assert_no_signal()
    after
        z_notifier:detach(websub_queue_changed, self(), Context)
    end.

fake_worker(Server) ->
    receive
        {complete, NextDue} -> Server ! {queues_checked, self(), NextDue}
    after 5000 -> exit(test_worker_timeout)
    end.

poll(Server) ->
    mod_websub:pid_observe_tick_1s(Server, tick_1s, undefined),
    %% Same-sender ordering makes the call a barrier for the tick cast.
    ?assertEqual({error, unknown_call}, gen_server:call(Server, barrier)).

started(Tag) ->
    receive {Tag, started, Worker} -> Worker
    after 1000 -> error(worker_not_started)
    end.

assert_no_start(Tag) ->
    receive {Tag, started, _} -> error(unexpected_worker)
    after 0 -> ok
    end.

finish(Server, Worker, NextDue) ->
    Worker ! {complete, NextDue},
    await_idle(Server).

await_idle(Server) ->
    await_idle(Server, erlang:monotonic_time(millisecond) + 2000).

await_idle(Server, Deadline) ->
    %% A state read ensures preceding worker messages have been handled before
    %% inspecting monitors. No assumption about worker scheduling speed is needed.
    _ = sys:get_state(Server),
    case process_info(Server, monitors) of
        {monitors, []} ->
            ?assertEqual({error, unknown_call}, gen_server:call(Server, barrier));
        _ ->
            ?assert(erlang:monotonic_time(millisecond) < Deadline),
            timer:sleep(10),
            await_idle(Server, Deadline)
    end.

assert_no_signal() ->
    receive {'$gen_cast', {websub_queue_changed, _}} -> error(unexpected_signal)
    after 0 -> ok
    end.
