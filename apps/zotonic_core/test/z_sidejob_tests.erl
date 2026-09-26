%% @author OpenAI
%% @hidden

-module(z_sidejob_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

-export([notify_when_released/3, notify_when_released/4]).

run_unique_single_execution_test() ->
    Parent = self(),
    Ref = make_ref(),
    RegName = 'sidejob_unique$run_unique_single_execution_test',
    ?assertEqual(undefined, whereis(RegName)),
    First = spawn(fun() ->
        z_sidejob:run_unique(RegName, {?MODULE, notify_when_released, [Parent, Ref, first]})
    end),
    with_job(First, Ref, first, fun() ->
        % Hold the first job until the competing process has actually exited.
        {Second, Monitor} = spawn_monitor(fun() ->
            z_sidejob:run_unique(RegName, {?MODULE, notify_when_released, [Parent, Ref, second]})
        end),
        try
            await_down(Second, Monitor),
            receive
                {Ref, entered, Second} -> error(duplicate_job_started)
            after 0 -> ok
            end
        after
            stop_worker(Second, Monitor)
        end
    end),
    ?assertEqual(undefined, whereis(RegName)).

%% Reproduce CI's mailbox contamination without depending on test ordering.
run_unique_ignores_unrelated_messages_test() ->
    Noise = {entered, self()},
    self() ! Noise,
    try
        run_unique_single_execution_test(),
        receive
            Noise -> ok
        after 0 ->
            error(unrelated_message_consumed)
        end
    after
        receive Noise -> ok after 0 -> ok end
    end.

system_unique_sidejob_test() ->
    ok = ensure_sidejobs(),
    Parent = self(),
    Ref = make_ref(),
    Name = z_sidejob_system_unique_test,
    RegName = 'sidejob_unique$z_sidejob_system_unique_test',
    ?assertEqual(undefined, whereis(RegName)),
    {ok, First} = z_sidejob:start_system_unique(
        Name, ?MODULE, notify_when_released, [Parent, Ref, system_first]),
    with_job(First, Ref, system_first, fun() ->
        ?assertEqual({error, already_running}, z_sidejob:start_system_unique(
            Name, ?MODULE, notify_when_released, [Parent, Ref, duplicate]))
    end),
    ?assertEqual(undefined, whereis(RegName)),
    {ok, Second} = z_sidejob:start_system_unique(
        Name, ?MODULE, notify_when_released, [Parent, Ref, system_second]),
    with_job(Second, Ref, system_second, fun() -> ok end).

site_unique_sidejob_per_site_test() ->
    ok = ensure_sidejobs(),
    Parent = self(),
    Ref = make_ref(),
    Name = z_sidejob_site_unique_test,
    Context1 = z_context:new(zotonic_site_testsandbox),
    Context2 = z_context:new(zotonic_site_testsandbox_other),
    {ok, First} = z_sidejob:start_site_unique(
        Name, ?MODULE, notify_when_released, [Parent, Ref, site_first], Context1),
    with_job(First, Ref, site_first, fun() ->
        ?assertEqual({error, already_running}, z_sidejob:start_site_unique(
            Name, ?MODULE, notify_when_released, [Parent, Ref, duplicate], Context1)),
        {ok, Second} = z_sidejob:start_site_unique(
            Name, ?MODULE, notify_when_released, [Parent, Ref, site_second], Context2),
        with_job(Second, Ref, site_second, fun() -> ok end)
    end).

%% Unique references prevent unrelated test or application messages from being
%% mistaken for job results. The gate removes assumptions about scheduler speed.
notify_when_released(Parent, Ref, Value) ->
    Parent ! {Ref, entered, self()},
    receive
        {Ref, release} -> Parent ! {Ref, done, Value}
    after 5000 ->
        error(job_not_released)
    end,
    ok.

notify_when_released(Parent, Ref, Value, _Context) ->
    notify_when_released(Parent, Ref, Value).

with_job(Pid, Ref, Expected, Check) ->
    Monitor = monitor(process, Pid),
    try
        ?assertEqual(Pid, receive_msg(Ref, entered)),
        Check(),
        Pid ! {Ref, release},
        ?assertEqual(Expected, receive_msg(Ref, done)),
        await_down(Pid, Monitor)
    after
        stop_worker(Pid, Monitor)
    end.

receive_msg(Ref, Event) ->
    receive
        {Ref, Event, Value} -> Value
    after 2000 ->
        error({missing_job_message, Event})
    end.

await_down(Pid, Monitor) ->
    receive
        {'DOWN', Monitor, process, Pid, normal} -> ok;
        {'DOWN', Monitor, process, Pid, Reason} -> error({job_failed, Reason})
    after 2000 ->
        error(job_not_finished)
    end.

stop_worker(Pid, Monitor) ->
    % Kill only this test's worker, never unregister another running process.
    CleanupMonitor = monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', CleanupMonitor, process, Pid, _} -> ok
    after 2000 ->
        error(worker_not_stopped)
    end,
    demonitor(Monitor, [flush]),
    ok.

ensure_sidejobs() ->
    case application:ensure_all_started(sidejob) of
        {ok, _} -> ok;
        {error, {already_started, sidejob}} -> ok
    end,
    case catch z_sidejob:usage() of
        {'EXIT', _} ->
            _ = z_sidejob:init(),
            ok;
        _ ->
            ok
    end.
