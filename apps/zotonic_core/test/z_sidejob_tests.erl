%% @author OpenAI
%% @hidden

-module(z_sidejob_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

-export([
    notify_after/3,
    notify_after/4
]).


run_unique_single_execution_test() ->
    Self = self(),
    RegName = 'sidejob_unique$run_unique_single_execution_test',
    unregister_if(RegName),
    spawn(fun() ->
        z_sidejob:run_unique(RegName, {?MODULE, notify_after, [Self, first, 75]})
    end),
    timer:sleep(10),
    spawn(fun() ->
        z_sidejob:run_unique(RegName, {?MODULE, notify_after, [Self, second, 0]})
    end),
    ?assertEqual(first, receive_msg()),
    ?assertEqual(timeout, receive_msg(100)),
    ok.

system_unique_sidejob_test() ->
    ok = ensure_sidejobs(),
    Self = self(),
    Name = z_sidejob_system_unique_test,
    RegName = 'sidejob_unique$z_sidejob_system_unique_test',
    unregister_if(RegName),
    {ok, _Pid} = z_sidejob:start_system_unique(Name, ?MODULE, notify_after, [Self, system_first, 100]),
    ok = wait_until_registered(RegName),
    ?assertEqual(
        {error, already_running},
        z_sidejob:start_system_unique(Name, ?MODULE, notify_after, [Self, system_second, 0])),
    ?assertEqual(system_first, receive_msg()),
    ok = wait_until_unregistered(RegName),
    {ok, _Pid2} = z_sidejob:start_system_unique(Name, ?MODULE, notify_after, [Self, system_second, 0]),
    ?assertEqual(system_second, receive_msg()),
    ok.

site_unique_sidejob_per_site_test() ->
    ok = ensure_sidejobs(),
    Self = self(),
    Name = z_sidejob_site_unique_test,
    Context1 = z_context:new(zotonic_site_testsandbox),
    Context2 = z_context:new(zotonic_site_testsandbox_other),
    RegName1 = 'sidejob_unique$z_sidejob_site_unique_test$zotonic_site_testsandbox',
    RegName2 = 'sidejob_unique$z_sidejob_site_unique_test$zotonic_site_testsandbox_other',
    unregister_if(RegName1),
    unregister_if(RegName2),
    {ok, _Pid1} = z_sidejob:start_site_unique(Name, ?MODULE, notify_after, [Self, site_first, 50], Context1),
    {ok, _Pid2} = z_sidejob:start_site_unique(Name, ?MODULE, notify_after, [Self, site_second, 50], Context2),
    Msgs = [ receive_msg(), receive_msg() ],
    ?assert(lists:member(site_first, Msgs)),
    ?assert(lists:member(site_second, Msgs)),
    ok.


notify_after(Pid, Msg, Delay) ->
    timer:sleep(Delay),
    Pid ! Msg,
    ok.

notify_after(Pid, Msg, Delay, _Context) ->
    notify_after(Pid, Msg, Delay).


ensure_sidejobs() ->
    case application:ensure_all_started(sidejob) of
        {ok, _} ->
            ok;
        {error, {already_started, sidejob}} ->
            ok
    end,
    case catch z_sidejob:usage() of
        {'EXIT', _} ->
            _ = z_sidejob:init(),
            ok;
        _ ->
            ok
    end.

wait_until_registered(RegName) ->
    wait_until(fun() -> erlang:whereis(RegName) =/= undefined end, 50).

wait_until_unregistered(RegName) ->
    wait_until(fun() -> erlang:whereis(RegName) =:= undefined end, 50).

wait_until(Fun, 0) ->
    ?assert(Fun()),
    ok;
wait_until(Fun, N) ->
    case Fun() of
        true ->
            ok;
        false ->
            timer:sleep(10),
            wait_until(Fun, N - 1)
    end.

receive_msg() ->
    receive_msg(1000).

receive_msg(Timeout) ->
    receive
        Msg ->
            Msg
    after Timeout ->
        timeout
    end.

unregister_if(RegName) ->
    case erlang:whereis(RegName) of
        undefined ->
            ok;
        _Pid ->
            catch erlang:unregister(RegName),
            ok
    end.
