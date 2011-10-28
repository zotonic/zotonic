%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Supervisor with a one_for_one strategy and disabling of too-often-crashing resources.
%% All children of this supervisor should be gen_server/supervisor processes.

%% Copyright 2010 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(z_supervisor).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

% gen_server exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% z_supervisor API
-export([
    start_link/1,
    start_link/2,
    add_child/2,
    add_child_async/2,
    delete_child/2,
    start_child/2,
    start_child/3,
    stop_child/2,
    restart_child/2,
    which_children/1,
    running_children/1,
    check_children/1,
    set_manager_pid/2
]).

% -record(supervised, {name, mfa, status, pid, crashes = 5, period = 60, period_retry = 1800, period_retries=10}).

-define(INTERVAL, 1000).
-record(state, {waiting=[], running=[], retrying=[], failed=[], stopped=[], timer_ref, manager_pid}).

-record(child_state, {name, pid,
                      state=waiting, time,
                      crash_time, crashes=0, 
                      retry_time, retries=0, 
                      fail_time,
                      child}).

-include_lib("zotonic.hrl").

%%% ---------------------------------------------------
%%% This is a general process supervisor built upon gen_server.erl.
%%% Servers/processes should/could also be built using gen_server.erl.
%%% SupName = {local, atom()} | {global, atom()}.
%%% ---------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

start_link(SupName, Args) ->
    gen_server:start_link(SupName, ?MODULE, Args, []).


%% @doc Add a child, the child will be added and started.
add_child(Pid, ChildSpec) ->
    gen_server:call(Pid, {add_child, ChildSpec}).

add_child_async(Pid, ChildSpec) ->
    gen_server:cast(Pid, {add_child, ChildSpec}).

%% @doc Delete a child, the child will be terminated and removed.
delete_child(Pid, Name) ->
    gen_server:cast(Pid, {delete_child, Name}).

%% @doc Start a child when it is not running (either failed or stopped)
start_child(Pid, Name) ->
    gen_server:call(Pid, {start_child, Name}).

%% @doc Start a child when it is not running (either failed or stopped)
start_child(Pid, Name, Timeout) ->
    gen_server:call(Pid, {start_child, Name}, Timeout).

%% @doc Stop a child, the child will be terminated and put in "stopped" state
stop_child(Pid, Name) ->
    gen_server:cast(Pid, {stop_child, Name}).

%% @doc Terminate and restart a child.
restart_child(Pid, Name) ->
    gen_server:call(Pid, {restart_child, Name}).

%% @doc Return the list of all children and their run state.
which_children(Pid) ->
    gen_server:call(Pid, which_children).

%% @doc Return the list of running children
running_children(Pid) ->
    gen_server:call(Pid, running_children).

%% @doc Check children, try restarting children when they are in 'error' state.
check_children(Pid) ->
    gen_server:cast(Pid, check_children).


%% @doc Set the manager pid for this supervisor
set_manager_pid(Pid, ManagerPid) ->
    gen_server:cast(Pid, {set_manager_pid, ManagerPid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(InitialChildren) ->
    process_flag(trap_exit, true),
    {ok, TimerRef} = timer:apply_interval(?INTERVAL, ?MODULE, check_children, [self()]),
    {ok, #state{
            waiting=[ #child_state{name=C#child_spec.name, child=C, state=starting, time=erlang:localtime()}
                        || C <- InitialChildren ], 
            timer_ref=TimerRef
        }
    }.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Add a child in the stopped state.
handle_call({add_child, ChildSpec}, _From, State) ->
    case exists(ChildSpec#child_spec.name, State) of
        false ->
            CS = #child_state{name=ChildSpec#child_spec.name, child=ChildSpec, state=starting, time=erlang:localtime()},
            {reply, ok, State#state{stopped=[CS|State#state.stopped]}};
        true ->
            {reply, {error, duplicate_name}, State}
    end;

%% @doc Start the child when it is not running already
handle_call({start_child, Name}, _From, State) ->
    case find_running(Name, State) of
        {ok, Pid} ->
            {reply, {ok, Pid}, State};
        error ->
            case do_remove_child(Name, State) of
                {CS, State1} -> 
                    State2 = do_start_child(CS, State1),
                    case find_running(Name, State2) of
                        {ok, Pid} ->
                            {reply, {ok, Pid}, State2};
                        error ->
                            {reply, {error, not_started}, State2}
                    end;
                error -> 
                    {reply, {error, unknown_child}, State}
            end
    end;

%% @doc Restart or start a child.
handle_call({restart_child, Name}, _From, State) ->
    case do_remove_child(Name, State) of
        {CS,State1} ->
            shutdown_child(CS, State),
            {reply, ok, do_start_child(CS, State1)};
        error ->
            %% Unknown child
            {reply, {error, unknown_child}, State}
    end;

%% @doc Return a full list of all children
handle_call(which_children, _From, State) ->
    F = fun(C) ->
        {C#child_state.name, #child_state.child, C#child_state.pid, C#child_state.time} 
    end,
    {reply, [
            {waiting, [ F(C) || C <- State#state.waiting]},
            {running, [ F(C) || C <- State#state.running]},
            {retrying, [ F(C) || C <- State#state.retrying]},
            {failed, [ F(C) || C <- State#state.failed]},
            {stopped, [ F(C) || C <- State#state.stopped]}
        ], State};

%% @doc Return the list of running children
handle_call(running_children, _From, State) ->
    {reply, [ C#child_state.name || C <- State#state.running ], State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    ?DEBUG({unknown_call, Message}),
    {stop, {unknown_call, Message}, State}.


% @doc Async version of the handle_calls({add_child, ...}, ...) above
handle_cast({add_child, ChildSpec}, State) ->
    case exists(ChildSpec#child_spec.name, State) of
        false ->
            CS = #child_state{name=ChildSpec#child_spec.name, child=ChildSpec, state=starting, time=erlang:localtime()},
            {noreply, State#state{stopped=[CS|State#state.stopped]}};
        true ->
            {noreply, State}
    end;

%% @doc Stop a child process and add it to the stopped list.
handle_cast({stop_child, Name}, State) ->
    case do_remove_child(Name, State) of
        {CS,State1} ->
            shutdown_child(CS, State1),
            CS1 = CS#child_state{state=stopped, time=erlang:localtime(), pid=undefined},
            {noreply, State1#state{stopped=[CS1|State1#state.stopped]}};
        error ->
            %% Unknown child
            {noreply, State}
    end;

%% @doc Delete a child and add remove it from any queue, optionally stopping it.
handle_cast({delete_child, Name}, State) ->
    case do_remove_child(Name, State) of
        {CS,State1} ->
            shutdown_child(CS, State1),
            {noreply, State1};
        error ->
            %% Unknown child
            {noreply, State}
    end;

%% @doc Start any children that are waiting or up for a retry.
handle_cast(check_children, State) ->
    State1 = handle_waiting_children(State),
    State2 = handle_retrying_children(State1),
    State3 = handle_failed_children(State2),
    z_utils:flush_message({'$gen_cast', check_children}),
    {noreply, State3};

%% @doc Set the manager pid of this supervisor
handle_cast({set_manager_pid, Pid}, State) ->
    {noreply, State#state{manager_pid=Pid}};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    ?DEBUG({unknown_cast, Message}),
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handle the exit of a child
handle_info({'EXIT', Pid, Reason}, State) ->
    {noreply, handle_exit(Pid, Reason, State)};

%% @doc Handling all non call/cast messages
handle_info(Info, State) ->
    ?DEBUG({unknown_info, Info}),
    {noreply, State}.


%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


%% @doc Start all waiting children and add them to the 'running' state.
handle_waiting_children(#state{waiting=[]} = State) ->
    State;
handle_waiting_children(#state{waiting=Waiting} = State) ->
    lists:foldl(fun(C, S) -> do_start_child(C, S) end, State#state{waiting=[]}, Waiting).

%% @doc Restart all retrying children add them to the 'running' state.
%% Repeat until the retry queue is empty.
handle_retrying_children(#state{retrying=[]} = State) ->
    State;
handle_retrying_children(#state{retrying=Retrying} = State) ->
    Now = z_utils:now(),
    {Start,Wait} = lists:partition(fun(CS) -> is_ready_for_retry(CS, Now) end, Retrying),
    lists:foldl(fun(#child_state{child=Child} = CS, S) ->
                    case start_child_mfa(Child#child_spec.mfa) of
                        {ok, Pid} ->
                            CS1 = CS#child_state{state=running_from_retry, pid=Pid, time=erlang:localtime()},
                            S#state{running=[CS1|S#state.running]};
                        {error, _What} ->
                            % Move the child to the failed state when it crashed too often
                            case CS#child_state.retries >= Child#child_spec.period_retries of
                                true ->
                                    CS1 = CS#child_state{state=failed, time=erlang:localtime(), fail_time=Now},
                                    S#state{failed=[CS1|S#state.failed]};
                                false ->
                                    CS1 = CS#child_state{retries=CS#child_state.retries+1, retry_time=Now},
                                    S#state{retrying=[CS1|S#state.retrying]}
                            end
                    end
                end,
                State#state{retrying=Wait}, Start).

    is_ready_for_retry(#child_state{retry_time=RetryTime, child=Child}, Now) ->
        Now - RetryTime > Child#child_spec.period_retry.

%% @doc Period check if any failed children are up for a restart.
handle_failed_children(#state{failed=[]} = State) ->
    State;
handle_failed_children(#state{failed=Failed} = State) ->
    Now = z_utils:now(),
    {Start,Fail} = lists:partition(fun(CS) -> is_ready_for_unfail(CS, Now) end, Failed),
    lists:foldl(fun(#child_state{child=Child} = CS, S) ->
                    case start_child_mfa(Child#child_spec.mfa) of
                        {ok, Pid} ->
                            CS1 = CS#child_state{state=running_from_failed, pid=Pid, time=erlang:localtime()},
                            S#state{running=[CS1|S#state.running]};
                        {error, _What} ->
                            CS1 = CS#child_state{state=failed, time=erlang:localtime(), fail_time=Now},
                            S#state{failed=[CS1|S#state.failed]}
                    end
                end,
                State#state{failed=Fail}, Start).

    is_ready_for_unfail(#child_state{fail_time=FailTime, child=Child}, Now) ->
        Now - FailTime > Child#child_spec.eternal_retry.


%% @doc Handle an 'EXIT' message for a Pid
handle_exit(Pid, Reason, State) ->
    case remove_running_pid(Pid, State) of
        {CS, State1} ->
            case Reason of
                normal -> append_stopped(CS, State1);
                shutdown -> append_stopped(CS, State1);
                _Other -> do_maybe_restart(CS, State1)
            end;
        error ->
            %% No child with this pid in the running list, ignore the exit
            State
    end.


%% @doc Start a single child.  Doesn't remove it from any queue (caller should have done that).
do_start_child(#child_state{child=Child} = CS, State) ->
    #child_spec{mfa=MFA} = Child,
    case start_child_mfa(MFA) of
        {ok, Pid} ->
            CS1 = CS#child_state{state=running, pid=Pid, time=erlang:localtime()},
            notify_start(CS1, State),
            State#state{running=[CS1|State#state.running]};
        {error, _What} ->
            do_maybe_restart(CS, State)
    end.


do_maybe_restart(CS, State) ->
    case may_restart(CS) of
        first_restart ->
            CS1 = CS#child_state{state=crashed, time=erlang:localtime(), 
                            crashes=1, crash_time=z_utils:now(), retries=0},
            do_start_child(CS1, State);
        restart ->
            CS1 = CS#child_state{state=crashed, time=erlang:localtime(),
                            crashes=CS#child_state.crashes+1, retries=0},
            do_start_child(CS1, State);
        first_retry ->
            CS1 = CS#child_state{state=retrying, time=erlang:localtime(),
                            retries=1, retry_time=z_utils:now()},
            State#state{retrying=[CS1|State#state.retrying]};
        retry ->
            CS1 = CS#child_state{state=retrying, time=erlang:localtime(),
                            retries=CS#child_state.retries+1, retry_time=z_utils:now()},
            State#state{retrying=[CS1|State#state.retrying]};
        fail ->
            CS1 = CS#child_state{state=failed, time=erlang:localtime(), fail_time=z_utils:now()},
            State#state{failed=[CS1|State#state.failed]}
    end.


%% @doc Remove the child with Name from any list of children
do_remove_child(Name, State) ->
    case remove_by_name(Name, State#state.waiting, []) of
        {CS, L} -> {CS, State#state{waiting=L}};
        error ->
            case remove_by_name(Name, State#state.running, []) of
                {CS1, L1} -> {CS1, State#state{running=L1}};
                error ->
                    case remove_by_name(Name, State#state.retrying, []) of
                        {CS2, L2} -> {CS2, State#state{retrying=L2}};
                        error ->
                            case remove_by_name(Name, State#state.failed, []) of
                                {CS3, L3} -> {CS3, State#state{failed=L3}};
                                error ->
                                    case remove_by_name(Name, State#state.stopped, []) of
                                        {CS4, L4} -> {CS4, State#state{stopped=L4}};
                                        error -> error
                                    end
                            end
                    end
            end
    end.


%% @doc Start a single child, catch exceptions
start_child_mfa({M,F,A}) ->
    case catch apply(M, F, A) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, Pid};
        ignore -> 
            {ok, undefined};
        {error, What} ->
            {error, What};
        What ->
            {error, What}
    end.


%% @doc Check if a child is not crashing or retrying too often.
%% @spec may_restart(#child_state{}) -> first_restart | restart | first_retry | retry | fail
may_restart(#child_state{state=running_from_fail, fail_time=FailTime, child=Child} = CS) ->
    #child_spec{eternal_retry=Period} = Child,
    case z_utils:now() - FailTime > Period of
        true ->
            %% Did run for longer than a (normal) retry period, reset to normal restarting behaviour
            may_restart(CS#child_state{state=running});
        false ->
            %% A restart within the retry/fail period, maybe queue for another eternal retry
            fail
    end;
may_restart(#child_state{state=running_from_retry, retry_time=RetryTime, retries=Retries, child=Child} = CS) ->
    #child_spec{period_retries=MaxRetries, period_retry=Period} = Child,
    case z_utils:now() - RetryTime > Period of
        true ->
            %% Did run for longer than a retry period, reset to new restarting behaviour
            may_restart(CS#child_state{state=running});
        false ->
            %% A restart within the retry period, maybe queue for another retry
            case Retries > MaxRetries of
                true -> fail;
                false -> retry
            end
    end;
may_restart(#child_state{crash_time=undefined}) ->
    first_restart;
may_restart(#child_state{crash_time=CrashTime, crashes=Crashes, child=Child}) ->
    #child_spec{period=Period, crashes=MaxCrashes} = Child,
    case z_utils:now() - CrashTime  > Period of
        true ->
            first_restart;
        false ->
            case Crashes > MaxCrashes of
                true -> first_retry;
                false -> restart
            end
    end.


%% @doc Append a child to the stopped queue
append_stopped(CS, State) ->
    CS1 = CS#child_state{state=stopped, time=erlang:localtime()},
    State#state{stopped=[CS1|State#state.stopped]}.


%% @doc Remove a child from the running list
remove_running_pid(Pid, State) ->
    case remove_by_pid(Pid, State#state.running, []) of
        {CS, Running} -> 
            notify_exit(CS, State),
            {CS#child_state{pid=undefined}, State#state{running=Running}};
        error ->
            error
    end.


remove_by_name(_Name, [], _Acc) ->
    error;
remove_by_name(Name, [#child_state{name=Name} = CS|Rest], Acc) ->
    {CS, Rest++Acc};
remove_by_name(Name, [CS|Rest], Acc) ->
    remove_by_name(Name, Rest, [CS|Acc]).


remove_by_pid(_Pid, [], _Acc) ->
    error;
remove_by_pid(Pid, [#child_state{pid=Pid} = CS|Rest], Acc) ->
    {CS, Rest++Acc};
remove_by_pid(Pid, [CS|Rest], Acc) ->
    remove_by_pid(Pid, Rest, [CS|Acc]).


%% @doc Find the pid() of a running child
find_running(Name, State) ->
    find_running1(Name, State#state.running).
    
    find_running1(_Name, []) -> error;
    find_running1(Name, [#child_state{name=Name, pid=Pid}|_]) -> {ok, Pid};
    find_running1(Name, [_|Running]) -> find_running1(Name, Running).

%% @doc Check if a named child exists
exists(Name, State) ->
    is_member(Name, State#state.waiting)
    orelse is_member(Name, State#state.running)
    orelse is_member(Name, State#state.retrying)
    orelse is_member(Name, State#state.failed)
    orelse is_member(Name, State#state.stopped).


is_member(_Name, []) -> false;
is_member(Name, [#child_state{name=Name}|_]) -> true;
is_member(Name, [_|Rest]) -> is_member(Name, Rest).



%% @doc Kill the child process when it is running
shutdown_child(#child_state{pid=Pid, child=Child} = CS, State) when is_pid(Pid) ->
    notify_exit(CS, State),
    shutdown(Pid, Child#child_spec.shutdown);
shutdown_child(_, _State) ->
    {error, no_process}.


%% @doc Notify the manager that a child has stopped
notify_exit(_ChildState, #state{manager_pid=undefined}) ->
    nop;
notify_exit(ChildState, #state{manager_pid=ManagerPid}) ->
    gen_server:cast(ManagerPid, {supervisor_child_stopped, ChildState#child_state.child, ChildState#child_state.pid}).


%% @doc Notify the manager that a child has started
notify_start(_ChildState, #state{manager_pid=undefined}) ->
    nop;
notify_start(ChildState, #state{manager_pid=ManagerPid}) ->
    gen_server:cast(ManagerPid, {supervisor_child_started, ChildState#child_state.child, ChildState#child_state.pid}).


%%-----------------------------------------------------------------
%% shutdown/2 and monitor_child/1 are from supervisor.erl
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
%% 
%% Shutdowns a child. We must check the EXIT value 
%% of the child, because it might have died with another reason than
%% the wanted. In that case we want to report the error. We put a 
%% monitor on the child an check for the 'DOWN' message instead of 
%% checking for the 'EXIT' message, because if we check the 'EXIT' 
%% message a "naughty" child, who does unlink(Sup), could hang the 
%% supervisor. 
%% Returns: ok | {error, OtherReason}  (this should be reported)
%%-----------------------------------------------------------------
shutdown(Pid, brutal_kill) ->
    case monitor_child(Pid) of
    ok ->
        exit(Pid, kill),
        receive
        {'DOWN', _MRef, process, Pid, killed} ->
            ok;
        {'DOWN', _MRef, process, Pid, OtherReason} ->
            {error, OtherReason}
        end;
    {error, Reason} ->      
        {error, Reason}
    end;
shutdown(Pid, Time) ->
    case monitor_child(Pid) of
    ok ->
        exit(Pid, shutdown), %% Try to shutdown gracefully
        receive 
        {'DOWN', _MRef, process, Pid, shutdown} ->
            ok;
        {'DOWN', _MRef, process, Pid, OtherReason} ->
            {error, OtherReason}
        after Time ->
            exit(Pid, kill),  %% Force termination.
            receive
            {'DOWN', _MRef, process, Pid, OtherReason} ->
                {error, OtherReason}
            end
        end;
    {error, Reason} ->      
        {error, Reason}
    end.


%% Help function to shutdown/2 switches from link to monitor approach
monitor_child(Pid) ->

    %% Do the monitor operation first so that if the child dies 
    %% before the monitoring is done causing a 'DOWN'-message with
    %% reason noproc, we will get the real reason in the 'EXIT'-message
    %% unless a naughty child has already done unlink...
    erlang:monitor(process, Pid),
    unlink(Pid),

    receive
    %% If the child dies before the unlik we must empty
    %% the mail-box of the 'EXIT'-message and the 'DOWN'-message.
    {'EXIT', Pid, Reason} -> 
        receive 
        {'DOWN', _, process, Pid, _} ->
            {error, Reason}
        end
    after 0 -> 
        %% If a naughty child did unlink and the child dies before
        %% monitor the result will be that shutdown/2 receives a 
        %% 'DOWN'-message with reason noproc.
        %% If the child should die after the unlink there
        %% will be a 'DOWN'-message with a correct reason
        %% that will be handled in shutdown/2. 
        ok   
    end.

