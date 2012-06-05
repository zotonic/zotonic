%% @author Marc Worrell <marc@worrell.nl>
%% @doc One for one supervisor to hold named worker processes, for use by modules and session management.
%%      Adapted from supervisor.erl

%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
%% Copyright 2011 Marc Worrell
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.


-module(z_supervisor_named).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

% API
-export([
    start_link/0,
    start_child/2,
    start_child/3,
    restart_child/2,
    delete_child/2,
    terminate_child/2,
    which_children/1,
    count_children/1,
    running_children/1,
    whereis/2,
    ensure_child/3
]).

% gen_server exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    children = dict:new(),
    dynamics = dict:new(),
    intensity = 5,
    period = 5000,
    restarts = []
}).

-record(child, {
    pid = undefined,  % pid is undefined when child is not running
    name,
    mfa,
    restart_type,
    shutdown,
    child_type,
    modules = []
}).


%% @doc Start a new supervisor
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%% ---------------------------------------------------
%%% 
%%% External functions.
%%% 
%%% ---------------------------------------------------

start_child(SupRef, ChildSpecOrPid) ->
    call(SupRef, {start_child, ChildSpecOrPid}).

start_child(SupRef, ChildName, Pid) when is_pid(Pid) ->
    ChildSpec = {
        ChildName,
        Pid,
        temporary, 5000, worker, []
    },
    call(SupRef, {start_child, ChildSpec});
start_child(SupRef, ChildName, {M,_,_} = MFA) ->
    ChildSpec = {
        ChildName,
        MFA,
        temporary, 5000, worker, [M]
    },
    call(SupRef, {start_child, ChildSpec}).


restart_child(SupRef, ChildRef) ->
    call(SupRef, {restart_child, ChildRef}).

terminate_child(SupRef, ChildRef) ->
    call(SupRef, {terminate_child, ChildRef}).

delete_child(SupRef, ChildRef) ->
    call(SupRef, {delete_child, ChildRef}).

which_children(SupRef) ->
    call(SupRef, which_children).

running_children(SupRef) ->
    call(SupRef, running_children).

count_children(SupRef) ->
    call(SupRef, count_children).

whereis(SupRef, ChildRef) ->
    call(SupRef, {whereis, ChildRef}).

ensure_child(SupRef, ChildRef, MFA) ->
    case whereis(SupRef, ChildRef) of
        {ok, Pid} -> {ok, Pid};
        {error, not_running} ->
            case restart_child(SupRef, ChildRef) of
                {ok, Pid} -> {ok, Pid};
                {ok, Pid, _Extra} -> {ok, Pid};
                {error, {running, Pid}} -> {ok, Pid};
                Other -> Other
            end;
        {error, not_found} ->
            case start_child(SupRef, ChildRef, MFA) of
                {ok, Pid} -> {ok, Pid};
                {ok, Pid, _Extra} -> {ok, Pid};
                {error, {already_present, Pid}} -> {ok, Pid};
                {error, already_present} ->
                    case restart_child(SupRef, ChildRef) of
                        {ok, Pid} -> {ok, Pid};
                        {error, {running, Pid}} -> {ok, Pid};
                        Other -> Other
                    end;
                Other -> Other
            end
    end.


call(Supervisor, Req) ->
    gen_server:call(Supervisor, Req, infinity).


%%% ---------------------------------------------------
%%% 
%%% Callback functions.
%%% 
%%% ---------------------------------------------------

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({start_child, Pid}, _From, State) when is_pid(Pid) ->
    case dict:find(Pid, State#state.dynamics) of
        {ok, _} ->
            {error, {already_present, Pid}};
        error ->
            MonitorRef = erlang:monitor(process, Pid),
            State1 = State#state{ 
                        dynamics=dict:store(Pid, {'$MONITOR', MonitorRef}, State#state.dynamics)
                    },
            {reply, {ok, Pid}, State1}
    end;

handle_call({start_child, {Name, MFAorPid, Restart, Shutdown, Type, Modules}}, _From, State) ->
    Child = #child{
        name = Name,
        mfa = MFAorPid,
        restart_type = Restart,
        shutdown = Shutdown,
        child_type = Type,
        modules = Modules
    },
    case dict:find(Child#child.name, State#state.children) of
        {ok, _} ->
            case Child#child.pid of
                undefined ->
                    {error, already_present};
                Pid ->
                    {error, {already_present, Pid}}
            end;
        error ->
            case do_start_child(Child) of
                {ok, Pid} ->
                    {reply, {ok, Pid}, save_child(Pid, Child, State)};
                {ok, Pid, Extra} ->
                    {reply, {ok, Pid, Extra}, save_child(Pid, Child, State)};
                What ->
                    {reply, What, State}
            end
    end;

handle_call({restart_child, Name}, _From, State) ->
    case dict:find(Name, State#state.children) of
        {ok, Child} when Child#child.pid =:= undefined ->
            case do_start_child(Child) of
                {ok, Pid} ->
                    {reply, {ok, Pid}, save_child(Pid, Child, State)};
                {ok, Pid, Extra} ->
                    {reply, {ok, Pid, Extra}, save_child(Pid, Child, State)};
                What ->
                    {reply, What, State}
            end;
        {ok, Child} ->
            {reply, {error, {running, Child#child.pid}}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({whereis, Name}, _From, State) ->
    case dict:find(Name, State#state.children) of
        {ok, Child} when Child#child.pid =:= undefined ->
            {reply, {error, not_running}, State};
        {ok, Child} ->
            {reply, {ok, Child#child.pid}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_child, Pid}, From, State) when is_pid(Pid) ->
    case dict:find(Pid, State#state.dynamics) of
        {ok, {'$MONITOR', MonitorRef}} ->
            erlang:demonitor(MonitorRef),
            {reply, ok, State#state{dynamics=dict:erase(Pid, State#state.dynamics)}};
        {ok, Name} ->
            handle_call({delete_child, Name}, From, State);
        error ->
            {reply, {error, not_found}, State}
    end;
        
handle_call({delete_child, Name}, _From, State) ->
    case dict:find(Name, State#state.children) of
        {ok, Child} when Child#child.pid =:= undefined ->
            {reply, ok, remove_child(Child, State)};
        {ok, _} ->
            {reply, {error, running}, State};
        _ ->
            {reply, {error, not_found}, State}
    end;

handle_call({terminate_child, Name}, _From, State) ->
    case dict:find(Name, State#state.children) of
        {ok, Child} ->
            do_terminate(Child),
            {reply, ok, save_child(undefined, Child, State)};
        _ ->
            {reply, {error, not_found}, State}
    end;

handle_call(which_children, _From, State) ->
    Resp = dict:fold(
            fun(_Key, #child{pid = Pid, name = Name, child_type = ChildType, modules = Mods}, Acc) ->
                [{Name, Pid, ChildType, Mods} | Acc]
            end,
            [],
            State#state.children),
    {reply, Resp, State};

handle_call(running_children, _From, State) ->
    {reply, dict:fetch_keys(State#state.dynamics), State};

handle_call(count_children, _From, State) ->
    %% Specs and children are together on the children list...
    {Specs, Active, Supers, Workers} =
            dict:fold(fun(_Key, Child, Counts) -> 
                        count_child(Child, Counts)
                      end,
                      {0,0,0,0},
                      State#state.children),
    OnlyPid = dict:fold(fun(_Pid, {'$MONITOR', _Ref}, Count) -> Count+1;
                           (_Pid, _Name, Count) -> Count
                        end,
                        0,
                        State#state.dynamics),

    %% Reformat counts to a property list.
    Reply = [
        {specs, Specs}, 
        {active, Active+OnlyPid}, 
        {supervisors, Supers}, 
        {workers, Workers},
        {monitors, OnlyPid}
    ],
    {reply, Reply, State}.

count_child(#child{pid = Pid, child_type = worker}, {Specs, Active, Supers, Workers}) ->
    case is_pid(Pid) andalso is_process_alive(Pid) of
        true ->  {Specs+1, Active+1, Supers, Workers+1};
        false -> {Specs+1, Active, Supers, Workers+1}
    end;
count_child(#child{pid = Pid, child_type = supervisor}, {Specs, Active, Supers, Workers}) ->
    case is_pid(Pid) andalso is_process_alive(Pid) of
        true ->  {Specs+1, Active+1, Supers+1, Workers};
        false -> {Specs+1, Active, Supers+1, Workers}
    end.


%%% Hopefully cause a function-clause as there is no API function
%%% that utilizes cast.
handle_cast(null, State) ->
    error_logger:error_msg("ERROR: Supervisor received cast-message 'null'~n", []),
    {noreply, State}.

%%
%% Take care of terminated children.
%%
handle_info({'EXIT', Pid, Reason}, State) ->
    case restart_child(Pid, Reason, State) of
    {ok, State1} ->
        {noreply, State1};
    {shutdown, State1} ->
        {stop, shutdown, State1}
    end;

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    case dict:find(Pid, State#state.dynamics) of
        {ok, {'$MONITOR', _Ref}} -> 
            {noreply, State#state{
                dynamics=dict:erase(Pid, State#state.dynamics)
            }};
        _Other ->
            {noreply, State}
    end;

handle_info(Msg, State) ->
    error_logger:error_msg("Supervisor received unexpected message: ~p~n", [Msg]),
    {noreply, State}.

%%
%% Terminate this server.
%%
terminate(_Reason, State) ->
    terminate_children(State#state.children),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% ---------------------------------------------------
%%% 
%%% Internal functions.
%%% 
%%% ---------------------------------------------------

do_start_child(#child{mfa = Pid}) when is_pid(Pid) ->
    case catch link(Pid) of
        true ->
            {ok, Pid};
        {'EXIT', {noproc, _}} ->
            {error, noproc}
    end;
do_start_child(#child{mfa = {M, F, A}}) ->
    case catch apply(M, F, A) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, Pid};
        {ok, Pid, Extra} when is_pid(Pid) ->
            {ok, Pid, Extra};
        ignore -> 
            {ok, undefined};
        {error, What} ->
            {error, What};
        What ->
            {error, What}
    end.


save_child(undefined, Child, State) ->
    Child1 = Child#child{pid=undefined},
    State#state{
        dynamics = dict:erase(Child#child.pid, State#state.dynamics),
        children = dict:store(Child1#child.name, Child1, State#state.children)
    };
save_child(Pid, Child, State) ->
    Child1 = Child#child{pid=Pid},
    State#state{
        dynamics = dict:store(Pid, Child1#child.name, 
                                dict:erase(Child#child.pid, State#state.dynamics)),
        children = dict:store(Child1#child.name, Child1, State#state.children)
    }.


remove_child(Child, State) ->
    State#state{
        dynamics = dict:erase(Child#child.pid, State#state.dynamics),
        children = dict:erase(Child#child.name, State#state.children)
    }.


terminate_children(Children) ->
    dict:fold(fun(_Key, Child, Acc) ->
                    [{do_terminate(Child), Child} | Acc]
              end,
              [],
              Children).

do_terminate(Child) when Child#child.pid =/= undefined ->
    case shutdown(Child#child.pid, Child#child.shutdown) of
        ok ->
            ok;
        {error, OtherReason} ->
            report_error(shutdown_error, OtherReason, Child),
            error
    end;
do_terminate(_Child) ->
    nop.


restart_child(Pid, Reason, State) ->
    case dict:find(Pid, State#state.dynamics) of
        {ok, Name} ->
            case dict:find(Name, State#state.children) of
                {ok, Child} ->
                    do_restart(Child#child.restart_type, Reason, Child, State);
                error ->
                    State1 = State#state{
                        dynamics=dict:erase(Pid, State#state.dynamics)
                    },
                    {ok, State1}
            end;
        error ->
            {ok, State}
    end.


do_restart(permanent, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child),
    restart(Child, State);
do_restart(_, normal, Child, State) ->
    NState = state_del_child(Child, State),
    {ok, NState};
do_restart(_, shutdown, Child, State) ->
    NState = state_del_child(Child, State),
    {ok, NState};
do_restart(transient, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child),
    restart(Child, State);
do_restart(temporary, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child),
    NState = state_del_child(Child, State),
    {ok, NState}.


state_del_child(#child{pid=undefined}, State) ->
    State;
state_del_child(#child{name=Name, pid=Pid} = Child, State) ->
    State#state{
        dynamics=dict:erase(Pid, State#state.dynamics),
        children=dict:store(Name, Child#child{pid=undefined}, State#state.children)
    }.

restart(Child, State) ->
    case add_restart(State) of
        {ok, NState} ->
            case do_start_child(Child) of
                {ok, Pid} ->
                    NState = save_child(Pid, Child, State),
                    {ok, NState};
                {ok, Pid, _Extra} ->
                    NState = save_child(Pid, Child, State),
                    {ok, NState};
                {error, Reason} ->
                    report_error(start_error, Reason, Child),
                    restart(Child, State)
            end;
        {terminate, NState} ->
            report_error(shutdown, reached_max_restart_intensity, Child),
            {shutdown, remove_child(Child, NState)}
    end.




%%% ------------------------------------------------------
%%% Add a new restart and calculate if the max restart
%%% intensity has been reached (in that case the supervisor
%%% shall terminate).
%%% All restarts accured inside the period amount of seconds
%%% are kept in the #state.restarts list.
%%% Returns: {ok, State'} | {terminate, State'}
%%% ------------------------------------------------------

add_restart(State) ->  
    I = State#state.intensity,
    P = State#state.period,
    R = State#state.restarts,
    Now = erlang:now(),
    R1 = add_restart([Now|R], Now, P),
    State1 = State#state{restarts = R1},
    case length(R1) of
        CurI when CurI  =< I ->
            {ok, State1};
        _ ->
            {terminate, State1}
    end.

add_restart([R|Restarts], Now, Period) ->
    case inPeriod(R, Now, Period) of
        true ->
            [R|add_restart(Restarts, Now, Period)];
        _ ->
            []
    end;
add_restart([], _, _) ->
    [].

inPeriod(Time, Now, Period) ->
    case difference(Time, Now) of
        T when T > Period ->
            false;
        _ ->
            true
    end.

%%
%% Time = {MegaSecs, Secs, MicroSecs} (NOTE: MicroSecs is ignored)
%% Calculate the time elapsed in seconds between two timestamps.
%% If MegaSecs is equal just subtract Secs.
%% Else calculate the Mega difference and add the Secs difference,
%% note that Secs difference can be negative, e.g.
%%      {827, 999999, 676} diff {828, 1, 653753} == > 2 secs.
%%
difference({TimeM, TimeS, _}, {CurM, CurS, _}) when CurM > TimeM ->
    ((CurM - TimeM) * 1000000) + (CurS - TimeS);
difference({_, TimeS, _}, {_, CurS, _}) ->
    CurS - TimeS.


%%-----------------------------------------------------------------
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


%%% ------------------------------------------------------
%%% Error and progress reporting.
%%% ------------------------------------------------------

report_error(Error, Reason, Child) ->
    ErrorMsg = [
        {supervisor, self()},
        {errorContext, Error},
        {reason, Reason},
        {offender, extract_child(Child)}
    ],
    error_logger:error_report(supervisor_report, ErrorMsg).


extract_child(Child) ->
    [{pid, Child#child.pid},
     {name, Child#child.name},
     {mfa, Child#child.mfa},
     {restart_type, Child#child.restart_type},
     {shutdown, Child#child.shutdown},
     {child_type, Child#child.child_type}].


