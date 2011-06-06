%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Page session for interaction with the page displayed on the user agent. Support for comet polls and websocket.
%%      The page session is the switchboard for getting data pushed to the user agent.  All queued requests 
%%      can be sent via the current request being handled, via a comet poll or a websocket connection.

%% Copyright 2009 Marc Worrell
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

-module(z_session_page).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

-include_lib("zotonic.hrl").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% session exports
-export([
    start_link/0, 
    start_link/1, 
    stop/1, 
    ping/1,
    
    session_pid/1,
    set/3, 
    get/2, 
    incr/3, 
    append/3,
    
    add_script/2,
    add_script/1,
    get_scripts/1,
    comet_attach/2,
    comet_detach/1,
    websocket_attach/2,

    get_attach_state/1,
    
    check_timeout/1,
    
    spawn_link/4
]).

-record(page_state, {
    last_detach,
    session_pid,
    linked=[],
    comet_pid=undefined,
    websocket_pid=undefined,
    script_queue=[],
    vars=[]
}).

%%====================================================================
%% API
%%====================================================================

%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the person manager server
start_link() ->
    start_link([]).
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
    try
        gen_server:cast(Pid, stop)
    catch _Class:_Term -> 
        error 
    end.

%% @doc Receive a ping, makes sure that we stay alive
ping(Pid) ->
    gen_server:cast(Pid, ping).

session_pid(Pid) ->
    gen_server:call(Pid, session_pid).

get_attach_state(Pid) ->
    try
        gen_server:call(Pid, get_attach_state)
    catch _Class:_Term -> 
        error 
    end.
        
set(Key, Value, #context{page_pid=Pid}) ->
	set(Key, Value, Pid);
set(Key, Value, Pid) ->
    gen_server:cast(Pid, {set, Key, Value}).

get(Key, #context{page_pid=Pid}) ->
	get(Key, Pid);
get(Key, Pid) ->
    gen_server:call(Pid, {get, Key}).

incr(Key, Value, #context{page_pid=Pid}) ->
	incr(Key, Value, Pid);
incr(Key, Value, Pid) ->
    gen_server:call(Pid, {incr, Key, Value}).

append(Key, Value, #context{page_pid=Pid}) ->
    append(Key, Value, Pid);
append(Key, Value, Pid) ->
    gen_server:cast(Pid, {append, Key, Value}).

%% @doc Attach the comet request process to the page session, enabling sending scripts to the user agent
comet_attach(CometPid, Pid) ->
    gen_server:cast(Pid, {comet_attach, CometPid}).

%% @doc Called when the comet request process closes, we will need to wait for the next connection
comet_detach(Pid) ->
    gen_server:cast(Pid, comet_detach).

%% @doc Attach the websocket request process to the page session, enabling sending scripts to the user agent
websocket_attach(WsPid, #context{page_pid=Pid}) ->
    websocket_attach(WsPid, Pid);
websocket_attach(WsPid, Pid) ->
    gen_server:cast(Pid, {websocket_attach, WsPid}).

%% @doc Called by the comet process or the page request to fetch any outstanding scripts
get_scripts(Pid) ->
    gen_server:call(Pid, get_scripts).

%% @doc Send a script to the user agent, will be queued and send when the comet process attaches
add_script(Script, #context{page_pid=Pid}) ->
    add_script(Script, Pid);
add_script(Script, Pid) ->
    gen_server:cast(Pid, {add_script, Script}).

%% @doc Split the scripts from the context and add the scripts to the page.
add_script(Context) ->
    {Scripts, CleanContext} = z_script:split(Context),
    add_script(Scripts, CleanContext),
    CleanContext.


%% @doc Spawn a new process, linked to the page pid
spawn_link(Module, Func, Args, Context) ->
    gen_server:call(Context#context.page_pid, {spawn_link, Module, Func, Args}).


%% @doc Kill this page when timeout has been reached
check_timeout(Pid) ->
    gen_server:cast(Pid, check_timeout).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server, initialises the pid lookup dicts
init(Args) ->
    SessionPid   = proplists:get_value(session_pid, Args),
    IntervalMsec = (?SESSION_PAGE_TIMEOUT div 2) * 1000,
    timer:apply_interval(IntervalMsec, ?MODULE, check_timeout, [self()]),
    State = #page_state{session_pid=SessionPid, last_detach=z_utils:now()},
    {ok, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({set, Key, Value}, State) ->
    State1 = State#page_state{vars = z_utils:prop_replace(Key, Value, State#page_state.vars)},
    {noreply, State1};
handle_cast({append, Key, Value}, State) ->
    NewValue = case proplists:lookup(Key, State#page_state.vars) of
        {Key, L} -> L ++ [Value];
        none -> [Value]
    end, 
    State1 = State#page_state{vars = z_utils:prop_replace(Key, NewValue, State#page_state.vars)},
    {noreply, State1};

handle_cast({comet_attach, CometPid}, State) ->
    case z_utils:is_process_alive(CometPid) of
        true ->
            erlang:monitor(process, CometPid),
            StateComet = State#page_state{comet_pid=CometPid},
            StatePing  = ping_comet_ws(StateComet),
            z_session:keepalive(State#page_state.session_pid),
            {noreply, StatePing};
        false ->
            {noreply, State}
    end;
handle_cast(comet_detach, State) ->
    StateNoComet = State#page_state{comet_pid=undefined, last_detach=z_utils:now()},
    {noreply, StateNoComet};
handle_cast({websocket_attach, WebsocketPid}, State) ->
    case z_utils:is_process_alive(WebsocketPid) of
        true ->
            erlang:monitor(process, WebsocketPid),
            StateWs = State#page_state{websocket_pid=WebsocketPid},
            StatePing = ping_comet_ws(StateWs),
            z_session:keepalive(State#page_state.session_pid),
            {noreply, StatePing};
        false ->
            {noreply, State}
    end;

handle_cast({add_script, Script}, State) ->
    StateQueued = State#page_state{script_queue=[Script|State#page_state.script_queue]},
    StatePing   = ping_comet_ws(StateQueued),
    {noreply, StatePing};
    
%% @doc Do not timeout while there is a comet or websocket process attached
handle_cast(check_timeout, State) when is_pid(State#page_state.comet_pid) or is_pid(State#page_state.websocket_pid)->
    z_utils:flush_message({'$gen_cast', check_timeout}),
    {noreply, State};

%% @doc Give the comet process some time to come back, timeout afterwards
handle_cast(check_timeout, State) ->
    Timeout = State#page_state.last_detach + ?SESSION_PAGE_TIMEOUT,
    z_utils:flush_message({'$gen_cast', check_timeout}),
    case Timeout =< z_utils:now() of
        true ->  {stop, normal, State};
        false -> {noreply, State}
    end;

handle_cast(ping, State) ->
    {noreply, State#page_state{last_detach=z_utils:now()}};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages

handle_call(session_pid, _From, State) ->
    {reply, State#page_state.session_pid, State};

handle_call({spawn_link, Module, Func, Args}, _From, State) ->
    Pid    = spawn_link(Module, Func, Args),
    Linked = [Pid | State#page_state.linked],
    erlang:monitor(process, Pid),
    {reply, Pid, State#page_state{linked=Linked}};

handle_call(get_scripts, _From, State) ->
    Queue   = State#page_state.script_queue,
    State1  = State#page_state{script_queue=[]},
    Scripts = lists:reverse(Queue),
    {reply, Scripts, State1};

handle_call({get, Key}, _From, State) ->
    Value = proplists:get_value(Key, State#page_state.vars),
    {reply, Value, State};

handle_call({incr, Key, Delta}, _From, State) ->
    NV = case proplists:lookup(Key, State#page_state.vars) of
        {Key, V} -> z_convert:to_integer(V) + Delta;
        none -> Delta
    end,
    State1 = State#page_state{ vars = z_utils:prop_replace(Key, NV, State#page_state.vars) },
    {reply, NV, State1};


handle_call(get_attach_state, _From, State) when is_pid(State#page_state.websocket_pid) ->
    {reply, attached, State};
handle_call(get_attach_state, _From, State) ->
    case State#page_state.comet_pid of
        undefined ->
            {reply, {detached, State#page_state.last_detach}, State};
        _Pid ->
            {reply, attached, State}
    end;

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) when Pid == State#page_state.websocket_pid ->
    {noreply, State#page_state{websocket_pid=undefined, last_detach=z_utils:now()}};
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) when Pid == State#page_state.comet_pid ->
    {noreply, State#page_state{comet_pid=undefined, last_detach=z_utils:now()}};
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    Linked = lists:delete(Pid, State#page_state.linked),
    {noreply, State#page_state{linked=Linked}};
handle_info(_, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% Terminate all processes coupled to the page.
terminate(_Reason, State) ->
    lists:foreach(fun(Pid) -> exit(Pid, 'EXIT') end, State#page_state.linked),
    ok.


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Ping the comet process that we have a script queued
ping_comet_ws(#page_state{script_queue=[]} = State) ->
    State;
ping_comet_ws(#page_state{comet_pid=undefined, websocket_pid=undefined} = State) ->
    State;
ping_comet_ws(#page_state{websocket_pid=WsPid} = State) when is_pid(WsPid) ->
    try
        State#page_state.websocket_pid ! {send_data, lists:reverse(State#page_state.script_queue)},
        State#page_state{script_queue=[]}
    catch _M : _E ->
        State#page_state{websocket_pid=undefined}
    end;
ping_comet_ws(State) ->
    try
        State#page_state.comet_pid ! script_queued,
        State
    catch _M : _E ->
        State#page_state{comet_pid=undefined}
    end.

