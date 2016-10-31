%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2013 Marc Worrell
%% @doc Page session for interaction with the page displayed on the user agent. Support for comet polls and websocket.
%%      The page session is the switchboard for getting data pushed to the user agent.  All queued requests
%%      can be sent via the current request being handled, via a comet poll or a websocket connection.

%% Copyright 2009-2013 Marc Worrell
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

-compile([{parse_transform, lager_transform}]).

-include_lib("zotonic.hrl").
-include_lib("emqtt/include/emqtt.hrl").

-define(INTERVAL_MSEC, (?SESSION_PAGE_TIMEOUT div 2) * 1000).

-record(auth_change, {page_id}).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% session exports
-export([
    start_link/3,
    stop/1,

    whereis/2,
    ping/1,

    session_pid/1,
    set/3,
    get/2,
    incr/3,
    append/3,

    auth_change/1,
    page_id/1,

    add_script/2,
    add_script/1,
    transport/2,
    transport/3,
    receive_ack/2,
    receive_ack/3,

    get_transport_data/1,
    get_transport_msgs/1,
    comet_attach/2,
    comet_detach/2,
    websocket_attach/2,
    websocket_attach/3,

    get_attach_state/1,

    check_timeout/1,

    spawn_link/4
]).

-record(page_state, {
    last_detach,
    session_pid,
    page_id,
    site,
    linked=[],
    comet_pid=undefined,
    comet_monitor_ref=undefined,
    websocket_pid=undefined,
    websocket_monitor_ref=undefined,
    transport,
    vars=[]
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the person manager server
-spec start_link(pid(), binary(), #context{}) -> {ok, pid()} | {error, term()}.
start_link(SessionPid, PageId, Context) when is_binary(PageId) ->
    % lager:debug(z_context:lager_md(Context), "[~p] register page ~p", [z_context:site(Context), PageId]),
    gen_server:start_link({via, z_proc, {{session_page,PageId}, Context}},
                          ?MODULE,
                          {SessionPid, PageId, z_context:site(Context)},
                          []).

stop(#context{page_pid=PagePid}) ->
    stop(PagePid);
stop(undefined) ->
    ok;
stop(Pid) ->
    try
        gen_server:cast(Pid, stop)
    catch _Class:_Term ->
        error
    end.

whereis(PageId, Context) when is_binary(PageId) ->
    z_proc:whereis({session_page, PageId}, Context).

%% @doc Receive a ping, makes sure that we stay alive
ping(Pid) ->
    gen_server:cast(Pid, ping).

session_pid(#context{page_pid=PagePid}) ->
    session_pid(PagePid);
session_pid(undefined) ->
    undefined;
session_pid(Pid) ->
    gen_server:call(Pid, session_pid).

get_attach_state(#context{page_pid=PagePid}) ->
    get_attach_state(PagePid);
get_attach_state(undefined) ->
    detached;
get_attach_state(Pid) ->
    try
        gen_server:call(Pid, get_attach_state)
    catch _Class:_Term ->
        error
    end.

auth_change(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, auth_change).

page_id(#context{page_pid=Pid}) ->
    page_id(Pid);
page_id(undefined) ->
    undefined;
page_id(Pid) when is_pid(Pid) ->
    case catch gen_server:call(Pid, page_id) of
        {'EXIT', _} -> undefined;
        {ok, PageId} -> PageId
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
    gen_server:call(Pid, {comet_attach, CometPid}).

%% @doc Called when the comet request process closes, we will need to wait for the next connection
comet_detach(_CometPid, undefined) ->
    z_utils:flush_message(script_queued);
comet_detach(CometPid, PagePid) ->
    catch gen_server:call(PagePid, {comet_detach, CometPid}),
    z_utils:flush_message(script_queued).

%% @doc Attach the websocket request process to the page session, enabling sending scripts to the user agent
-spec websocket_attach(pid(), binary()|undefined, #context{}) -> ok.
websocket_attach(WsPid, PageId, Context) when is_binary(PageId) ->
    websocket_attach(WsPid, whereis(PageId, Context));
websocket_attach(_WsPid, undefined, _Context) ->
    % No page id, ignore attach.
    ok.

-spec websocket_attach(pid(), #context{}|pid()|undefined) -> ok.
websocket_attach(WsPid, #context{page_pid=PagePid}) ->
    websocket_attach(WsPid, PagePid);
websocket_attach(WsPid, PagePid) when is_pid(PagePid) ->
    gen_server:cast(PagePid, {websocket_attach, WsPid});
websocket_attach(_WsPid, undefined) ->
    % No page process, ignore attach
    ok.

%% @doc Called by the comet process or the page request to fetch any queued transport messages
get_transport_data(Pid) when is_pid(Pid) ->
    {ok, Data} = gen_server:call(Pid, get_transport_data),
    Data.

%% @doc Called by the postback process or the page request to fetch any queued transport messages
get_transport_msgs(#context{page_pid=undefined}) ->
    [];
get_transport_msgs(#context{page_pid=PagePid}) ->
    case is_process_alive(PagePid) of
        true -> get_transport_msgs(PagePid);
        false -> []
    end;
get_transport_msgs(Pid) when is_pid(Pid) ->
    {ok, Msgs} = gen_server:call(Pid, get_transport_msgs),
    Msgs.

%% @doc Send a script to the user agent, will be queued and send when the comet process attaches
add_script(Script, Context) ->
    z_transport:page(javascript, Script, Context).

%% @doc Split the scripts from the context and add the scripts to the page.
add_script(Context) ->
    {Scripts, CleanContext} = z_script:split(Context),
    z_transport:page(javascript, Scripts, CleanContext),
    CleanContext.

%% @doc Send a msg to the pages, queue if no page-transport attached
transport([], _Context) ->
    ok;
transport(_Msg, undefined) ->
    ok;
transport(Msg, #context{page_pid=PagePid}) ->
    transport(Msg, PagePid);
transport(Msg, Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, {transport, Msg}).

transport(Msg, PageId, Context) when is_binary(PageId) ->
    transport(Msg, whereis(PageId, Context)).

%% @doc Receive an ack for a sent message
-spec receive_ack(#z_msg_ack{}, #context{}|pid()) -> ok.
receive_ack(_Ack, undefined) ->
    ok;
receive_ack(Ack, #context{page_pid=PagePid}) ->
    receive_ack(Ack, PagePid);
receive_ack(Ack, Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, {receive_ack, Ack}).

receive_ack(Ack, PageId, Context) when is_binary(PageId) ->
    receive_ack(Ack, whereis(PageId, Context)).

%% @doc Spawn a new process, linked to the session pid, supervised by the sidejobs
-spec spawn_link(atom(), atom(), list(), #context{}) -> {ok, pid()} | {error, no_page|overload}.
spawn_link(_Module, _Func, _Args, #context{page_pid=undefined}) ->
    {error, no_page};
spawn_link(Module, Func, Args, #context{page_pid=PagePid} = Context) ->
    Job = {Module, Func, Args},
    case sidejob_supervisor:spawn(zotonic_sidejobs, {z_session, do_spawned_job, [Job, Context]}) of
        {ok, Pid} when is_pid(Pid) ->
            gen_server:cast(PagePid, {link, Pid}),
            {ok, Pid};
        {error, overload} ->
            {error, overload}
    end.

%% @doc Kill this page when timeout has been reached
check_timeout(Pid) ->
    Pid ! check_timeout.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server, initialises the pid lookup dicts
init({SessionPid, PageId, Site}) ->
    process_flag(trap_exit, true),
    lager:md([
        {site, Site},
        {module, ?MODULE},
        {page_id, PageId}
      ]),
    trigger_check_timeout(),
    {ok, #page_state{
            session_pid=SessionPid,
            page_id=PageId,
            last_detach=z_utils:now(),
            site=Site,
            transport=z_transport_queue:new()
    }}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}

handle_cast(stop, State) ->
    {stop, normal, cleanup(State)};
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

handle_cast({websocket_attach, WebsocketPid}, #page_state{websocket_pid=WebsocketPid} = State) ->
    {noreply, State};
handle_cast({websocket_attach, WebsocketPid}, State) ->
    case z_utils:is_process_alive(WebsocketPid) of
        true ->
            %% Close the possibly attached websocket.
            State1 = opt_close_websocket(State),
            State2 = opt_close_comet(State1),

            %% Attach the new websocket.
            StateWs = State2#page_state{
                websocket_pid = WebsocketPid, 
                websocket_monitor_ref = erlang:monitor(process, WebsocketPid)
            },
            StatePing = ping_comet_ws(StateWs),
            z_session:keepalive(StatePing#page_state.session_pid),
            {noreply, StatePing};
        false ->
            {noreply, State}
    end;

%% @doc Add a message to all page's transport queues
handle_cast({transport, Ms}, #page_state{transport=Transport} = State) when is_list(Ms) ->
    Transport1 = lists:foldl(
                    fun(M, TQAcc) ->
                        z_transport_queue:in(M, TQAcc)
                    end,
                    Transport,
                    Ms),
    State2 = ping_comet_ws(State#page_state{transport=Transport1}),
    {noreply, State2};
handle_cast({transport, Msg}, State) ->
    State1 = State#page_state{transport=z_transport_queue:in(Msg, State#page_state.transport)},
    State2 = ping_comet_ws(State1),
    {noreply, State2};

%% @doc Handle the ack of a sent message
handle_cast({receive_ack, Ack}, State) ->
    {noreply, State#page_state{transport=z_transport_queue:ack(Ack, State#page_state.transport)}};

handle_cast(ping, State) ->
    {noreply, State#page_state{last_detach=z_utils:now()}};

%% The user of the session changed, signal any connected push connections, stop this page.
handle_cast(auth_change, #page_state{comet_pid=CometPid, websocket_pid=WsPid} = State) ->
    Msg = z_transport:msg(page, <<"session">>, #auth_change{page_id=State#page_state.page_id}, []),
    case WsPid of
        undefined when is_pid(CometPid) ->
            CometPid ! {final, [Msg]};
        undefined ->
            ok;
        _ ->
            {ok, Data} = z_ubf:encode([Msg]),
            controller_websocket:websocket_send_data(WsPid, Data)
    end,
    {stop, normal, State};

handle_cast({link, Pid}, State) ->
    MRef = erlang:monitor(process, Pid),
    Linked = [{Pid,MRef} | State#page_state.linked],
    {noreply, State#page_state{linked=Linked}};

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

handle_call(page_id, _From, State) ->
    {reply, {ok, State#page_state.page_id}, State};

handle_call(get_transport_data, _From, State) ->
    {Data, State1} = do_transport_data(State),
    {reply, {ok, Data}, State1};

handle_call(get_transport_msgs, _From, State) ->
    {Msgs, State1} = do_fetch_transport_msgs(State),
    {reply, {ok, Msgs}, State1};

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

handle_call({comet_attach, _CometPid}, _From, #page_state{websocket_pid=Pid}=State) when is_pid(Pid) ->
    {reply, {error, websocket_connected}, State};
handle_call({comet_attach, CometPid}, _From, State) ->
    case z_utils:is_process_alive(CometPid) of
        true ->
            State1 = opt_close_comet(State),
            MRef = erlang:monitor(process, CometPid),
            StateComet = State1#page_state{comet_pid=CometPid, comet_monitor_ref=MRef},
            StatePing  = ping_comet_ws(StateComet),
            z_session:keepalive(State1#page_state.session_pid),
            {reply, ok, StatePing};
        false ->
            {reply, ok, State}
    end;
handle_call({comet_detach, Pid}, _From, #page_state{comet_pid=Pid}=State) ->
    erlang:demonitor(State#page_state.comet_monitor_ref), 
    StateNoComet = State#page_state{
        comet_pid=undefined, 
        comet_monitor_ref=undefined,
        last_detach=z_utils:now()
    },
    {reply, ok, StateNoComet};
handle_call({comet_detach, _Pid}, _From, State) ->
    {reply, {error, not_attached}, State};

handle_call(get_attach_state, _From, #page_state{websocket_pid = Pid} = State) when is_pid(Pid) ->
    {reply, attached, State};
handle_call(get_attach_state, _From, #page_state{comet_pid = Pid} = State) when is_pid(Pid) ->
    {reply, attached, State};
handle_call(get_attach_state, _From, State) ->
    {reply, {detached, State#page_state.last_detach}, State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, #page_state{session_pid=Pid} = State) ->
    {stop, normal, cleanup(State)};
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, #page_state{websocket_pid=Pid} = State) ->
    {noreply, State#page_state{websocket_pid=undefined, websocket_monitor_ref=undefined, last_detach=z_utils:now()}};
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, #page_state{comet_pid=Pid} = State) ->
    {noreply, State#page_state{comet_pid=undefined, comet_monitor_ref=undefined, last_detach=z_utils:now()}};
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    Linked = lists:keydelete(Pid, 1, State#page_state.linked),
    {noreply, State#page_state{linked=Linked}};

%% @doc Do not timeout while there is a comet or websocket process attached
handle_info(check_timeout, State) when is_pid(State#page_state.comet_pid) or is_pid(State#page_state.websocket_pid)->
    State1 = State#page_state{transport=z_transport_queue:periodic(State#page_state.transport)},
    State2 = ping_comet_ws(State1),
    z_utils:flush_message(check_timeout),
    trigger_check_timeout(),
    {noreply, State2};

%% @doc Give the comet process some time to come back, timeout afterwards
handle_info(check_timeout, State) ->
    Timeout = State#page_state.last_detach + ?SESSION_PAGE_TIMEOUT,
    z_utils:flush_message(check_timeout),
    case Timeout =< z_utils:now() of
        true ->
            {stop, normal, State};
        false ->
            State1 = State#page_state{transport=z_transport_queue:periodic(State#page_state.transport)},
            State2 = ping_comet_ws(State1),
            trigger_check_timeout(),
            {noreply, State2}
    end;

%% @doc MQTT message, forward it to the page.
%% @todo Decide if/how/where the topic mapping should be done.
handle_info({route, Msg}, State) ->
    lager:debug("Page ~p route ~p", [State#page_state.page_id, Msg]),
    MinimalContext = #context{
        site=State#page_state.site,
        page_id=State#page_state.page_id,
        session_pid=State#page_state.session_pid
    },
    ClientTopic = z_mqtt:make_context_topic(Msg#mqtt_msg.topic, MinimalContext),
    Msg1 = Msg#mqtt_msg{
                topic=ClientTopic,
                encoder=undefined
            },
    Transport = z_transport:msg(page, <<"mqtt_route">>, Msg1, [{qos, Msg1#mqtt_msg.qos}]),
    handle_cast({transport, Transport}, State);

handle_info(_, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc Terminate all processes coupled to the page.
terminate(_Reason, State) ->
    cleanup(State),
    ok.


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

cleanup(State) ->
    State1 = opt_close_websocket(State),
    State2 = opt_close_comet(State1),
    lists:foreach(
        fun({Pid,MRef}) ->
            erlang:demonitor(MRef), 
            exit(Pid, page)
        end,
        State2#page_state.linked),
    State2#page_state{ linked = [] }.

%% @doc Close the websocket if there is one.
opt_close_websocket(#page_state{websocket_pid = Pid} = State) when is_pid(Pid) ->
    erlang:demonitor(State#page_state.websocket_monitor_ref),
    Pid ! close,
    State#page_state{websocket_pid = undefined, websocket_monitor_ref = undefined};
opt_close_websocket(State) ->
    State.

%% @doc Close the comet connection if there is one.
opt_close_comet(#page_state{comet_pid = Pid} = State) when is_pid(Pid) ->
    erlang:demonitor(State#page_state.comet_monitor_ref),
    Pid ! close,
    State#page_state{comet_pid = undefined, comet_monitor_ref = undefined};
opt_close_comet(State) ->
    State.

%% @doc Trigger sending a check_timeout message.
trigger_check_timeout() ->
    erlang:send_after(?INTERVAL_MSEC, self(), check_timeout).


do_transport_data(State) ->
    {Msgs, State1} = do_fetch_transport_msgs(State),
    case Msgs of
        [] ->
            {<<>>, State};
        _ ->
            {ok, Data} = z_ubf:encode(Msgs),
            {Data, State1}
    end.

do_fetch_transport_msgs(State) ->
    {Msgs,Transport1} = z_transport_queue:out_all(State#page_state.transport),
    Transport2 = lists:foldl(fun(Msg,TQ) ->
                                z_transport_queue:wait_ack(Msg, page, TQ)
                             end,
                             Transport1,
                             Msgs),
    {Msgs, State#page_state{transport=Transport2}}.


%% @doc Ping the comet or ws process that we have a message queued
ping_comet_ws(#page_state{websocket_pid=WsPid, comet_pid=CometPid} = State) when is_pid(WsPid) andalso is_pid(CometPid) ->
    State1 = opt_close_comet(State),
    ping_comet_ws(State1);
ping_comet_ws(#page_state{websocket_pid=WsPid} = State) when is_pid(WsPid) ->
    case do_transport_data(State) of
        {<<>>, State1} -> State1;
        {Data, State1} ->
            controller_websocket:websocket_send_data(WsPid, Data),
            State1
    end;
ping_comet_ws(#page_state{transport=TQ, comet_pid=CometPid} = State) when is_pid(CometPid) ->
    case z_transport_queue:is_empty(TQ) of
        true -> nop;
        false -> CometPid ! transport
    end,
    State;
ping_comet_ws(State) ->
    State.
