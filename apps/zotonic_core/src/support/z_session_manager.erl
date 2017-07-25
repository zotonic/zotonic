%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2014 Marc Worrell
%% @doc User agent session management for zotonic.  A ua session is a process started for every
%%      user agent visiting the site.  The session is alive for a fixed period after the
%%      last request has been done.  The session manager manages all the ua session processes.

%% Copyright 2009-2014 Marc Worrell
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


-module(z_session_manager).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% The name of the session cookie
-define(SESSION_COOKIE, <<"z_sid">>).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% External exports
-export([
    start_session/3,
    continue_session/1,
    ensure_session/1,
    stop_session/1,
    stop_session/2,
    rename_session/1,
    whereis/2,
    whereis_user/2,
    add_script/1,
    add_script/2,
    count/1,
    dump/1,
    get_session_id/1,
    tick/1,
    foreach/2,
    broadcast/2,
    fold/3,

    get_session_cookie_name/1
]).

-include_lib("zotonic.hrl").

%% The session server state
-record(session_srv, {context, key2pid, pid2key}).

-type session_id() :: binary().
-type persistent_id() :: binary().

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the session manager server
-spec start_link( SiteProps :: list() ) -> {ok, pid()} | ignore | {error, term()}.
start_link(SiteProps) ->
    {site, Site} = proplists:lookup(site, SiteProps),
    Name = z_utils:name_for_site(?MODULE, Site),
    gen_server:start_link({local, Name}, ?MODULE, SiteProps, []).

%% @doc Continue an existing session. No new session will be created.
-spec continue_session( #context{} ) -> {ok, #context{}} | {error, term()}.
continue_session(#context{session_pid=Pid} = Context) when is_pid(Pid) ->
    {ok, Context};
continue_session(Context) ->
    case get_session_cookie(Context) of
        <<>> -> {ok, Context};
        SessionId -> start_session(optional, SessionId, Context)
    end.

%% @doc Start a new session or continue an existing session
-spec ensure_session( #context{} ) -> {ok, #context{}} | {error, term()}.
ensure_session(#context{session_pid=Pid} = Context) when is_pid(Pid) ->
    {ok, Context};
ensure_session(Context) ->
    start_session(ensure, get_session_cookie(Context), Context).


%% @doc Explicitly stop an existing session
-spec stop_session(#context{}) -> {ok, #context{}} | {error, term()}.
stop_session(#context{session_manager=SessionManager, session_pid=SessionPid} = Context) ->
    SessionId = get_session_cookie(Context),
    case gen_server:call(SessionManager, {stop_session, SessionPid, SessionId}) of
        ok -> {ok, clear_session_cookie(Context)};
        ignore -> {ok, Context}
    end.

-spec stop_session(binary(), #context{}) -> ok | {error, term()}.
stop_session(SessionId, #context{session_manager=SessionManager} = Context) ->
    case whereis(SessionId, Context) of
        undefined ->
            {error, notfound};
        SessionPid ->
            case gen_server:call(SessionManager, {stop_session, SessionPid, SessionId}) of
                ok -> ok;
                ignore -> {error, notfound}
            end
    end.

%% @doc Rename the session id, only call this after ensure_session
-spec rename_session(#context{}) -> {ok, #context{}} | {error, term()}.
rename_session(#context{session_manager=SessionManager, session_pid=SessionPid} = Context) when is_pid(SessionPid) ->
    case z_context:get(set_session_id, Context) of
        true ->
            % The session id cookie has been set, ignore the session id change
            {ok, Context};
        _ ->
            % Rename the session id, set a new session cookie.
            case gen_server:call(SessionManager, {rename_session, SessionPid}) of
                {ok, NewSessionId} ->
                    CleanContext = Context#context{session_id=NewSessionId, page_pid=undefined, page_id=undefined},
                    {ok, set_session_cookie(NewSessionId, CleanContext)};
                ignore ->
                    {ok, Context};
                {error, _} = Error ->
                    Error
            end
    end;
rename_session(Context) ->
    % Ignore, there is no session set.
    {ok, Context}.


%% @spec add_script(Context) -> none()
%% @doc Send the scripts in the context to all pages of all sessions
add_script(Context) ->
    Script = z_script:get_script(Context),
    add_script(Script, Context).

%% @spec add_script(Script::io_list(), Context) -> none()
%% @doc Send a script to all pages of all sessions
add_script(Script, Context) ->
    foreach(fun(Pid) -> z_session:add_script(Script, Pid) end, Context).

%% @spec count(Context) -> Int
%% @doc Return the number of open sessions
count(#context{session_manager=SessionManager}) ->
    gen_server:call(SessionManager, count).

%% @spec dump(Context) -> void()
%% @doc Dump all session to stdout
dump(#context{session_manager=SessionManager}) ->
    gen_server:call(SessionManager, dump).


%% @doc Fetch the session id
-spec get_session_id(#context{}) -> undefined | session_id().
get_session_id(Context) ->
    case Context#context.session_id of
        undefined ->
            case Context#context.session_pid of
                undefined ->
                    get_session_cookie(Context);
                Pid when is_pid(Pid) ->
                    z_session:session_id(Pid)
            end;
        SessionId ->
            SessionId
    end.

%% @doc Find the session with the given id
-spec whereis(session_id(), #context{}) -> pid() | undefined.
whereis(SessionId, #context{session_manager=SessionManager}) when is_binary(SessionId) ->
    case gen_server:call(SessionManager, {whereis, SessionId}) of
        {ok, Pid} -> Pid;
        {error, notfound} -> undefined
    end.

%% @doc Find all the sessions for a certain user
-spec whereis_user(integer()|undefined, #context{}) -> [pid()].
whereis_user(UserId, #context{site=Site}) ->
    gproc:lookup_pids({p, l, {Site, user_session, UserId}}).


%% @spec tick(pid()) -> void()
%% @doc Periodic tick used for cleaning up sessions
tick(SessionManager) when is_pid(SessionManager) ->
    Tick = z_utils:now(),
    foreach(fun(Pid) -> z_session:check_expire(Tick, Pid) end, SessionManager).

%% @spec foreach(function(), #context{}) -> void()
%% @doc Apply the given function to all sessions
foreach(Function, #context{session_manager=SessionManager}) when is_function(Function) ->
    foreach(Function, SessionManager);
foreach(Function, SessionManager) when is_function(Function) ->
    gen_server:cast(SessionManager, {foreach, Function}).

%% @spec fold(function(), Acc0 :: term(), #context{}) -> Acc :: term()
%% @doc Calls Fun on successive sessions together with an extra argument Acc (short for accumulator). Fun must return a new accumulator which is passed to the next call. Acc0 is returned if the list is empty. The evaluation order is undefined.
fold(Function, Acc0, #context{session_manager=SessionManager}) when is_function(Function) ->
    fold(Function, Acc0, SessionManager);
fold(Function, Acc0, SessionManager) ->
    gen_server:call(SessionManager, {fold, Function, Acc0}).

%% @spec broadcast(#broadcast{}, Context) -> ok
%% @doc Broadcast a notification message to all open sessions.
broadcast(#broadcast{title=Title, message=Message, is_html=IsHtml, type=Type, stay=Stay}, Context) ->
    Message1 = case IsHtml of
        true -> [ <<"<strong>">>, Title, <<"</strong> ">>, Message ];
        false -> [ <<"<strong>">>, z_html:escape(Title), <<"</strong> ">>, z_html:escape(Message) ]
    end,
    Context1 = z_context:prune_for_scomp(Context),
    add_script(z_render:growl(Message1, Type, Stay, Context1)),
    ok.



%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(SiteProps) -> {ok, State}
%% @doc Initialize the session server with an empty session table.  We make the session manager a system process
%%      so that crashes in sessions are isolated from each other.
init(SiteProps) ->
    {site, Site} = proplists:lookup(site, SiteProps),
    lager:md([
        {site, Site},
        {module, ?MODULE}
      ]),
    State = #session_srv{
                    context=z_acl:sudo(z_context:new(Site)),
                    key2pid=dict:new(),
                    pid2key=dict:new()
              },
    update_session_metrics(State),
    timer:apply_interval(?SESSION_CHECK_EXPIRE * 1000, ?MODULE, tick, [self()]),
    process_flag(trap_exit, true),
    {ok, State}.

%% Ensure that the request has a session attached, pings the session
handle_call({start_session, Action, SessionId, PersistId}, _From, State) ->
    case session_find_pid(SessionId, State) of
        Pid when is_pid(Pid) orelse Action =:= ensure ->
            {ok, SessionState, NewSessionPid, NewSessionId, State1} = ensure_session1(SessionId, Pid, PersistId, State),
            {reply, {ok, SessionState, NewSessionPid, NewSessionId}, State1};
        error ->
            {reply, {error, no_session_pid}, State}
    end;

%% Stop the session from the context or the request props
handle_call({stop_session, undefined, undefined}, _From, State) ->
    {reply, ignore, State};
handle_call({stop_session, undefined, SessionId}, _From, State) ->
    forget_session_id(SessionId, State),
    {reply, ok, State};
handle_call({stop_session, SessionPid, _SessionId}, _From, State) when is_pid(SessionPid) ->
    z_session:stop(SessionPid),
    {reply, ok, State};


%% Rename the current session, retain the same session pid, only call this after ensure_session
handle_call({rename_session, undefined}, _From, State) ->
    {reply, ignore, State};
handle_call({rename_session, SessionPid}, _From, State) ->
    {ok, NewSessionId, State1} = rename_session(SessionPid, State),
    {reply, {ok, NewSessionId}, State1};

%% Return the number of sessions
handle_call(count, _From, State) ->
    Count = dict:size(State#session_srv.pid2key),
    {reply, Count, State};

%% Dump all sessions to stdout
handle_call(dump, _From, State) ->
    SesPids = dict:to_list(State#session_srv.pid2key),
    lists:foreach(fun({Pid,Key}) -> io:format("sid:~p~n", [Key]), z_session:dump(Pid) end, SesPids),
    {reply, ok, State};

%% Call Function on successive sessions together with an extra argument Acc (short for accumulator).
%% Fun must return a new accumulator which is passed to the next call.
%% Acc0 is returned if no sessions exist.
handle_call({fold, Function, Acc0}, From,
	    #session_srv{context=Context, pid2key=Pid2Key} = State) ->
    SesPids = dict:fetch_keys(Pid2Key),
    if
	is_function(Function, 2) ->
	    spawn(fun() ->
			  Res = lists:foldl(Function, Acc0, SesPids),
			  gen_server:reply(From, Res)
		  end);
	is_function(Function, 3) ->
	    spawn(fun() ->
			  Res = lists:foldl(fun(Pid, Acc) -> Function(Pid, Context#context{session_pid=Pid}, Acc) end, Acc0, SesPids),
			  gen_server:reply(From, Res)
		  end)
    end,
    {noreply, State};

%% Find a specific session.
handle_call({whereis, SessionId}, _From,  #session_srv{key2pid=Key2Pid} = State) ->
    case dict:find(SessionId, Key2Pid) of
        {ok, Pid} ->
            {reply, {ok, Pid}, State};
        error ->
            {reply, {error, notfound}, State}
    end;

%% Find a specific session by pid
handle_call({whois, SessionPid}, _From,  #session_srv{pid2key=Pid2Key} = State) ->
    case dict:find(SessionPid, Pid2Key) of
        {ok, SessionId} ->
            {reply, {ok, SessionId}, State};
        error ->
            {reply, {error, notfound}, State}
    end;

handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.


%% Apply Function to all sessions. The function application is spawned to support slow
%% running functions and to protect the session manager from crashes.
handle_cast({foreach, Function}, #session_srv{context=Context, pid2key=Pid2Key} = State) ->
    SesPids = dict:fetch_keys(Pid2Key),
    if
        is_function(Function, 1) ->
            spawn(fun() -> lists:foreach(fun(Pid) -> Function(Pid) end, SesPids) end);
        is_function(Function, 2) ->
            spawn(fun() -> lists:foreach(fun(Pid) -> Function(Pid, Context#context{session_pid=Pid}) end, SesPids) end)
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% Handle the down message from a stopped session, remove it from the session admin
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    State1 = erase_session_pid(Pid, State),
    update_session_metrics(State1),
    {noreply, State1};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%%====================================================================
%% support functions - called by session server
%%====================================================================


%% Make sure that the session cookie is set and that the session process has been started.
ensure_session1(SessionId, SessionPid, PersistId, State) when SessionId =:= undefined orelse SessionPid =:= error ->
    NewSessionId = make_session_id(),
    NewSessionPid = spawn_session(NewSessionId, PersistId, State#session_srv.context),
    State1 = store_session_pid(NewSessionId, NewSessionPid, State),
    update_session_metrics(State1),
    {ok, new, NewSessionPid, NewSessionId, State1};
ensure_session1(SessionId, SessionPid, _PersistId, State) ->
    {ok, alive, SessionPid, SessionId, State}.

rename_session(Pid, State) ->
    % Remove old session pid from the lookup tables
    State1 = erase_session_pid(Pid, State),

    % Generate a new session id and set cookie
    NewSessionId = make_session_id(),

    % Tell the session it has a new session id.
    ok = z_session:rename_session(NewSessionId, Pid),

    State2 = store_session_pid(NewSessionId, Pid, State1),
    {ok, NewSessionId, State2}.

make_session_id() ->
    z_ids:id(32).

%% @doc Remove the pid from the session state
-spec erase_session_pid(pid(), #session_srv{}) -> #session_srv{}.
erase_session_pid(Pid, State) ->
    case dict:find(Pid, State#session_srv.pid2key) of
        {ok, Key} ->
            State#session_srv{
                    pid2key = dict:erase(Pid, State#session_srv.pid2key),
                    key2pid = dict:erase(Key, State#session_srv.key2pid)};
        error ->
            State
    end.


%% @doc Add the pid to the session state
-spec store_session_pid(session_id(), pid(), #session_srv{}) -> #session_srv{}.
store_session_pid(SessionId, Pid, State) when is_binary(SessionId) and is_pid(Pid) ->
    State#session_srv{
            pid2key = dict:store(Pid, SessionId, State#session_srv.pid2key),
            key2pid = dict:store(SessionId, Pid, State#session_srv.key2pid)
        }.


%% @doc Stop the session process linked to the session id
-spec forget_session_id(session_id(), #session_srv{}) -> true | error.
forget_session_id(SessionId, State) ->
    case dict:find(SessionId, State#session_srv.key2pid) of
        {ok, Pid} ->
            z_session:stop(Pid);
        error ->
            error
    end.


%% @doc find the pid associated with the session id
-spec session_find_pid(session_id() | undefined, #session_srv{}) -> pid() | error.
session_find_pid(undefined, _State) ->
    error;
session_find_pid(SessionId, State) ->
    case dict:find(SessionId, State#session_srv.key2pid) of
        {ok, Pid} ->
            Pid;
        error ->
            error
    end.


%% @doc Spawn a new session, monitor the pid as we want to know about normal exits
-spec spawn_session(session_id(), persistent_id(), #context{}) -> pid().
spawn_session(SessionId, PersistId, Context) ->
    {ok, Pid} = z_session:start_link(SessionId, PersistId, Context),
    erlang:monitor(process, Pid),
    Pid.



%%====================================================================
%% support functions - outside gen_server
%%====================================================================


-spec start_session( optional | ensure, session_id(), #context{} ) -> {ok, #context{}} | {error, term()}.
start_session(optional, undefined, Context) ->
    {ok, Context};
start_session(Action, CurrentSessionId, Context) ->
    PersistId = to_binary(z_context:get_cookie(?PERSIST_COOKIE, Context)),
    case gen_server:call(Context#context.session_manager, {start_session, Action, CurrentSessionId, PersistId}) of
        {ok, SessionState, SessionPid, NewSessionId} ->
            Context1 = Context#context{
                            session_pid=SessionPid,
                            session_id=NewSessionId
                       },
            Context2 = case NewSessionId of
                           CurrentSessionId -> Context1;
                            _ -> set_session_cookie(NewSessionId, Context1)
                       end,
            % lager:debug("Session: ~p ~p (old ~p, for ~p)", [SessionState, NewSessionId, CurrentSessionId, m_req:get(peer, Context2)]),
            Props = [
               {remote_ip, m_req:get(peer, Context2)}
            ],
            z_session:set(Props, Context2),
            Context3 = case SessionState of
                           new ->
                               z_notifier:notify(#session_init{}, Context2),
                               z_notifier:foldl(#session_init_fold{}, Context2, Context2);
                           restart ->
                               Context2;
                           alive ->
                               z_session:keepalive(Context2#context.page_pid, SessionPid),
                               Context2
                       end,
            {ok, Context3};
        {error, no_session_pid} when Action =:= optional ->
            lager:debug("Session: continuation request for non-existing session ~p", [CurrentSessionId]),
            {ok, Context};
        {error, _} = Error ->
            Error
    end.


%% @doc fetch the session id from the request, return 'undefined' when not found
-spec get_session_cookie( #context{} ) -> string() | undefined.
get_session_cookie(Context) ->
    case z_context:get_cookie(get_session_cookie_name(Context), Context) of
        undefined ->
            % Check the z_sid in query or dispatch args
            case z_context:get_q(?SESSION_COOKIE, Context) of
                undefined ->
                    % and as last resort check the context to support custom mechanisms
                    to_binary(z_context:get(z_sid, Context));
                SessionId ->
                    to_binary(SessionId)
            end;
        SessionId ->
            to_binary(SessionId)
    end.

%% @doc Fetch the name of the session cookie. Default to "z_sid"
-spec get_session_cookie_name(#context{}) -> binary().
get_session_cookie_name(Context) ->
    case m_config:get_value(site, session_cookie_name, Context) of
        undefined -> ?SESSION_COOKIE;
        Cookie -> z_convert:to_binary(Cookie)
    end.

%% @doc Save the session id in a cookie on the user agent
-spec set_session_cookie( binary(), #context{} ) -> #context{}.
set_session_cookie(SessionId, Context) when is_binary(SessionId) ->
    Options = [{path, <<"/">>},
               {http_only, true}],
    z_context:set_cookie(
                    get_session_cookie_name(Context),
                    SessionId,
                    Options,
                    z_context:set(set_session_id, true, Context)).


%% @doc Remove the session id from the user agent and clear the session pid in the context
-spec clear_session_cookie( #context{} ) -> #context{}.
clear_session_cookie(Context) ->
    Options = [{max_age, 0},
               {path, <<"/">>},
               {http_only, true}],
    Context1 = z_context:set_cookie(get_session_cookie_name(Context), <<>>, Options, Context),
    Context1#context{session_id=undefined, session_pid=undefined}.


%% @doc Update the metrics of the session count
update_session_metrics(State) ->
    Value = dict:size(State#session_srv.pid2key),
    exometer:update([zotonic, z_context:site(State#session_srv.context), session, sessions], Value).

to_binary(undefined) -> undefined;
to_binary(A) -> z_convert:to_binary(A).
