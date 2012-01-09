%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc User agent session management for zotonic.  A ua session is a process started for every
%%      user agent visiting the site.  The session is alive for a fixed period after the 
%%      last request has been done.  The session manager manages all the ua session processes.
%%
%% @todo make sure that all sessions and page sessions are linked to some process so that they will be killed 
%% when the zotonic application is stopped.

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


-module(z_session_manager).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% The name of the session cookie
-define(SESSION_COOKIE, "z_sid").

%% The name of the persistent data cookie
-define(PERSIST_COOKIE, "z_pid").

%% Max age of the person cookie, 10 years or so.
-define(PERSIST_COOKIE_MAX_AGE, 3600*24*3650).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% External exports
-export([
    continue_session/1,
    ensure_session/1, 
    stop_session/1, 
    rename_session/1, 
	add_script/1,
	add_script/2,
    count/1, 
    dump/1, 
	get_session_id/1,
    tick/1,
    foreach/2,
    broadcast/2
]).

-include_lib("zotonic.hrl").

%% The session server state
-record(session_srv, {context, key2pid, pid2key, persist2pid, pid2persist}).


%% @spec start_link(SiteProps) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the session manager server
start_link(SiteProps) -> 
    {host, Host} = proplists:lookup(host, SiteProps),
    Name = z_utils:name_for_host(?MODULE, Host),
    gen_server:start_link({local, Name}, ?MODULE, SiteProps, []).

%% @spec continue_session(Context) -> Context
%% @doc Continue an existing session
continue_session(#context{session_manager=SessionManager} = Context) ->
    case get_session_id(Context) of
        undefined -> {error, no_session_id};
        _ -> gen_server:call(SessionManager, {ensure_session, false, Context})
    end.

%% @spec ensure_session(Context) -> Context
%% @doc Start a new session or continue an existing session
ensure_session(#context{session_manager=SessionManager} = Context) ->
    case gen_server:call(SessionManager, {ensure_session, true, Context}) of
        {ok, Context1} -> Context1;
        {error, _} -> Context
    end.

%% @spec stop_session(Context) -> Context
%% @doc Explicitly stop an existing session
stop_session(#context{session_manager=SessionManager} = Context) ->
    gen_server:call(SessionManager, {stop_session, Context}).

%% @spec rename_session(Context) -> Context
%% @doc Rename the session id, only call this after ensure_session
rename_session(#context{session_manager=SessionManager} = Context) ->
    gen_server:call(SessionManager, {rename_session, Context}).

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

%% @spec broadcast(#broadcast{}, Context) -> ok
%% @doc Broadcast a notification message to all open sessions.
broadcast(#broadcast{title=Title, message=Message, is_html=IsHtml, type=Type, stay=Stay}, Context) ->
	Message1 = case IsHtml of
		true -> [ <<"<strong>">>, Title, <<"</strong> ">>, Message ];
		false -> [ <<"<strong>">>, z_html:escape(Title), <<"</strong> ">>, z_html:escape(Message) ]
	end,
	Context1 = z_context:prune_for_scomp(?ACL_VIS_PUBLIC, Context),
	add_script(z_render:growl(Message1, Type, Stay, Context1)),
	ok.


%% gen_server callbacks

%% @spec init(SiteProps) -> {ok, State}
%% @doc Initialize the session server with an empty session table.  We make the session manager a system process
%%      so that crashes in sessions are isolated from each other.
init(SiteProps) ->
	{host, Host} = proplists:lookup(host, SiteProps),
    State = #session_srv{
					context=z_acl:sudo(z_context:new(Host)),
					key2pid=dict:new(), 
					pid2key=dict:new(), 
					persist2pid=dict:new(), 
					pid2persist=dict:new()
			},
    timer:apply_interval(?SESSION_CHECK_EXPIRE * 1000, ?MODULE, tick, [self()]),
    process_flag(trap_exit, true),
    {ok, State}.

%% Ensure that the request has a session attached, pings the session
handle_call({ensure_session, AllowNew, Context}, _From, State) ->
    SessionId = get_session_id(Context),
    case session_find_pid(SessionId, State) of
        Pid when is_pid(Pid) orelse AllowNew ->
            {_Pid1, Context1, State1} = ensure_session1(SessionId, Pid, Context, State),
            {reply, {ok, Context1}, State1};
        error ->
            {reply, {error, no_session_pid}, State}
    end;

%% Stop the session from the context or the request props
handle_call({stop_session, Context}, _From, State) ->
    case Context#context.session_pid of
        undefined -> 
            SessionId = get_session_id(Context),
            case SessionId of
                undefined -> true;
                S -> forget_session_id(S, State)
            end;
        Pid -> 
            z_session:stop(Pid)
    end,
    Context1 = clear_session_id(Context),
    {reply, Context1, State};


%% Rename the current session, retain the same session pid, only call this after ensure_session
handle_call({rename_session, Context}, _From, State) ->
    case Context#context.session_pid of
        undefined -> 
            {reply, Context, State};
        Pid -> 
			{Context1, State1} = rename_session(Pid, Context, State),
            {reply, Context1, State1}
    end;

%% Return the number of sessions
handle_call(count, _From, State) ->
    Count = dict:size(State#session_srv.pid2key),
    {reply, Count, State};

%% Dump all sessions to stdout
handle_call(dump, _From, State) ->
    SesPids = dict:to_list(State#session_srv.pid2key),
    lists:foreach(fun({Pid,Key}) -> io:format("sid:~p~n", [Key]), z_session:dump(Pid) end, SesPids),
    {reply, ok, State};
    
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.

%% Handle the down message from a stopped session, remove it from the session admin
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    State1 = erase_session_pid(Pid, State),
    {noreply, State1};
handle_info(_Msg, State) -> 
    {noreply, State}.

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

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%% Make sure that the session cookie is set and that the session process has been started.
ensure_session1(S, P, Context, State) when S == undefined orelse P == error ->
	{PersistId, Context1} = ensure_persist_cookie(Context),
	case dict:find(PersistId, State#session_srv.persist2pid) of
		{ok, Pid} ->
			% Browser restart, though session still alive
			z_session:restart(Pid),
			{Context2, State1} = rename_session(Pid, Context1, State),
		    Context3 = Context2#context{session_pid = Pid},
			{Pid, Context3, State1};
		error ->
		    Pid       = spawn_session(PersistId, Context1),
		    SessionId = z_ids:id(),
		    Context2  = set_session_id(SessionId, Context1),
			State1    = store_persist_pid(PersistId, Pid, store_session_pid(SessionId, Pid, State)),
		    Context3  = Context2#context{session_pid = Pid},
			z_notifier:notify(session_init, Context3),
			Context4 = z_notifier:foldl(session_init_fold, Context3, Context3),
		    {Pid, Context4, State1}
	end;
ensure_session1(SessionId, Pid, Context, State) ->
    z_session:keepalive(Context#context.page_pid, Pid),
    Context1  = Context#context{session_pid = Pid, props=[{session_id, SessionId}|Context#context.props]},
    {Pid, Context1, State}.


rename_session(Pid, Context, State) ->
	case z_context:get(set_session_id, Context) of
		true ->
			{Context, State};
		_ ->
		    % Remove old session-id from the lookup tables
		    State1 = erase_session_pid(Pid, State),
		    % Generate a new session id and set cookie
		    SessionId = z_ids:id(),
		    Context1  = set_session_id(SessionId, Context),
		    State2    = store_session_pid(SessionId, Pid, State1),
			{Context1, State2}
	end.


%% @spec erase_session_pid(pid(), State) -> State
%% @doc Remove the pid from the session state
erase_session_pid(Pid, State) ->
    case dict:find(Pid, State#session_srv.pid2key) of
        {ok, Key} ->
			State1 = State#session_srv{
                    pid2key = dict:erase(Pid, State#session_srv.pid2key),
                    key2pid = dict:erase(Key, State#session_srv.key2pid)
                },
			case dict:find(Pid, State1#session_srv.pid2persist) of
				{ok, Persist} ->
					State1#session_srv{
						persist2pid = dict:erase(Persist, State#session_srv.persist2pid),
						pid2persist = dict:erase(Pid, State#session_srv.pid2persist)
					};
				error ->
					State1
			end;
        error ->
            State
    end.


%% @spec store_session_pid(SessionId, pid(), State) -> State
%% @doc Add the pid to the session state
store_session_pid(SessionId, Pid, State) ->
    State#session_srv{
            pid2key = dict:store(Pid, SessionId, State#session_srv.pid2key),
            key2pid = dict:store(SessionId, Pid, State#session_srv.key2pid)
        }.


%% @spec store_persist_pid(PersistId, pid(), State) -> State
%% @doc Add the pid to the persist state
store_persist_pid(PersistId, Pid, State) ->
    State#session_srv{
            pid2persist = dict:store(Pid, PersistId, State#session_srv.pid2persist),
            persist2pid = dict:store(PersistId, Pid, State#session_srv.persist2pid)
        }.


%% @spec forget_session_id(SessionId::string(), State) -> true | error
%% @doc Stop the session process linked to the session id
forget_session_id(SessionId, State) ->
    case dict:find(SessionId, State#session_srv.key2pid) of
        {ok, Pid} ->
            z_session:stop(Pid);
        error ->
            error
    end.


%% @spec session_find_pid(string(), State) ->  error | pid()
%% @doc find the pid associated with the session id
session_find_pid(undefined, _State) ->
    error;
session_find_pid(SessionId, State) ->
    case dict:find(SessionId, State#session_srv.key2pid) of
        {ok, Pid} ->
            Pid;
        error ->
            error
    end.


%% @spec spawn_session(PersistId, Context) -> pid()
%% @doc Spawn a new session, monitor the pid as we want to know about normal exits
spawn_session(PersistId, Context) ->
    {ok, Pid} = z_session:start_link(PersistId, Context),
    erlang:monitor(process, Pid),
    Pid.


%% @spec get_session_id(Context) -> undefined | string()
%% @doc fetch the session id from the request, return 'undefined' when not found
get_session_id(Context) ->
    case z_context:get_cookie(?SESSION_COOKIE, Context) of
        undefined ->
            % Check the z_sid query args
            ReqData = z_context:get_reqdata(Context),
            case wrq:get_qs_value("z_sid", ReqData) of
                undefined ->
                    case dict:find(z_sid, wrq:path_info(ReqData)) of
                        {ok, SessionId} -> SessionId;
                        error -> undefined
                    end;
                SessionId ->
                    SessionId
            end;
        SessionId ->
            SessionId
    end.

%% @spec set_session_id(SessionId::string(), Context::#context{}) -> #context{}
%% @doc Save the session id in a cookie on the user agent
set_session_id(SessionId, Context) ->
    Options = [{path, "/"},
               {http_only, true}],
    z_context:set([{set_session_id, true}, {session_id, SessionId}], z_context:set_cookie(?SESSION_COOKIE, SessionId, Options, Context)).


%% @spec clear_session_id(Context::#context{}) -> #context{}
%% @doc Remove the session id from the user agent and clear the session pid in the context
clear_session_id(Context) ->
    Options = [{max_age, 0}, 
               {path, "/"}, 
               {http_only, true}],
    Context1 = z_context:set_cookie(?SESSION_COOKIE, "", Options, Context),
    Context1#context{session_pid=undefined}.



%% @doc Ensure that there is a persistent cookie set at the browser, return the updated context and the id.
%% We need to do this on first visit as the user might communicate further via websockets.
ensure_persist_cookie(Context) ->
    case z_context:get_cookie(?PERSIST_COOKIE, Context) of
        undefined ->
            NewPersistCookieId = z_ids:id(),
            Options = [
                {max_age, ?PERSIST_COOKIE_MAX_AGE}, 
                {path, "/"},
                {http_only, true}],
            {NewPersistCookieId, z_context:set_cookie(?PERSIST_COOKIE, NewPersistCookieId, Options, Context)};
        PersistCookie ->
            {PersistCookie, Context}
    end.
