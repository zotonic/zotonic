%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2014 Marc Worrell
%% @doc Session for zotonic, holds all information for the current session at an user agent.
%%      An agent can have multiple pages open and an user_session can have multiple sessions.
%%      The user agent session also starts and monitors the page sessions.

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

% Sessions have two timeouts:
%
% 1. Initial timeout after setting up the process till the first ping from the page (expire_1)
% 2. Inactivity timeout after receiving some communication from the page (expire_n)
%
% This is to make a distinction between pages fetched by visitors (with executing callbacks etc)
% and bots (without executing callbacks).
%
% Pages fetched by bots won't setup the web socket connection, or perform postbacks, so we can
% quickly stop the page and session-process.
%
% Real visitors will perform interaction with the page, for this we keep the session and
% page-process. Even during some inactivity.

-module(z_session).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

-include_lib("zotonic.hrl").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% session exports
-export([
    start_link/3,
    stop/1,
    set/2,
    set/3,
    get/2,
    get/3,
    incr/3,
    session_id/1,
    rename_session/2,
    persistent_id/1,
    set_persistent/3,
    get_persistent/2,
    get_persistent/3,
    keepalive/1,
    keepalive/2,
    ensure_page_session/1,
    lookup_page_session/2,
    get_pages/1,
    get_attach_state/1,
    add_script/2,
    add_script/1,
    transport/2,
    receive_ack/2,
    add_cookie/4,
    get_cookies/1,
    clear_cookies/1,
    check_expire/2,
    dump/1,
    spawn_link/4,
    is_job_overload/1,
    job/2
]).

%% Callback for job/2 and spawn_link/2
-export([
    do_spawned_job/2
    ]).


%% The session state
-record(session, {
            expire,
            expire_1,
            expire_n,
            pages=[] :: list(),
            linked=[],
            session_id = undefined,
            persist_id = undefined,
            persist_is_saved = false,
            persist_is_dirty = false,
            props=[],
            props_persist=[],
            cookies=[],
            transport,
            jobs=[],
            context
            }).

%% The state per page
-record(page, {
            page_id,
            page_pid
            }).


-define(SIDEJOBS_PER_SESSION, 20).


%%====================================================================
%% API
%%====================================================================


start_link(<<>>, PersistId, Context) ->
    start_link(undefined, PersistId, Context);
start_link(SessionId, <<>>, Context) ->
    start_link(SessionId, undefined, Context);
start_link(SessionId, PersistId, Context) ->
    gen_server:start_link(?MODULE, {z_context:site(Context), SessionId, PersistId}, []).

stop(SessionPid) when is_pid(SessionPid) ->
    gen_server:cast(SessionPid, stop).


set(Props, #context{session_pid=Pid}) ->
    set(Props, Pid);
set(Props, Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, {set, Props});
set(_Props, _) ->
    {error, no_session}.


%% Set, get or increment session state variables.
set(Key, Value, #context{session_pid=Pid}) ->
    set(Key, Value, Pid);
set(Key, Value, Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, {set, Key, Value});
set(_K, _V, _) ->
    {error, no_session}.

%% @doc Get a session value. Defaults to 'undefined'.
get(Key, Context) ->
    get(Key, Context, undefined).

%% @doc Get a session value with a default.
get(Key, #context{session_pid=Pid}, DefaultValue) ->
    get(Key, Pid, DefaultValue);
get(Key, Pid, DefaultValue) when is_pid(Pid) ->
    gen_server:call(Pid, {get, Key, DefaultValue});
get(_, _, DefaultValue) ->
    DefaultValue.


incr(Key, Value, #context{session_pid=Pid}) ->
    incr(Key, Value, Pid);
incr(Key, Value, Pid) ->
    gen_server:call(Pid, {incr, Key, Value}).

% @doc Get the session_id of this session.
session_id(#context{session_pid=Pid}) ->
    session_id(Pid);
session_id(Pid) ->
    gen_server:call(Pid, session_id).

% @doc Rename the session
rename_session(NewSessionId, #context{session_pid=Pid}) ->
    rename_session(NewSessionId, Pid);
rename_session(NewSessionId, Pid) ->
    gen_server:call(Pid, {rename_session, NewSessionId}).

persistent_id(Context) ->
    gen_server:call(Context#context.session_pid, persistent_id).

set_persistent(_Key, _Value, #context{session_pid=undefined} = Context) ->
    Context;
set_persistent(Key, Value, Context) ->
    case gen_server:call(Context#context.session_pid, {set_persistent, Key, Value}) of
        {new_persist_id, NewPersistCookieId} ->
            Options = [
                 {max_age, ?PERSIST_COOKIE_MAX_AGE},
                 {path, "/"},
                 {http_only, true}],
             z_context:set_cookie(?PERSIST_COOKIE, NewPersistCookieId, Options, Context);
        ok ->
            Context
    end.

get_persistent(Key, Context) ->
   get_persistent(Key, Context, undefined).

get_persistent(_Key, #context{session_pid=undefined}, DefaultValue) ->
    DefaultValue;
get_persistent(Key, Context, DefaultValue) ->
    gen_server:call(Context#context.session_pid, {get_persistent, Key, DefaultValue}).


%% @doc Send a script to all session pages
-spec add_script(iolist(), #context{}|pid()) -> ok.
add_script(Script, ContextOrPid) ->
    z_transport:session(javascript, Script, ContextOrPid).

%% @doc Split the scripts from the context and add the scripts to the session pages.
-spec add_script(z:context()) -> z:context().
add_script(Context) ->
    {Scripts, CleanContext} = z_script:split(Context),
    z_transport:session(javascript, Scripts, CleanContext),
    CleanContext.

%% @doc Send a msg to all attached pages, queue if no pages
transport([], _Context) ->
    ok;
transport(_Msg, undefined) ->
    ok;
transport(Msg, Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, {transport, Msg});
transport(Msg, Context) ->
    gen_server:cast(Context#context.session_pid, {transport, Msg}).

%% @doc Ack a message, removes it from the retry queue
receive_ack(Ack, Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, {receive_ack, Ack});
receive_ack(Ack, Context) ->
    gen_server:cast(Context#context.session_pid, {receive_ack, Ack}).


%% @doc Store a cookie on the session. Useful for setting cookies from a websocket connection.
add_cookie(Key, Value, Options, #context{session_pid=Pid}) ->
    gen_server:cast(Pid, {add_cookie, Key, Value, Options}).

%% @doc Get cookies stored on the session.
get_cookies(#context{session_pid=Pid}) ->
    gen_server:call(Pid, get_cookies).

%% @doc Resets cookies temporarily stored on the session.
clear_cookies(#context{session_pid=Pid}) ->
    gen_server:cast(Pid, clear_cookies).

%% @doc Reset the expire counter of the session, called from the page process when comet attaches
keepalive(Pid) ->
    gen_server:cast(Pid, keepalive).

%% @doc Reset the timeout counter of the page and session according to the current tick
keepalive(undefined, Pid) ->
    keepalive(Pid);
keepalive(PageId, Pid) ->
    gen_server:cast(Pid, {keepalive, PageId}).


%% @doc Make sure that the request has a page session, when the page session was alive then
%%      adjust the expiration of the page.  Returns a new context with the page id set.
-spec ensure_page_session(#context{}) -> #context{}.
ensure_page_session(#context{req=undefined} = Context) ->
    Context;
ensure_page_session(#context{session_pid=undefined} = Context) ->
    lager:debug("ensure page session without a session_pid"),
    Context;
ensure_page_session(#context{page_pid=undefined}=Context) ->
    {ok, NewPageId, PagePid} = gen_server:call(Context#context.session_pid, start_page_session),
    Context#context{page_id=NewPageId, page_pid=PagePid};
ensure_page_session(Context) ->
    Context.


%% @doc Lookup a page session (if any)
lookup_page_session(_PageId, #context{session_pid=undefined}) ->
    {error, notfound};
lookup_page_session(PageId, Context) when is_binary(PageId) ->
    gen_server:call(Context#context.session_pid, {lookup_page_session, PageId}).

%% @spec check_expire(Now::integer(), Pid::pid()) -> none()
%% @doc Check session and page expiration, periodically called by the session manager
check_expire(Now, Pid) ->
    gen_server:cast(Pid, {check_expire, Now}).


%% @spec get_attach_state(Context::#context{}) -> [States]
%% @doc Check the state of all the attached pages.
get_attach_state(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_attach_state);
get_attach_state(Context) ->
    get_attach_state(Context#context.session_pid).


%% @doc Get app page pid()'s that are attached to the session
get_pages(undefined) ->
    [];
get_pages(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_pages);
get_pages(#context{} = Context) ->
    get_pages(Context#context.session_pid).


%% @doc Dump the session details
dump(Pid) ->
    gen_server:cast(Pid, dump).


%% @doc Spawn a new process, linked to the session pid, supervised by sidejob
-spec spawn_link(atom(), atom(), list(), #context{}) -> {ok, pid()} | {error, no_session|overload}.
spawn_link(_Module, _Func, _Args, #context{session_pid=undefined}) ->
    {error, no_session};
spawn_link(Module, Func, Args, #context{session_pid=SessionPid} = Context) ->
    Job = {Module, Func, Args},
    case sidejob_supervisor:spawn(zotonic_sidejobs, {?MODULE, do_spawned_job, [Job, Context]}) of
        {ok, Pid} when is_pid(Pid) ->
            gen_server:cast(SessionPid, {link, Pid}),
            {ok, Pid};
        {error, overload} ->
            {error, overload}
    end.

-spec is_job_overload(#context{}) -> boolean().
is_job_overload(#context{session_pid=SessionPid}) ->
    case gen_server:call(SessionPid, sidejob_check, infinity) of
        ok -> false;
        overload -> true
    end.

-spec job(list(#z_msg_v1{})|#z_msg_v1{}|function()|{atom(),atom(),list()}, #context{}) -> {ok, pid()|undefined} | {error, overload}.
job(Job, #context{session_pid=undefined} = Context) ->
    case Job of
        Fun when is_function(Fun, 0) ->
            Fun(),
            {ok, [], Context};
        Fun when is_function(Fun, 1) ->
            Fun(Context),
            {ok, [], Context};
        {M,F,A} ->
            erlang:apply(M, F, A),
            {ok, [], Context};
        Msg ->
            z_transport:incoming(Msg, Context)
    end;
job(Job, #context{session_pid=SessionPid} = Context) ->
    case gen_server:call(SessionPid, job_check, infinity) of
        ok ->
            case sidejob_supervisor:spawn(zotonic_sessionjobs, {?MODULE, do_spawned_job, [Job, Context]}) of
                {ok, Pid} when is_pid(Pid) ->
                    gen_server:cast(SessionPid, {job, Pid}),
                    {ok, Pid};
                {error, overload} ->
                    {error, overload}
            end;
        overload ->
            {error, overload}
    end.

-spec do_spawned_job(list(#z_msg_v1{})|#z_msg_v1{}|function()|{atom(),atom(),list()}, #context{}) -> ok.
do_spawned_job(Fun, _Context) when is_function(Fun,0) ->
    Fun();
do_spawned_job(Fun, Context) when is_function(Fun,1) ->
    Fun(Context);
do_spawned_job({M,F,A}, _Context) ->
    erlang:apply(M, F, A);
do_spawned_job(Msg, Context) ->
    {ok, Reply, Context1} = z_transport:incoming(Msg, Context),
    z_transport:transport(Reply, Context1),
    ok.


%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Host, SessionId, PersistId}) ->
    process_flag(trap_exit, true),
    lager:md([
        {site, Host},
        {module, ?MODULE}
      ]),
    gproc:reg({p, l, {Host, user_session, undefined}}),
    {ok, new_session(Host, SessionId, PersistId)}.

handle_cast(stop, Session) ->
    {stop, normal, Session};

%% @doc Reset the timeout counter for the session and, optionally, a specific page
handle_cast(keepalive, Session) ->
    Session1 = Session#session{
                    expire=z_utils:now() + Session#session.expire_n
                },
    {noreply, Session1};

%% @doc Reset the timeout counter, propagate to the page process.
handle_cast({keepalive, PageId}, Session) ->
    Session1 = Session#session{
                    expire=z_utils:now() + Session#session.expire_n
                },
    case find_page(PageId, Session1) of
        #page{page_pid=Pid} -> z_session_page:ping(Pid);
        undefined -> ok
    end,
    {noreply, Session1};

%% @doc Check session expiration, stop when passed expiration.
handle_cast({check_expire, Now}, Session) ->
    Session1 = Session#session{
                    transport=z_transport_queue:periodic(Session#session.transport)
                },
    case Session1#session.pages of
        [] ->
            if
                Session1#session.expire < Now -> {stop, normal, Session1};
                true -> {noreply, Session1}
            end;
        _ ->
            Expire   = Now + ?SESSION_PAGE_TIMEOUT,
            Session2 = if
                            Expire > Session1#session.expire -> Session1#session{expire=Expire};
                            true -> Session1
                       end,
            {noreply, Session2}
    end;

%% @doc Add a script to a specific page's script queue
handle_cast({send_script, Script, PageId}, Session) ->
    case find_page(PageId, Session) of
        undefined ->
            Session;
        #page{page_pid=Pid} ->
            z_session_page:add_script(Script, Pid)
    end,
    {noreply, Session};


%% @doc Add a message to all page's transport queues
handle_cast({transport, Msg}, #session{pages=[]} = Session) ->
    {noreply, Session#session{transport=z_transport_queue:in(Msg, Session#session.transport)}};
handle_cast({transport, Msg}, #session{pages=Pages} = Session) ->
    lists:foreach(
            fun (P) ->
                z_session_page:transport(Msg, P#page.page_pid)
            end,
            Pages),
    Transport1 = z_transport_queue:wait_ack(Msg, session, Session#session.transport),
    {noreply, Session#session{transport=Transport1}};

%% @doc Receive a message ack
handle_cast({receive_ack, Ack}, Session) ->
    Transport1 = z_transport_queue:ack(Ack, Session#session.transport),
    {noreply, Session#session{transport=Transport1}};


%% @doc Add a cookie to the session. Useful for setting cookies from a websocket connection.
handle_cast({add_cookie, Key, Value, Options}, Session) ->
    Cookies = [{Key, Value, Options} | Session#session.cookies],
    {noreply, Session#session{cookies=Cookies}};

%% @doc Reset the stored cookies.
handle_cast(clear_cookies, Session) ->
    {noreply, Session#session{cookies=[]}};

%% @doc Set the session variable, replaces any old value
handle_cast({set, language, Value}, Session) ->
    case proplists:get_value(language, Session#session.props) of
        Value ->
            {noreply, Session};
        _ ->
            Session1 = handle_set(language, Value, Session),
            z_notifier:notify(#language{language=Value}, Session1#session.context),
            {noreply, Session1}
    end;

handle_cast({set, Key, Value}, Session) ->
    Session1 = handle_set(Key, Value, Session),
    Session2 = maybe_auth_change(Key, Value, Session1, Session),
    {noreply, Session2};

handle_cast({set, Props}, Session) ->
    Props1 = lists:foldl(fun({K,V}, Ps) ->
                            prop_replace(K, V, Ps, z_context:site(Session#session.context))
                         end,
                         Session#session.props,
                         Props),
    {noreply, Session#session{props = Props1}};

handle_cast({link, Pid}, Session) ->
    MRef = erlang:monitor(process, Pid),
    Linked = [{Pid,MRef} | Session#session.linked],
    {noreply, Session#session{linked=Linked}};

handle_cast({job, Pid}, Session) ->
    MRef = erlang:monitor(process, Pid),
    erlang:link(Pid),
    {noreply, Session#session{jobs=[{Pid,MRef}|Session#session.jobs]}};

handle_cast(dump, Session) ->
    io:format("~p~n", [Session]),
    {noreply, Session};

handle_cast(Msg, Session) ->
    {stop, {unknown_cast, Msg}, Session}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
handle_call(persistent_id, _From, Session) ->
    PersistedSession = case Session#session.persist_is_saved of
        true -> Session;
        false -> save_persist(Session#session{persist_is_dirty=true})
    end,
    {reply, PersistedSession#session.persist_id, PersistedSession};

%% @doc Get the session if of this session.
handle_call(session_id, _From, Session) ->
    {reply, Session#session.session_id, Session};


%% @doc Rename the session
handle_call({rename_session, NewSessionId}, _From, Session) ->
    {reply, ok, Session#session{session_id=NewSessionId}};

%% @doc Set a persistent variable, replaces any old value
handle_call({set_persistent, Key, Value}, _From, #session{persist_id=undefined}=Session) ->
    PersistId = new_id(),
    Session1 = Session#session{
        persist_id=PersistId,
        props_persist=[{Key, Value}],
        persist_is_dirty=true},
    {reply, {new_persist_id, PersistId}, save_persist(Session1)};
handle_call({set_persistent, Key, Value}, _From, Session) ->
    case proplists:get_value(Key, Session#session.props_persist) of
        Value ->
            {reply, ok, Session};
        _Other ->
            % @todo Save the persistent state on tick, and not every time it is changed.
            %       For now (and for testing) this is ok.
            Session1 = Session#session{
                            props_persist = z_utils:prop_replace(Key, Value, Session#session.props_persist),
                            persist_is_dirty = true
                    },
            {reply, ok, save_persist(Session1)}
    end;

handle_call({get_persistent, Key, DefaultValue}, _From, Session) ->
    {reply, proplists:get_value(Key, Session#session.props_persist, DefaultValue), Session};

handle_call(get_cookies, _From, Session) ->
    {reply, Session#session.cookies, Session};

handle_call({get, Key, DefaultValue}, _From, Session) ->
    {reply, proplists:get_value(Key, Session#session.props, DefaultValue), Session};

handle_call({incr, Key, Delta}, _From, Session) ->
    NV = case proplists:lookup(Key, Session#session.props) of
        {Key, V} -> z_convert:to_integer(V) + Delta;
        none -> Delta
    end,
    {reply, NV, Session#session{props = z_utils:prop_replace(Key, NV, Session#session.props)}};

handle_call(start_page_session, _From, Session) ->
    {Page, Session1} = do_ensure_page_session_new(Session),
    Session2 = transport_all(Session1),
    {reply, {ok, Page#page.page_id, Page#page.page_pid}, Session2};

handle_call({lookup_page_session, PageId}, _From, Session) ->
    case find_page(PageId, Session) of
        undefined ->
            {reply, {error, notfound}, Session};
        #page{page_pid=Pid} ->
            z_session_page:ping(Pid),
            {reply, {ok, Pid}, Session}
    end;

handle_call(get_attach_state, _From, Session) ->
    {reply, [z_session_page:get_attach_state(Pid) ||  #page{page_pid=Pid} <- Session#session.pages], Session};

handle_call(get_pages, _From, Session) ->
    {reply, [ Pid ||  #page{page_pid=Pid} <- Session#session.pages], Session};

handle_call(job_check, _From, #session{jobs=Jobs} = Session) ->
    Reply = case length(Jobs) < ?SIDEJOBS_PER_SESSION of
        true -> ok;
        false -> overload
    end,
    {reply, Reply, Session};

handle_call(Msg, _From, Session) ->
    {stop, {unknown_cast, Msg}, Session}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, Session) ->
    case lists:keytake(Pid, 1, Session#session.jobs) of
        {value, {Pid,_MRef}, Jobs} ->
            {noreply, Session#session{jobs=Jobs}};
        false ->
            case lists:keytake(Pid, 1, Session#session.linked) of
                {value, {Pid,_MRef}, Linked} ->
                    {noreply, Session#session{linked=Linked}};
                false ->
                    case lists:keytake(Pid, #page.page_pid, Session#session.pages) of
                        {value, #page{}, Pages} ->
                            exometer:update([zotonic, z_context:site(Session#session.context), session, page_processes], -1),
                            {noreply, Session#session{pages=Pages}};
                        false ->
                            % Happens after auth-change, where we disconnect all pages.
                            {noreply, Session}
                    end
            end
    end;

%% @doc MQTT message, forward it to the page.
%% TODO: Add all handling
handle_info({route, Msg}, State) ->
    lager:debug("Session route ~p", [Msg]),
    {noreply, State};

handle_info(_, Session) ->
    {noreply, Session}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% Terminate all processes coupled to the session.
terminate(_Reason, Session) ->
    save_persist(Session),
    lists:foreach(fun exit_linked/1, Session#session.linked),
    lists:foreach(fun exit_linked/1, Session#session.jobs),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, Session, _Extra) ->
    {ok, Session}.


%%====================================================================
%% support functions
%%====================================================================

exit_linked({Pid, MRef}) ->
    erlang:demonitor(MRef),
    exit(Pid, session).


handle_set(Key, Value, Session) ->
    Session#session{ props = prop_replace(Key, Value, Session#session.props, z_context:site(Session#session.context))}.


maybe_auth_change(auth_user_id, UserId, Session, OldSession) ->
    case proplists:get_value(auth_user_id, OldSession#session.props) of
        UserId ->
            Session;
        none when UserId =:= undefined ->
            Session;
        undefined when UserId =:= none ->
            Session;
        _OldUserId ->
            lists:foreach(fun(#page{page_pid=PagePid}) ->
                              z_session_page:auth_change(PagePid)
                           end,
                           Session#session.pages),
            Session#session{pages=[], transport=z_transport_queue:new()}
    end;
maybe_auth_change(_K, _V, Session, _OldSession) ->
    Session.


%% @doc Try to transport all queued messages to the connected pages
transport_all(#session{pages=[]} = Session) ->
    Session;
transport_all(#session{transport=Transport, pages=Pages} = Session) ->
    case z_transport_queue:is_empty(Transport) of
        true ->
            Session;
        false ->
            {Ms,Transport1} = z_transport_queue:out_all(Transport),
            lists:foreach(
                fun(#page{page_pid=PagePid}) ->
                    z_session_page:transport(Ms, PagePid)
                end,
                Pages),
            Transport2 = lists:foldl(
                            fun(Msg, TQAcc) ->
                                z_transport_queue:wait_ack(Msg, session, TQAcc)
                            end,
                            Transport1,
                            Ms),
            Session#session{transport=Transport2}
    end.


%% @doc Initialize a new session record
new_session(Host, SessionId, PersistId) ->
    Context = z_context:new(Host),
    Expire1 = z_convert:to_integer(m_config:get_value(site, session_expire_1, ?SESSION_EXPIRE_1, Context)),
    ExpireN = z_convert:to_integer(m_config:get_value(site, session_expire_n, ?SESSION_EXPIRE_N, Context)),
    load_persist(#session{
            expire=z_utils:now() + Expire1,
            expire_1 = Expire1,
            expire_n = ExpireN,
            session_id = SessionId,
            persist_id = PersistId,
            transport=z_transport_queue:new(),
            context=Context
            }).


%% @doc Load the persistent data from the database, used on session start.
load_persist(Session) ->
    {PropsPersist, PersistIsSaved} =
            case m_persistent:get(Session#session.persist_id, Session#session.context) of
                L when is_list(L) -> {L,  true};
                _ -> {[], false}
            end,
    Session#session{
        props_persist    = PropsPersist,
        persist_is_dirty = false,
        persist_is_saved = PersistIsSaved
    }.


%% @doc Save the persistent data to the database, when it is changed. Reset the dirty flag.
save_persist(#session{persist_is_dirty=true, persist_id=Id, props_persist=Props, context=Context} = Session) ->
    ok = m_persistent:put(Id, Props, Context),
    Session#session{persist_is_dirty = false, persist_is_saved = true};
save_persist(Session) ->
    Session.


%% @doc Replace properties, special handling for tracking sessions belonging to an user_id
prop_replace(auth_user_id, NewUserId, Props, Site) when is_list(Props) ->
    case lists:keyfind(auth_user_id, 1, Props) of
        {auth_user_id, NewUserId} ->
            Props;
        {auth_user_id, PrefUserId} ->
            gproc:unreg({p, l, {Site, user_session, PrefUserId}}),
            gproc:reg({p, l, {Site, user_session, NewUserId}}),
            z_utils:prop_replace(auth_user_id, NewUserId, Props);
        false ->
            gproc:unreg({p, l, {Site, user_session, undefined}}),
            gproc:reg({p, l, {Site, user_session, NewUserId}}),
            [{auth_user_id, NewUserId} | Props]
    end;
prop_replace(Key, Value, Props, _Site) when is_list(Props) ->
    z_utils:prop_replace(Key, Value, Props).

% @doc Create a new page session
do_ensure_page_session_new(Session) ->
    case page_start(Session#session.context) of
        {error, already_started} ->
            do_ensure_page_session_new(Session);
        {ok, P} ->
            {P, Session#session{pages=[P|Session#session.pages]}}
    end.

%% @doc Return a new page record, monitor the started page process because we want to know about normal exits
page_start(Context) ->
    PageId = new_id(),
    case z_session_page:start_link(self(), PageId, Context) of
        {ok,PagePid} ->
            erlang:monitor(process, PagePid),
            exometer:update([zotonic, z_context:site(Context), session, page_processes], 1),
            {ok, #page{page_pid=PagePid, page_id=PageId}};
        {error, {already_started, _PagePid}} ->
            lager:error("Page-session process already running ~p", [PageId]),
            {error, already_started}
    end.


%% @doc Find the page record in the list of known pages
find_page(undefined, _Session) ->
    undefined;
find_page(PageId, Session) ->
    case lists:keysearch(PageId, #page.page_id, Session#session.pages) of
        {value, Page} -> Page;
        false -> undefined
    end.

new_id() ->
    z_ids:id().
