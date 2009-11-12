%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Session for zotonic, holds all information for the current session at an user agent.
%%      An agent can have multiple pages open and an user_session can have multiple sessions.
%%      The user agent session also starts and monitors the page sessions.

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


-module(z_session).
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
    set/3,
    get/2, 
    incr/3, 
    keepalive/1, 
    keepalive/2, 
    ensure_page_session/2,
    add_script/2,
    check_expire/2,
    dump/1,
    spawn_link/4
    ]).


%% The session state
-record(session, {
            expire,
            timer_ref,
            pages,
            linked,
            props=[]
            }).

%% The state per page
-record(page, {
            page_id,
            page_pid
            }).

-define(PAGEID_OFFSET, 2).


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

set(Key, Value, Pid) ->
    gen_server:cast(Pid, {set, Key, Value}).

get(Key, Pid) ->
    gen_server:call(Pid, {get, Key}).

incr(Key, Value, Pid) ->
    gen_server:call(Pid, {incr, Key, Value}).


%% @doc Reset the expire counter of the session, called from the page process when comet attaches
keepalive(Pid) ->
    gen_server:cast(Pid, keepalive).

%% @doc Reset the timeout counter of the page and session according to the current tick
keepalive(PageId, Pid) ->
    gen_server:cast(Pid, {keepalive, PageId}).


%% @spec ensure_page_session(Context::#context, Pid::pid()) -> #context
%% @doc Make sure that the request has a page session, when the page session was alive then
%%      adjust the expiration of the page.  Returns a new context with the page id set.
ensure_page_session(Context, Pid) ->
    gen_server:call(Pid, {ensure_page_session, Context}).


%% @spec add_script(Script::io_list(), PageId::list(), Pid::pid()) -> none()
%% @doc Send a script to all session pages
add_script(Script, Pid) ->
    gen_server:cast(Pid, {add_script, Script}).


%% @spec check_expire(Now::integer(), Pid::pid()) -> none()
%% @doc Check session and page expiration, periodically called by the session manager
check_expire(Now, Pid) ->
    gen_server:cast(Pid, {check_expire, Now}).


%% @doc Dump the session details
dump(Pid) ->
    gen_server:cast(Pid, dump).


%% @doc Spawn a new process, linked to the session pid
spawn_link(Module, Func, Args, Context) ->
    gen_server:call(Context#context.session_pid, {spawn_link, Module, Func, Args, Context}).


%% Gen_server callbacks

init(Args) ->
    Session = new_session(Args),
    {ok, Session}.

handle_cast(stop, Session) ->
    {stop, normal, Session};

%% @doc Reset the timeout counter for the session and, optionally, a specific page
handle_cast(keepalive, Session) ->
    Now      = z_utils:now(),
    Session1 = Session#session{expire=Now + ?SESSION_EXPIRE_N},
    {noreply, Session1};

%% @doc Reset the timeout counter, propagate to the page process.
handle_cast({keepalive, PageId}, Session) ->
    Now      = z_utils:now(),
    Session1 = Session#session{expire=Now + ?SESSION_EXPIRE_N},
    case find_page(PageId, Session1) of
        #page{page_pid=Pid} -> 
            % Keep the page process alive
            catch z_session_page:ping(Pid);
        undefined -> 
            ok
    end,
    {noreply, Session1};

%% @doc Check session expiration, stop when passed expiration.
handle_cast({check_expire, Now}, Session) ->
    case length(Session#session.pages) of
        0 ->
            if 
                Session#session.expire < Now ->  {stop, normal, Session};
                true ->  {noreply, Session}
            end;
        _ ->
            Expire   = Now + ?SESSION_PAGE_TIMEOUT,
            Session1 = if 
                            Expire > Session#session.expire -> Session#session{expire=Expire};
                            true -> Session
                       end,
            {noreply, Session1}
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


%% @doc Add a script to all page's script queues
handle_cast({add_script, Script}, Session) ->
    F = fun(P) ->
            catch z_session_page:add_script(Script, P#page.page_pid)
        end,
    lists:foreach(F, Session#session.pages),
    {noreply, Session};

%% @doc Set the session variable, replaces any old value
handle_cast({set, Key, Value}, Session) ->
    {noreply, Session#session{ props = z_utils:prop_replace(Key, Value, Session#session.props) }};

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
%% Description: Handling call messages

handle_call({get, Key}, _From, Session) ->
    {reply, proplists:get_value(Key, Session#session.props), Session};

handle_call({incr, Key, Delta}, _From, Session) ->
    NV = case proplists:lookup(Key, Session#session.props) of
        {Key, V} -> z_convert:to_integer(V) + Delta;
        none -> Delta
    end,
    {reply, NV, Session#session{props = z_utils:prop_replace(Key, NV, Session#session.props)}};

handle_call({spawn_link, Module, Func, Args, Context}, _From, Session) ->
    Pid    = spawn_link(Module, Func, [Args, Context]),
    Linked = [Pid | Session#session.linked],
    erlang:monitor(process, Pid),
    {reply, Pid, Session#session{linked=Linked}};

handle_call({ensure_page_session, Context}, _From, Session) ->
    Context1  = z_context:ensure_qs(Context),
    PageId    = z_context:get_q(?SESSION_PAGE_Q, Context1),
    NewPageId = case PageId of
                    undefined -> z_ids:id();
                    Id -> Id
                end,
    {NewPage, Session1} = case find_page(NewPageId, Session) of
                            undefined -> 
                                % Make a new page for this pid
                                P     = page_start(NewPageId),
                                Pages = [P|Session#session.pages], 
                                {P, Session#session{pages=Pages}};
                            #page{page_pid=Pid}=P -> 
                                % Keep the page alive
                                catch z_session_page:ping(Pid),
                                {P, Session}
                          end,
    Context2 = Context1#context{page_id=NewPageId, page_pid=NewPage#page.page_pid},
    {reply, Context2, Session1};

handle_call(Msg, _From, Session) ->
    {stop, {unknown_cast, Msg}, Session}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, Session) ->
    FIsUp  = fun(Page) -> Page#page.page_pid /= Pid end,
    Pages  = lists:filter(FIsUp, Session#session.pages),
    Linked = lists:delete(Pid, Session#session.linked),
    {noreply, Session#session{pages=Pages, linked=Linked}};
handle_info(_, Session) ->
    {noreply, Session}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% Terminate all processes coupled to the session.
terminate(_Reason, Session) ->
    lists:foreach(fun(Pid) -> exit(Pid, 'EXIT') end, Session#session.linked),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, Session, _Extra) ->
    {ok, Session}.


%%====================================================================
%% support functions
%%====================================================================


%% @doc Initialize a new session record
new_session(_Args) ->
    Now = z_utils:now(),
    #session{
            expire=Now + ?SESSION_EXPIRE_1,
            timer_ref=undefined,
            pages=[],
            linked=[],
            props=[]
            }.

%% @doc Return a new page record, monitor the started page process because we want to know about normal exits
page_start(PageId) ->
    {ok,PagePid} = z_session_page:start_link([{session_pid, self()}]),
    erlang:monitor(process, PagePid),
    #page{page_pid=PagePid, page_id=PageId }.

%% @doc Find the page record in the list of known pages
find_page(undefined, _Session) ->
    undefined;
find_page(PageId, Session) ->
    case lists:keysearch(PageId, ?PAGEID_OFFSET, Session#session.pages) of
        {value, Page} -> Page;
        false -> undefined
    end.
