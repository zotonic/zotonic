%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Visitor manager.  One visitor can have only one visitor process, with multiple ua-sessions and page sessions.
%%      The visitor process manager spawns the visitor processes.  A visitor process stops itself after some time
%%      of inactivity.  This visitor process manager finds the correct visitor process for the person id coupled to
%%      the user-agent.

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

% - On first visit:
% 	1. Create new user, flag as anonymous user
% 	2. Add cookie to user, set cookie "zpuid" - valid for 10 years or so
% 	3. Name of user is empty, no details known except for last visit
% 	4. Set autologon of cookie to false - an anonymous user can't logon
% - On next visit:
% 	1. Grab user from db, using zpuid cookie
% 	2. If no such user -> handle as first visit
% 	3. Set 'last visit' of user to now()
% 	4. If autologon status set, mark user session as logged on (protected stuff is visible)
%
% - On user creation:
% 	1. Create new user
% 	2. Send user an e-mail with account details
% 	3. Log on as the new user (see below)
% - On user logon:
% 	1. Find user record with username/password (or openid)
% 	2. Set autologon status of zpuid cookie to checkbox value
% 	3. If current user is anonymous -> Copy/merge public information over to new user
% 	4. Change zpsid and zpuid (safety measure)
% - On user logoff:
% 	1. Set user process state to 'public' (locking protected and private properties)

-module(z_visitor_manager).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([ensure_visitor/1]).

-include_lib("zotonic.hrl").

%% The name of the person cookie
-define(VISITOR_COOKIE, "z_pid").

%% Max age of the person cookie, 10 years or so.
-define(VISITOR_COOKIE_MAX_AGE, 3600*24*52*10).

%% Internal state
-record(state, {pid2key, key2pid}).


%%====================================================================
%% API
%%====================================================================

%% @spec start_link(SiteProps) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the person manager server
start_link(SiteProps) -> 
    {host, Host} = proplists:lookup(host, SiteProps),
    Name = z_utils:name_for_host(?MODULE, Host),
    gen_server:start_link({local, Name}, ?MODULE, SiteProps, []).


%% @doc Ensure that the context has a valid person pid. When there is a session pid then the session is
%%      added to the person process.
%% @spec ensure_visitor(Context) -> Context
ensure_visitor(#context{visitor_manager=VisitorManager} = Context) ->
    gen_server:call(VisitorManager, {ensure_visitor, Context}).



%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server, initialises the pid lookup dicts
init(_Args) ->
    State = #state{pid2key=dict:new(), key2pid=dict:new()},
    {ok, State}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages

%% @doc Ensure that we have a visitor associated with the request.
handle_call({'ensure_visitor', Context}, _From, State) ->
    {Context1, State1} = ensure_visitor_process(Context, State),
    erlang:monitor(process, Context1#context.visitor_pid),
    {reply, Context1, State1};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}

%% @doc Handle the disappearance of a person process
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    State1 = erase_pid(Pid, State),
    {noreply, State1};

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
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


%% @spec ensure_visitor_process(Context, State) -> {NewContex, NewState}
%% @doc Ensure that we have a person process, binding together the sessions of the visitor accessing the site
ensure_visitor_process(Context, State) ->
    % Check if the ua visited this site before
    case get_cookie(Context) of
        undefined -> first_visit(Context, State);
        VisitorCookieId -> returning(VisitorCookieId, Context, State)
    end.
    

%% @spec first_visit(Context, State) -> {NewContex, NewState}
%% @doc A new visit, set a new visitor cookie, start an anonymous visitor process
first_visit(Context, State) ->
    VisitorCookieId = z_ids:id(),
    {ok, Pid} = z_visitor:new_anonymous(VisitorCookieId, Context),
    State1    = store_pid(VisitorCookieId, Pid, State),
    Context1  = set_cookie(VisitorCookieId, Context),
    Context2  = Context1#context{visitor_pid=Pid},
    {Context2, State1}.


%% @spec returning(VisitorCookieId, Context, State) -> {NewContext, NewState}
%% @doc A returning visitor, lookup the person and ensure that the user process is running
returning(VisitorCookieId, Context, State) ->
    case find_pid(VisitorCookieId, State) of
        {ok, Pid} ->
            z_visitor:associate_session(Pid, Context),
            Context1 = Context#context{visitor_pid=Pid},
            {Context1, State};
        error -> 
            % Not started, check if the cookie is associated with a person
            case z_visitor:new_returning(VisitorCookieId, Context) of
                {ok, Pid} ->
                        %% Cookie was associated with a person, person process started
                        State1   = store_pid(VisitorCookieId, Pid, State),
                        Context1 = Context#context{visitor_pid=Pid},
                        {Context1, State1};
                error ->
                        %% Unknown cookie, or person was deleted, handle as a first visit
                        first_visit(Context, State)
            end
    end.


%% @spec store_pid(pid(), State) -> State
%% @doc Add the pid to the person state
store_pid(VisitorCookieId, Pid, State) ->
    State#state{
            pid2key = dict:store(Pid, VisitorCookieId, State#state.pid2key),
            key2pid = dict:store(VisitorCookieId, Pid, State#state.key2pid)
        }.

%% @spec erase_pid(pid(), State) -> State
%% @doc Remove the pid from the person manager state
erase_pid(Pid, State) ->
    case dict:find(Pid, State#state.pid2key) of
        {ok, Key} ->
            State#state{
                    pid2key = dict:erase(Pid, State#state.pid2key),
                    key2pid = dict:erase(Key, State#state.key2pid)
                };
        error ->
            State
    end.


%% @spec get_cookie(Context) -> undefined | VisitorCookieId
%% @doc fetch the person cookie id from the request, return error when not found
get_cookie(Context) ->
    RD = z_context:get_reqdata(Context),
    wrq:get_cookie_value(?VISITOR_COOKIE, RD).


%% @spec set_cookie(VisitorCookieId, Context) -> Context
%% @doc Save the person id in a cookie on the user agent
set_cookie(VisitorCookieId, Context) ->
    RD = z_context:get_reqdata(Context),
    %% TODO: set the {domain,"example.com"} of the session cookie
    Options = [{max_age, ?VISITOR_COOKIE_MAX_AGE}, {path, "/"}],
    Hdr = mochiweb_cookies:cookie(?VISITOR_COOKIE, VisitorCookieId, Options),
    RD1 = wrq:merge_resp_headers([Hdr], RD),
    z_context:set_reqdata(RD1, Context).


%% @spec find_pid(VisitorCookieId, State) ->  error | {ok, pid()}
%% @doc find the pid associated with the visitor id
find_pid(undefined, _State) ->
    error;
find_pid(VisitorCookieId, State) ->
    dict:find(VisitorCookieId, State#state.key2pid).

