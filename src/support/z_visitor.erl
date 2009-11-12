%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Visitor process. This server binds all sessions of a visitor together.

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

-module(z_visitor).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([new_anonymous/2, new_returning/2, associate_session/2, get/2, set/3, ensure_visitor_id/1]).

-include_lib("zotonic.hrl").

-record(state, {
    sessions=[],
    cookie=undefined,
    associate_count=1,
    id=undefined,
    rsc_id=undefined,
    props=[],
    dirty=false,
    visitor_id=undefined,
    context=undefined
}).

%%====================================================================
%% API
%%====================================================================

%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the person process server
start_link() ->
    start_link([]).
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).


%% @spec new_anonymous(CookieId, Context) -> {ok, pid()}
%% @doc Start a new anonymous user process, link to the new process.
new_anonymous(CookieId, Context) ->
    SessionPid = Context#context.session_pid,
    start_link([{session, SessionPid}, {cookie, CookieId}, {is_new, true}, {context, z_context:new(Context)}]).


%% @spec new_returning(CookieId, SessionPid) -> {ok, pid()} | error
%% @doc Start a new process for the visitor associated with the cookie, return error when no person associated.
new_returning(CookieId, Context) ->
    SessionPid = Context#context.session_pid,
    start_link([{session, SessionPid}, {cookie, CookieId}, {is_new, false}, {context, z_context:new(Context)}]).


%% @spec associate_session(Pid, SessionPid) -> void()
%% @doc Associate a new session with this visitor, monitor the session so that we known when the session is gone.
associate_session(Pid, SessionPid) when is_pid(Pid) ->
    gen_server:cast(Pid, {associate_session, SessionPid});
associate_session(_Pid, _SessionPid) -> 
    ok.


%% @doc Set a property of the visitor information
%% @spec set(Pid, Key, Value) -> void()
set(Key, Value, Pid) ->
    gen_server:cast(Pid, {set, Key, Value}).

%% @doc Get a property from the visitor information
%% @spec get(Pid, Key) -> term()
get(Key, Pid) ->
    gen_server:call(Pid, {get, Key}).


%% @doc Force the visitor to have an id, return the id.
%% @spec get_id(Pid) -> integer()
ensure_visitor_id(Pid) ->
    gen_server:call(Pid, ensure_visitor_id).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    State = #state{
        cookie=proplists:get_value(cookie, Args),
        context=proplists:get_value(context, Args)
    },
    State1 = add_session(proplists:get_value(session, Args), State),
    {Id, RscId, Props} = case proplists:get_value(is_new, Args, false) of
        false -> get_props(State1);
        true  -> {undefined, undefined, []}
    end,
    {ok, State1#state{props=Props, id=Id, rsc_id=RscId}, ?VISITOR_TIMEOUT}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
handle_call(ensure_visitor_id, _From, State) ->
    case State#state.id of
        undefined ->
            State1 = save_props(State#state{associate_count=2, dirty=true}),
            {reply, State1#state.id, State1, ?VISITOR_TIMEOUT};
        Id ->
            {reply, Id, State, ?VISITOR_TIMEOUT}
    end;

handle_call({get, Key}, _From, State) ->
    {reply, proplists:get_value(Key, State#state.props), State, ?VISITOR_TIMEOUT};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State, ?VISITOR_TIMEOUT}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}

handle_cast({associate_session, SessionPid}, State) when is_pid(SessionPid) ->
    State1 = add_session(SessionPid, State),
    {noreply, State1#state{associate_count=State1#state.associate_count+1}, ?VISITOR_TIMEOUT};
handle_cast({associate_session, _SessionPid}, State) ->
    {noreply, State#state{associate_count=State#state.associate_count+1}, ?VISITOR_TIMEOUT};

handle_cast({set, Key, Value}, State) ->
    Props = z_utils:prop_replace(Key, Value, State#state.props),
    {noreply, State#state{props=Props, dirty=true}, ?VISITOR_TIMEOUT};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State, ?VISITOR_TIMEOUT}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}

%% @doc Handle the disappearance of a session process
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    RemainingSessions = lists:delete(Pid, State#state.sessions),
    {noreply, State#state{sessions=RemainingSessions}, ?VISITOR_TIMEOUT};

%% @doc Handle a timeout after a period of inactivity, stop the process when no sessions left
handle_info(timeout, State) ->
    State1 = save_props(State),
    case length(State1#state.sessions) of
        0 -> {stop, normal, State1};
        _ -> {noreply, State1, ?VISITOR_TIMEOUT}
    end;

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State, ?VISITOR_TIMEOUT}.


%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    save_props(State).

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @spec add_session(pid(), State) -> NewState
%% @doc Add a session to the person session, ignore when undefined or pid is already known
add_session(undefined, State) ->
    State;
add_session(SessionPid, State) ->
    case lists:member(SessionPid, State#state.sessions) of
        false ->
            erlang:monitor(process, SessionPid),
            State#state{sessions=[SessionPid|State#state.sessions]};
        true ->
            State
    end.


%% @doc Read the visitor properties from the database
%% @spec get_props(State) -> {rsc_id, PropList}
get_props(State) ->
    case z_db:q("
        select v.id, v.rsc_id, v.props
        from visitor v join visitor_cookie vc on v.id = vc.visitor_id
        where vc.cookie = $1", [State#state.cookie], State#state.context) 
    of
        [{Id, RscId, P}] when is_list(P) -> {Id, RscId, P};
        [{Id, RscId, _}] -> {Id, RscId, []};
        [] -> {undefined, undefined, []}
    end.


%% @doc Save the properties when they are changed and the associate_count > 1
%% @spec save_props(State) -> State
save_props(#state{associate_count=Count, dirty=Dirty} = State) when Dirty andalso Count > 1 ->
    case State#state.id of
        undefined ->
            F = fun(Ctx) ->
                {ok, Id} = z_db:insert(visitor, [
                            {props, State#state.props}, 
                            {rsc_id, State#state.rsc_id}], Ctx),
                {ok, _} = z_db:insert(visitor_cookie, [
                            {cookie, State#state.cookie}, 
                            {visitor_id, Id}], Ctx),
                {ok, Id}
            end,
            {ok, Id} = z_db:transaction(F, State#state.context),
            State#state{id=Id, dirty=false};
        Id ->
            z_db:update(visitor, Id, [
                            {props, State#state.props}, 
                            {rsc_id, State#state.rsc_id}, 
                            {modified, calendar:universal_time()}], State#state.context),
            State#state{dirty=false}
    end;
save_props(State) ->
    State.
