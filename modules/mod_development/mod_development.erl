%% @author Marc Worrell <marc@worrell.nl>
%% @author Bryan Fink <bryan@basho.com>
%% @copyright 2009 Marc Worrell
%% Date: 2009-11-01
%% @doc Development server.  Periodically performs a "make" and loads new files.
%% When new files are loaded the caches are emptied.

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

-module(mod_development).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Development").
-mod_description("Development support, periodically builds and loads changed files.").
-mod_prio(1000).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    reload/1,
    make/1,
    debug_stream/3,
    observe_debug_stream/2,
    pid_observe_development_reload/3,
    pid_observe_development_make/3,
    
    % internal (for spawn)
    page_debug_stream/3,
    page_debug_stream_loop/3
]).

-include_lib("zotonic.hrl").

-record(state, {context}).

% Interval for checking for new and/or changed files.
-define(DEV_POLL_INTERVAL, 10000).


%%====================================================================
%% API
%%====================================================================

%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).


reload(Context) ->
    z_notifier:notify(development_reload, Context).

make(Context) ->
    z_notifier:notify(development_make, Context).

%% @doc Stream specific debug information to an area on the current page.
debug_stream(TargetId, What, Context) ->
    z_notifier:notify1(#debug_stream{target=TargetId, what=What}, Context).

%% @doc Stream all debug information of a certain kind to the target id on the user agent.
observe_debug_stream(#debug_stream{target=TargetId, what=What}, Context) ->
    start_debug_stream(TargetId, What, Context).

pid_observe_development_reload(Pid, development_reload, _Context) ->
    gen_server:cast(Pid, development_reload).

pid_observe_development_make(Pid, development_make, _Context) ->
     gen_server:cast(Pid, development_make).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    timer:send_interval(?DEV_POLL_INTERVAL, ensure_server),

    z_sass_converter:start_link(Context),

    {ok, #state{
        context  = z_context:new(Context)
    }}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
handle_cast(development_reload, State) ->
    ensure_dev_server(),
    z_development_server:reload(),
    {noreply, State};

handle_cast(development_make, State) ->
    ensure_dev_server(),
    z_development_server:make(),
    {noreply, State};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Periodic check if the dev server is still running.
handle_info(ensure_server, State) ->
    ensure_dev_server(),
    z_utils:flush_message(ensure_server), 
    {noreply, State};

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


%% @doc Ensure that a dev server is running.  If it is not running then the server is started and
%% this module process is linked to the server.
ensure_dev_server() ->
	case erlang:whereis(z_development_server) of
		undefined -> z_development_server:start_link();
		_Pid -> ok
	end.


%% @doc Start a listener for a certain kind of debug information, echo it to the target id on the current page.
start_debug_stream(TargetId, What, Context) ->
    Context1 = z_context:prune_for_async(Context),
    z_session_page:spawn_link(?MODULE, page_debug_stream, [TargetId, What, Context1], Context1).

%% @doc Process started and linked to the current page, subscribes to debug notifications
page_debug_stream(TargetId, What, Context) ->
    process_flag(trap_exit, true),
    z_notifier:observe(debug, self(), Context),
    ?MODULE:page_debug_stream_loop(TargetId, What, Context).
    
    page_debug_stream_loop(TargetId, What, Context) ->
        receive
            {'EXIT', _} ->
                z_notifier:detach(debug, self(), Context),
                done;
            {'$gen_cast', {#debug{what=What, arg=Arg}, _Context}} ->
                %% Update the target id with a dump of this debug message
                S = io_lib:format("~p: ~p~n", [What, Arg]),
                z_session_page:add_script(z_render:insert_top(TargetId, S, Context)),
                ?MODULE:page_debug_stream_loop(TargetId, What, Context);
            {'$gen_cast', {#debug{what=_Other}, _Context}} ->
                ?MODULE:page_debug_stream_loop(TargetId, What, Context)
        end.
        
