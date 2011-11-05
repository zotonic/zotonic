%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-10-12

%% @doc Watch for changed files using inotifywait.

%% Copyright 2011 Arjan Scherpenisse
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

-module(z_filewatcher_inotify).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("include/zotonic.hrl").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-record(state, {port, context, timers=[]}).

%% interface functions
-export([
]).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Context=#context{}) ->
    case os:cmd("which inotifywait") of
        [] ->
            {error, "inotifywait not found"};
        _ ->
            case whereis(?MODULE) of
                undefined ->
                    gen_server:start_link({local, ?MODULE}, ?MODULE, Context, []);
                Pid ->
                    {ok, Pid}
            end
    end.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Context) ->
    process_flag(trap_exit, true),
    {ok, #state{context=Context, port=start_inotify()}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.




%% @doc Reading a line from the inotifywait program. Sets a timer to
%% prevent duplicate file changed message for the same filename
%% (e.g. if a editor saves a file twice for some reason).
handle_info({Port, {data, {eol, Line}}}, State=#state{port=Port, timers=Timers}) ->
    case re:run(Line, "^(.+) (MODIFY|CREATE) (.+)", [{capture, all_but_first, list}]) of
        nomatch -> 
            {noreply, State};
        {match, [Path, Verb, File]} ->
            Filename = filename:join(Path, File),
            Timers1 = case proplists:lookup(Filename, Timers) of
                          {Filename, TRef} ->
                              erlang:cancel_timer(TRef),
                              proplists:delete(Filename, Timers);
                          none ->
                              Timers
                      end,
            TRef2 = erlang:send_after(300, self(), {filechange, verb(Verb), Filename}),
            Timers2 = [{Filename, TRef2} | Timers1],
            {noreply, State#state{timers=Timers2}}
    end;

%% @doc Launch the actual filechanged notification
handle_info({filechange, Verb, Filename}, State=#state{timers=Timers}) ->
    mod_development:file_changed(Verb, Filename),
    {noreply, State#state{timers=proplists:delete(Filename, Timers)}};

handle_info({'EXIT', Port, _}, State=#state{port=Port}) ->
    ?DEBUG("restart inotify"),
    {noreply, State#state{port=start_inotify()}};

handle_info(_Info, State) ->
    ?DEBUG(_Info),
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, {_, Port}) ->
    true = erlang:port_close(Port),
    os:cmd("killall inotifywait"),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


start_inotify() ->
    os:cmd("killall inotifywait"),
    Args = ["-e", "modify,create", "-m", "-r",
            filename:join(os:getenv("ZOTONIC"), "src"),
            filename:join(os:getenv("ZOTONIC"), "priv/sites"),
            filename:join(os:getenv("ZOTONIC"), "modules")
            |
            string:tokens(os:cmd("find " ++ z_utils:os_escape(os:getenv("ZOTONIC")) ++ " -type l"), "\n")],
    erlang:open_port({spawn_executable, "/usr/bin/inotifywait"}, [{args, Args}, {line, 1024}]).


verb("MODIFY") -> modify;
verb("CREATE") -> create.
