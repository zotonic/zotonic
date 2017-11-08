%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011-2015 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-10-12

%% @doc Watch for changed files using inotifywait.
%%      https://github.com/rvoicilas/inotify-tools/wiki

%% Copyright 2011-2015 Arjan Scherpenisse
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

-module(zotonic_filewatcher_inotify).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("zotonic_core/include/zotonic.hrl").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-record(state, {
    pid,
    port,
    executable
}).

%% interface functions
-export([
    is_installed/0
]).


%%====================================================================
%% API
%%====================================================================
%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    case os:find_executable("inotifywait") of
        false ->
            {error, "inotifywait not found"};
        Executable ->
            gen_server:start_link({local, ?MODULE}, ?MODULE, [Executable], [])
    end.

-spec is_installed() -> boolean().
is_installed() ->
    os:find_executable("inotifywait") =/= false.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init([Executable]) ->
    process_flag(trap_exit, true),
    State = #state{
        executable = Executable,
        port = undefined,
        pid = undefined
    },
    timer:send_after(100, start),
    {ok, State}.


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
handle_info({stdout, Port, Data}, #state{ port = Port } = State) ->
    Lines = binary:split(
        binary:replace(Data, <<"\r\n">>, <<"\n">>, [global]),
        <<"\n">>,
        [global]),
    lists:map(
        fun(Line) ->
            case re:run(Line, "^(.+) (MODIFY|CREATE|DELETE) (.+)", [{capture, all_but_first, binary}]) of
                nomatch ->
                    ok;
                {match, [Path, Verb, File]} ->
                    Filename = filename:join(Path, File),
                    zotonic_filewatcher_handler:file_changed(verb(Verb), Filename)
            end
        end,
        Lines),
    {noreply, State};

handle_info({'DOWN', _Port, process, Pid, Reason}, #state{pid = Pid} = State) ->
    lager:error("[inotify] inotify port closed with ~p, restarting in 5 seconds.", [Reason]),
    State1 = State#state{
        pid = undefined,
        port = undefined
    },
    timer:send_after(5000, start),
    {noreply, State1};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};

handle_info(start, #state{ port = undefined } = State) ->
    lager:info("[inotify] Starting inotify file monitor."),
    {noreply, start_inotify(State)};
handle_info(start, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, #state{pid = undefined}) ->
    ok;
terminate(_Reason, #state{pid = Pid}) ->
    catch exec:stop(Pid),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

start_inotify(#state{executable = Executable, port = undefined} = State) ->
    lager:info("[inotify] Starting inotify file monitor."),
    Args = [
        Executable,
        "-q", "-e", "modify,create,delete", "-m", "-r"
        | zotonic_filewatcher_sup:watch_dirs()
    ],
    {ok, Pid, Port} = exec:run_link(Args, [stdout, monitor]),
    State#state{
        port = Port,
        pid = Pid
    };
start_inotify(State) ->
    State.

verb(<<"CREATE">>) -> create;
verb(<<"MODIFY">>) -> modify;
verb(<<"DELETE">>) -> delete.
