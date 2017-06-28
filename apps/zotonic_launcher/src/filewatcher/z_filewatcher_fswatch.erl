%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2014-2015 Arjan Scherpenisse <arjan@scherpenisse.net>

%% @doc Watch for changed files using fswatch (MacOS X; brew install fswatch).
%%      https://github.com/emcrisostomo/fswatch

%% Copyright 2014-2015 Arjan Scherpenisse
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

-module(z_filewatcher_fswatch).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("zotonic_core/include/zotonic.hrl").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-record(state, {port, executable, timers=[]}).

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
    case os:find_executable("fswatch") of
        false ->
            {error, "fswatch not found"};
        Executable ->
            gen_server:start_link({local, ?MODULE}, ?MODULE, [Executable], [])
    end.

-spec is_installed() -> boolean().
is_installed() ->
    os:find_executable("fswatch") =/= false.

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
    State = #state{executable=Executable},
    os:cmd("killall fswatch"),
    {ok, State, 0}.


%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @doc Reading a line from the fswatch program.
handle_info({Port, {data, FilenameFlags}}, State=#state{port=Port, timers=Timers}) ->
    Fs = extract_filename_verb(FilenameFlags),
    {Timers1,_N1} = lists:foldl(
                        fun({Filename, Verb}, {TimersAcc,N}) ->
                            {z_filewatcher_handler:set_timer(Filename, Verb, TimersAcc, N), N+100}
                        end,
                        {Timers, 0},
                        Fs),
    {noreply, State#state{timers=Timers1}};

%% @doc Launch the actual filechanged notification
handle_info({filechange, Verb, Filename}, State=#state{timers=Timers}) ->
    z_filewatcher_handler:file_changed(Verb, Filename),
    {noreply, State#state{timers=lists:keydelete(Filename, 1, Timers)}};

handle_info({Port,{exit_status,Status}}, State=#state{port=Port}) ->
    lager:error("[fswatch] fswatch port closed with ~p, restarting in 5 seconds.", [Status]),
    {noreply, State, 5000};

handle_info({'EXIT', Port, _}, State=#state{port=Port}) ->
    lager:error("[fswatch] fswatch port closed, restarting in 5 seconds."),
    {noreply, State, 5000};

handle_info(timeout, #state{port=undefined} = State) ->
    lager:info("[fswatch] Starting fswatch file monitor."),
    {noreply, start_fswatch(State)};
handle_info(timeout, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    ?DEBUG(_Info),
    ?DEBUG({port, State#state.port}),
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, #state{port=undefined}) ->
    ok;
terminate(_Reason, #state{port=Port}) ->
    catch erlang:port_close(Port),
    os:cmd("killall fswatch"),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

start_fswatch(State=#state{executable=Executable}) ->
    % os:cmd("killall fswatch"),
    Args = ["-0", "-x", "-r", "-L" | z_filewatcher_sup:watch_dirs() ],
    Port = erlang:open_port({spawn_executable, Executable}, [{args, Args}, stream, exit_status, binary]),
    State#state{port=Port}.

extract_filename_verb(Line) ->
    Lines = binary:split(Line, <<0>>, [global]),
    lists:foldr(fun split_line/2, [], Lines).

split_line(<<>>, Acc) ->
    Acc;
split_line(Line, Acc) ->
	% get a file path that may include spaces
	Filepath = get_filepath(Line),
	% extract a verb from the line, while ignoring strings that are not verbs
	[_|Flags] = binary:split(Line, <<" ">>, [global]),
	Verb = extract_verb(Flags),
    [{Filepath, Verb} | Acc].

%% Remove verbs from line, preserve spaces
get_filepath(Line) ->
	Space = <<" ">>,
	Parts = lists:foldl(fun(Part, Acc) ->
		case extract_filepath(Part) of
			<<>> -> Acc;
			P -> <<Acc/binary, P/binary, Space/binary>>
		end
	end, <<>>, binary:split(Line, Space, [global])),
	string:strip(unicode:characters_to_list(Parts), both, $ ).

% Remove all known verbs; the remainder must be the file path
% of course this breaks when new event names are added to fswatch
extract_filepath(<<>>) -> <<>>;
extract_filepath(<<"PlatformSpecific">>) -> <<>>;
extract_filepath(<<"AttributeModified">>) -> <<>>;
extract_filepath(<<"Created">>) -> <<>>;
extract_filepath(<<"Updated">>) -> <<>>;
extract_filepath(<<"Removed">>) -> <<>>;
extract_filepath(<<"Renamed">>) -> <<>>;
extract_filepath(<<"OwnerModified">>) -> <<>>;
extract_filepath(<<"MovedFrom">>) -> <<>>;
extract_filepath(<<"MovedTo">>) -> <<>>;
extract_filepath(<<"IsFile">>) -> <<>>;
extract_filepath(<<"IsDir">>) -> <<>>;
extract_filepath(<<"IsSymLink">>) -> <<>>;
extract_filepath(<<"Link">>) -> <<>>;
extract_filepath(F) -> F.

extract_verb([]) -> modify;
extract_verb([<<"Removed">>, <<"Renamed">> | _ ]) -> modify;
extract_verb([<<"Created">>|_]) -> create;
extract_verb([<<"Removed">>|_]) -> delete;
extract_verb([<<"MovedFrom">>|_]) -> delete;
extract_verb([<<"MovedTo">>|_]) -> create;
extract_verb([<<"Renamed">>|_]) -> create;
extract_verb([_|Flags]) -> extract_verb(Flags).
