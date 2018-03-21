%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2014-2018 Arjan Scherpenisse <arjan@scherpenisse.net>

%% @doc Watch for changed files using fswatch (MacOS X; brew install fswatch).
%%      https://github.com/emcrisostomo/fswatch

%% Copyright 2014-2018 Arjan Scherpenisse
%% Copyright 2015-2018 Marc Worrell
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

-module(zotonic_filewatcher_fswatch).
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
    is_installed/0,
    restart/0
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

-spec restart() -> ok.
restart() ->
    gen_server:cast(?MODULE, restart).

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
handle_cast(restart, #state{ pid = undefined } = State) ->
    {noreply, State};
handle_cast(restart, #state{ pid = Pid } = State) when is_pid(Pid) ->
    lager:info("[inotify] Stopping fswatch file monitor."),
    catch exec:stop(Pid),
    {noreply, start_fswatch(State#state{ port = undefined })};

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @doc Reading a line from the fswatch program.
handle_info({stdout, _Port, FilenameFlags}, #state{} = State) ->
    lists:map(
        fun({Filename, Verb}) ->
            zotonic_filewatcher_handler:file_changed(Verb, Filename)
        end,
        extract_filename_verb(FilenameFlags)),
    {noreply, State};

handle_info({'DOWN', _Port, process, Pid, Reason}, #state{pid = Pid} = State) ->
    lager:error("[fswatch] fswatch port closed with ~p, restarting in 5 seconds.", [Reason]),
    State1 = State#state{
        pid = undefined,
        port = undefined
    },
    timer:send_after(5000, start),
    {noreply, State1};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};

handle_info(start, #state{port = undefined} = State) ->
    {noreply, start_fswatch(State)};
handle_info(start, State) ->
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

start_fswatch(State=#state{executable = Executable, port = undefined}) ->
    lager:info("[fswatch] Starting fswatch file monitor."),
    REs = lists:foldl(
        fun(RE, Acc) ->
            [ "-e", RE | Acc ]
        end,
        [],
        string:tokens(zotonic_filewatcher_handler:re_exclude(), "|")),
    Args = [ Executable, "-0", "-x", "-Lr" ]
        ++ REs
        ++ zotonic_filewatcher_sup:watch_dirs_expanded(),
    {ok, Pid, Port} = exec:run_link(Args, [stdout, monitor]),
    State#state{
        port = Port,
        pid = Pid
    };
start_fswatch(State) ->
    State.

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
    Verb = case extract_verb(Flags) of
        create ->
            % Deletes and renames are sometimes seen as a create
            case filelib:is_file(Filepath) of
                true -> create;
                false -> delete
            end;
        V ->
            V
    end,
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
