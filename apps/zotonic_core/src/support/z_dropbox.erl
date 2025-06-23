%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Simple drop folder handler, monitors a directory and signals new files.
%%
%% Flow:
%% 1. a user uploads/moves a file to the drop folder directory
%% 2. Drop folder handler sees the file, moves it so a safe place, and notifies the file handler of it existance.
%% @end

%% Copyright 2009-2023 Marc Worrell
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

-module(z_dropbox).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    scan/1
]).

%% internal
-export([]).

-include_lib("zotonic.hrl").

-record(state, {
    dropbox_dir :: binary(),
    processing_dir :: binary(),
    unhandled_dir :: binary(),
    handled_dir :: binary(),
    min_age :: integer(),
    max_age :: integer(),
    site :: atom(),
    context :: z:context()
}).

-define(FILE_MIN_AGE, 2).
-define(FILE_MAX_AGE, 3600).
-define(SCAN_INTERVAL, 10000).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(SiteArgs) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the drop folder server
start_link(Site) ->
    Name = z_utils:name_for_site(?MODULE, Site),
    gen_server:start_link({local, Name}, ?MODULE, Site, []).


%% @doc Perform a scan of the drop folder, periodically called by a timer and by mod_admin after
%% a file has been uploaded to the dropbox.
-spec scan(Context) -> ok when
    Context :: z:context().
scan(Context) ->
    gen_server:cast(Context#context.dropbox_server, scan).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(SiteProps) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.  Options are: dropbox_dir, processing_dir, unhandled_dir, interval, max_age and min_age
init(Site) ->
    logger:set_process_metadata(#{
        site => Site,
        module => ?MODULE
    }),
    Context = z_context:new(Site),
	DefaultDropBoxDir = z_path:files_subdir_ensure(<<"dropbox">>, Context),
	DefaultProcessingDir = z_path:files_subdir_ensure(<<"processing">>, Context),
	DefaultUnhandledDir = z_path:files_subdir_ensure(<<"unhandled">>, Context),
    DefaultHandledDir = z_path:files_subdir_ensure(<<"handled">>, Context),
    DropBox    = z_string:trim_right(config(dropbox_dir,            Context, DefaultDropBoxDir),    $/),
    ProcDir    = z_string:trim_right(config(dropbox_processing_dir, Context, DefaultProcessingDir), $/),
    UnDir      = z_string:trim_right(config(dropbox_unhandled_dir,  Context, DefaultUnhandledDir),  $/),
    HandledDir = z_string:trim_right(config(dropbox_handled_dir,    Context, DefaultHandledDir),  $/),
    State    = #state{
        dropbox_dir = DropBox,
        processing_dir = ProcDir,
        unhandled_dir = UnDir,
        handled_dir = HandledDir,
        min_age = z_convert:to_integer(config(dropbox_min_age, Context, ?FILE_MIN_AGE)),
        max_age = z_convert:to_integer(config(dropbox_max_age, Context, ?FILE_MAX_AGE)),
        site = Site,
        context = Context
    },
    Interval = z_convert:to_integer(config(dropbox_interval, Context, ?SCAN_INTERVAL)),
    timer:apply_interval(Interval, ?MODULE, scan, [Context]),
    gen_server:cast(self(), cleanup),
    {ok, State}.

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
%%                                  {stop, eason, State}
%% @doc Scan the drop folder, broadcast found files.
handle_cast(scan, State) ->
    do_scan(State),
    z_utils:flush_message({'$gen_cast', scan}),
    {noreply, State};

% Move all files in the processing directory to the unhandled directory
handle_cast(cleanup, #state{ processing_dir = ProcDir, unhandled_dir = UnDir } = State) ->
    lists:foreach(fun(F) ->
                       move_file(ProcDir, F, true, UnDir)
                  end,
                  scan_directory(ProcDir)),
    {noreply, State};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
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

config(Key, Context, Default) ->
    case m_site:get(Key, Context) of
        undefined -> Default;
        V -> V
    end.

%% @doc Perform a scan of the drop folder, broadcast all to be processed files.
-spec do_scan( #state{} ) -> ok.
do_scan(State) ->
    #state{
        processing_dir = ProcDir,
        dropbox_dir = DropDir,
        unhandled_dir = UnhandledDir,
        handled_dir = HandledDir,
        min_age = MinAge,
        max_age = MaxAge
    } = State,

    % Move all old files in the processing directory to the unhandled directory
    ProcFiles = scan_directory(ProcDir),
    {ToProcess,ToRemove} = lists:foldl(fun(F, Acc) -> max_age_split(F, MaxAge, Acc) end,
                                       {[],[]},
                                       ProcFiles),
    lists:foreach(fun(F) -> move_file(ProcDir, F, true, UnhandledDir) end, ToRemove),

    % Move all new drop folder files to the processing directory
    AllDropFiles  = scan_directory(DropDir),
    SafeDropFiles = lists:foldl(fun(F, Acc)-> min_age_check(F, MinAge, Acc) end,
                                [],
                                AllDropFiles),
    Moved      = lists:map(fun(F) -> {F,move_file(DropDir, F, false, ProcDir)} end, SafeDropFiles),
    ToProcess1 = lists:foldl(   fun
                                    ({_, {ok, File}}, Acc) ->
                                        [File|Acc];
                                    ({F, {error, Reason}}, Acc) ->
                                        ?LOG_WARNING(#{
                                            text => <<"z_dropbox: Failed to move file">>,
                                            filename => F,
                                            to_dir => ProcDir,
                                            result => error,
                                            reason => Reason
                                        }),
                                        Acc
                                end,
                                ToProcess,
                                Moved),
    lists:foreach(
        fun(File) ->
            File1 = unicode:characters_to_binary(File),
            Basename = unicode:characters_to_binary(filename:basename(File)),
            case z_notifier:first(#dropbox_file{
                    filename = File1,
                    basename = Basename
                }, State#state.context)
            of
                undefined ->
                    ?LOG_WARNING(#{
                        in => zotonic_core,
                        text => <<"Drop folder file was not handled by modules, moved to unhandled">>,
                        result => error,
                        reason => no_handler,
                        file => File1,
                        basename => Basename
                    }),
                    move_file(ProcDir, File1, true, UnhandledDir);
                ok ->
                    move_file(ProcDir, File1, true, HandledDir);
                _ ->
                    % Retry next time
                    ok
            end
        end,
        ToProcess1).


%% @doc Scan a directory, return list of files not changed in the last 10 seconds.
scan_directory(Dir) ->
    Fs = filelib:fold_files(unicode:characters_to_list(Dir), "", true, fun(F,Acc) -> append_file(F, Acc) end, []),
    [ unicode:characters_to_binary(F) || F <- Fs ].


%% @doc Check if this is a file we are interested in, should not be part of a .svn or other directory
-spec append_file( file:filename_all(), list( file:filename_all() ) ) -> list( file:filename_all() ).
append_file(Filename, Acc) ->
    Parts = filename:split(Filename),
    case lists:any(fun is_dotfile/1, Parts) of
        true -> Acc;
        false -> [ Filename | Acc ]
    end.

is_dotfile(<<"..">>) -> false;
is_dotfile(<<".", _/binary>>) -> true;
is_dotfile("..") -> false;
is_dotfile("." ++ _) -> true;
is_dotfile(_) -> false.


min_age_check(File, MinAge, Acc) ->
    Mod     = filelib:last_modified(File),
    ModSecs = calendar:datetime_to_gregorian_seconds(Mod),
    Now     = calendar:local_time(),
    NowSecs = calendar:datetime_to_gregorian_seconds(Now),
    case NowSecs - ModSecs > MinAge of
        true -> [File|Acc];
        false -> Acc
    end.

max_age_split(File, MaxAge, {AccNew, AccOld}) ->
    Mod     = filelib:last_modified(File),
    ModSecs = calendar:datetime_to_gregorian_seconds(Mod),
    Now     = calendar:local_time(),
    NowSecs = calendar:datetime_to_gregorian_seconds(Now),
    case NowSecs - ModSecs > MaxAge of
        true ->  {AccNew,        [File|AccOld]};
        false -> {[File|AccNew], AccOld}
    end.


%% @spec move_file(BaseDir, File, DeleteTarget, ToDir) -> {ok, NewFile} | {error, Reason}
%% @doc Move a file relative to one directory to another directory
move_file(BaseDir, File, DeleteTarget, ToDir) ->
    Rel    = rel_file(BaseDir, File),
    Target = filename:join(ToDir,Rel),
    case filelib:is_dir(Target) of
        true -> file:del_dir(Target);
        false -> ok
    end,
    case DeleteTarget of
        true -> file:delete(Target);
        false -> ok
    end,
    case filelib:is_regular(Target) of
        false ->
            case z_filelib:ensure_dir(Target) of
                ok ->
                    case file:rename(File,Target) of
                        ok -> {ok, Target};
                        Error -> Error
                    end;
                Error ->
                    Error
            end;
        true ->
            {error, eexist}
    end.

%% @doc Return the relative path of the file to a BaseDir
rel_file(BaseDir, File) ->
    Size = size(BaseDir),
    case binary:longest_common_prefix([ BaseDir, File ]) of
        Size ->
            <<_:Size/binary, _Sep, Rest/binary>> = File,
            Rest;
        _ ->
            filename:basename(File)
    end.
