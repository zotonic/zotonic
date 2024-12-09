%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Periodically loads modules whose beam file have been updated.
%% @end

%% Copyright 2009-2024 Marc Worrell
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

-module(zotonic_filewatcher_beam_reloader).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

%% interface functions
-export([
    reload/0,
    make/0,
    reload_module/1
]).

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").

% The state record for this server
-record(state, {
    start_time :: calendar:datetime()
}).

% Interval for checking for new and/or changed BEAM files.
-define(DEV_POLL_INTERVAL, 10000).


%%====================================================================
%% API
%%====================================================================
%% @doc Starts the server
start_link() ->
    case erlang:whereis(?MODULE) of
        undefined ->
            gen_server:start_link({local, ?MODULE}, ?MODULE, [], []);
        Pid ->
            {ok, Pid}
    end.


%% @doc Check if beam files are changed, load the changed ones.
reload() ->
    gen_server:cast(?MODULE, reload).

%% @doc Perform a make:all() and reload the changed beam files.
make() ->
    gen_server:cast(?MODULE, make).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init([]) ->
    init_scanner(),
    {ok, #state{
        start_time=calendar:local_time()
    }}.

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


handle_cast(reload, State) ->
    reload_all(),
    {noreply, State};
handle_cast(make, State) ->
    make_all(),
    reload_all(),
    {noreply, State};
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @doc Handling all non call/cast messages
handle_info({zotonic_filewatcher_monitor, _Ref, {changed, Path, file, _FileInfo, _Diff}}, State) ->
    zotonic_filewatcher_handler:file_changed(modify, Path),
    {noreply, State};
handle_info({zotonic_filewatcher_monitor, _Ref, {found, Path, file, FileInfo, _Diff}}, #state{start_time=StartTime} = State) ->
    case StartTime < FileInfo#file_info.mtime of
        true -> zotonic_filewatcher_handler:file_changed(create, Path);
        false -> ok
    end,
    {noreply, State};
handle_info({zotonic_filewatcher_monitor, _Ref, {changed, Path, directory, _FileInfo, Diff}}, State) ->
    lists:foreach(
            fun
                ({deleted, File}) ->
                    FilePath = filename:join([Path, File]),
                    zotonic_filewatcher_handler:file_changed(delete, FilePath);
                ({added, File}) ->
                    FilePath = filename:join([Path, File]),
                    zotonic_filewatcher_handler:file_changed(create, FilePath)
            end,
            Diff),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Check if any of the loaded modules has been changed. If so reload the module's beam file. Also rescan for templates and files.
reload_all() ->
    case reload_loaded_modules() of
        [] ->
            z_sites_dispatcher:update_dispatchinfo(),
            [ z_module_indexer:reindex(C) || C <- z_sites_manager:get_site_contexts() ],
            ok;
        _ ->
            z:flush()
    end.


%% @doc Remake beam files from source. Do not load the new files (yet).
make_all() ->
    make:all([]),
    ok.

%% @doc Reload a module, purge the old code.
reload_module(M) ->
    ?LOG_DEBUG(#{
        text => <<"Reloading module">>,
        in => zotonic_filewatcher,
        module => M
    }),
    code:purge(M),
    code:soft_purge(M),
    case code:load_file(M) of
        {module, M} ->
            {ok, M};
        {error, Reason} = Error ->
            ?LOG_WARNING(#{
                text => <<"Could not reload module">>,
                in => zotonic_filewatcher,
                module => M,
                result => error,
                reason => Reason
            }),
            Error
    end.

%% @doc Reload all modules from the zotonic directory or subdirectories.  Return a list of modules reloaded. Empty list when nothing changed.
%% @spec reload_loaded_modules() -> [Result]
reload_loaded_modules() ->
    Dir = z_utils:lib_dir(),
    Modules = [{M,P} || {M, P} <- code:all_loaded(), is_list(P) andalso string:str(P, Dir) > 0],
    [reload_module(M) || {M,Path} <- Modules, module_changed(M,Path)].

%% @doc Check if the version number of the module has been changed.  Skip template modules.
%% @spec module_changed(atom(), filename()) -> bool()
module_changed(Module, BeamFile) ->
    case z_template:is_template_module(Module) of
        true ->
            false;
        false ->
            Props = Module:module_info(attributes),
            case proplists:get_value(vsn, Props) of
                undefined ->
                    false;
                Version ->
                    case beam_lib:version(BeamFile) of
                        {ok, {_Module, Version}} -> false;
                        {ok, {_Module, _OtherVersion}} -> true;
                        {error, _, _} -> false
                    end
            end
    end.

%% @doc Initialize the filewatcher monitor, this one uses polling
init_scanner() ->
    Dirs = zotonic_filewatcher_sup:watch_dirs(),
    lists:foreach(fun(Dir) ->
                    _ = zotonic_filewatcher_monitor:automonitor(Dir)
                  end,
                  Dirs).
