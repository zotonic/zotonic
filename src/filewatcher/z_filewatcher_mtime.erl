%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell <marc@worrell.nl>
%% @doc Keep a registration of file modification times, especially for z_template

%% Copyright 2015 Marc Worrell
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

%% The modification times are administrated in a couple of ets tables.
%%
%% MTIME holds pairs {filename, timestamp} for all modified files
%% FILE_TEMPLATE holds pairs {filename, template_module_name}
%% TEMPLATE_MODIFIED holds pairs {template_module_name, timestamp}
%%
%% If a template is compiled then all its dependencies are added to the tables.
%% Older dependencies are removed and its TEMPLATE_MODIFIED entry is removed.
%% After this a manual check on all files is done, to ensure that the files were
%% not changed during the template compilation.
%%
%% If a file is changed then the timestamp is added to the MTIME table.
%% Then the FILE_TEMPLATE is checked, and all the corresponding templates are
%% set to the new timestamp.

-module(z_filewatcher_mtime).
-author("Marc Worrell <marc@worrell.nl>").

-include_lib("zotonic.hrl").
-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-record(state, {
    is_scanner_enabled = true :: boolean()
}).

%% interface functions
-export([
    mtime/1,
    modified/1,
    flush_site/1,
    is_template_modified/2,
    insert_template/2,
    file_mtime/1
]).

-define(MTIME, z_filewatcher_mtime_tab).
-define(FILE_TEMPLATE, z_filewatcher_file_template_tab).
-define(TEMPLATE_FILES, z_filewatcher_template_files_tab).
-define(TEMPLATE_MODIFIED, z_filewatcher_template_modified_tab).

%% If no file scanner, then cache all mtimes for max 2 seconds
-define(FLUSH_INTERVAL, 2000).

%% @doc Return the modification time of a file
-spec mtime(filename:filename()) -> {ok, calendar:datetime()} | {error, notfound}.
mtime(File) when is_list(File) ->
    mtime(unicode:characters_to_binary(File));
mtime(File) when is_binary(File) ->
    case ets:lookup(?MTIME, File) of
        [] ->
            modified(File);
        [{_, 0}] ->
            {error, notfound};
        [{_, MTime}] ->
            {ok, MTime}
    end.

%% @doc Mark a file as modified
modified(File) when is_list(File) ->
    modified(unicode:characters_to_binary(File));
modified(File) when is_binary(File) ->
    gen_server:call(?MODULE, {modified, File}).

%% @doc Invalidate all templates of a whole site
flush_site(Site) ->
    gen_server:cast(?MODULE, {flush_site, Site}).

%% @doc Check if a template is marked as modified
is_template_modified(Module, Site) ->
    case ets:lookup(?TEMPLATE_MODIFIED, Module) of
        [{_, 0}] ->
            true;
        [] ->
            true;
        [{_, CompileDT}] ->
            case ets:lookup(?TEMPLATE_MODIFIED, Site) of
                [] -> false;
                [{_,FlushDT}] -> FlushDT > CompileDT
            end
    end.

%% @doc Return the (universal) modification time of file, 0 on enoent
-spec file_mtime(filename:filename()) -> calendar:datetime() | 0.
file_mtime(File) ->
    case file:read_file_info(File, [{time, universal}]) of
        {ok, #file_info{mtime=MTime}} -> MTime;
        {error, enoent} -> 0;
        {error, _} -> 0
    end.

%% @doc Insert a template and its dependencies
insert_template(Module, CompileTime) ->
    gen_server:call(?MODULE, {insert_template, Module, CompileTime}).

%%====================================================================
%% API
%%====================================================================
%% @doc Starts the server
-spec start_link(boolean()) -> {ok, pid()} | ignore | {error, term()}.
start_link(IsScannerEnabled) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [z_convert:to_bool(IsScannerEnabled)], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init([IsScannerEnabled]) ->
    ets:new(?MTIME, [named_table, set, {keypos, 1}, protected, {read_concurrency, true}]),
    ets:new(?FILE_TEMPLATE, [named_table, bag, {keypos, 1}, protected, {read_concurrency, true}]),
    ets:new(?TEMPLATE_FILES, [named_table, set, {keypos, 1}, protected, {read_concurrency, true}]),
    ets:new(?TEMPLATE_MODIFIED, [named_table, set, {keypos, 1}, protected, {read_concurrency, true}]),
    timer:send_after(?FLUSH_INTERVAL, flush),
    {ok, #state{is_scanner_enabled = IsScannerEnabled}}.

handle_call({modified, File}, _From, State) ->
    MTime = file_mtime(File),
    case ets:lookup(?MTIME, File) of
        [{_, MTime}] ->
            ok;
        _ ->
            ets:insert(?MTIME, {File, MTime}),
            do_dependencies(File, MTime)
    end,
    Reply = case MTime of
                0 -> {error, notfound};
                _ -> {ok, MTime}
            end,
    {reply, Reply, State};

handle_call({insert_template, Module, CompileTime}, _From, State) ->
    try
        Deps = [ File || {File, _MTime} <- Module:dependencies() ],
        OldDeps = ets:lookup(?TEMPLATE_FILES, Module),
        DelDeps = OldDeps -- Deps,
        NewDeps = Deps -- OldDeps,
        lists:foreach(
                fun({_, File}) ->
                    ets:delete_object(?FILE_TEMPLATE, {File, Module})
                end,
                DelDeps),
        ets:insert(?FILE_TEMPLATE, [ {File, Module} || File <- NewDeps ]),
        case ets:lookup(?TEMPLATE_MODIFIED, Module) of
            [{_, 0}] ->
                ets:insert(?TEMPLATE_MODIFIED, {Module, CompileTime});
            [{_, PrevDT}] when PrevDT < CompileTime ->
                ets:insert(?TEMPLATE_MODIFIED, {Module, CompileTime});
            [] ->
                ets:insert(?TEMPLATE_MODIFIED, {Module, CompileTime});
            [_] ->
                ok
        end
    catch
        error:undef ->
            ets:insert(?TEMPLATE_MODIFIED, {Module, 0})
    end,
    {reply, ok, State};

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast({flush_site, Site}, State) ->
    ets:insert(?TEMPLATE_MODIFIED, {Site, calendar:universal_time()}),
    {noreply, State};

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(flush, #state{is_scanner_enabled = false} = State) ->
    ets:delete_all_objects(?MTIME),
    timer:send_after(?FLUSH_INTERVAL, flush),
    {noreply, State};

handle_info(flush, #state{is_scanner_enabled = true} = State) ->
    {noreply, State};

handle_info(_Info, State) ->
    ?DEBUG(_Info),
    {noreply, State}.

terminate(_Reason, #state{}) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

do_dependencies(File, 0) ->
    lists:foreach(
            fun({_, Template}) ->
                ets:insert(?TEMPLATE_MODIFIED, {Template, 0})
            end,
            ets:lookup(?FILE_TEMPLATE, File));
do_dependencies(File, MTime) ->
    lists:foreach(
            fun({_, Template}) ->
                case ets:lookup(?TEMPLATE_MODIFIED, Template) of
                    [{_, 0}] ->
                        ok;
                    [{_, MTimeTpl}] when MTimeTpl < MTime ->
                        ets:insert(?TEMPLATE_MODIFIED, {Template, 0});
                    _ ->
                        ok
                end
            end,
            ets:lookup(?FILE_TEMPLATE, File)).
