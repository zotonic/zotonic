%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2018 Marc Worrell
%%
%% @doc Server process to handle file changes. Serializes build events.

%% Copyright 2017-2018 Marc Worrell
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


-module(zotonic_filehandler_handler).

-behaviour(gen_server).

-export([
    handle_changes/1,
    compile_all/0,
    reload_modules/0,
    reload_module/1,
    ignore_dir/2,
    ignore_action/2
]).

-export([
    filewatcher_changes_observer/2
]).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-include_lib("zotonic_core/include/zotonic_notifications.hrl").
-include_lib("zotonic_notifier/include/zotonic_notifier.hrl").
-include_lib("zotonic_filewatcher/include/zotonic_filewatcher.hrl").

-record(state, {
        ignore_dir = #{} :: #{ [ binary() ] := boolean() },
        ignore_action = #{} :: #{ term() := boolean() }
    }).


-spec handle_changes( map() ) -> ok.
handle_changes(Es) ->
    gen_server:call(?MODULE, {filewatcher_changes, Es}, infinity).

%% @doc Observer for the filewatcher changes notification.
-spec filewatcher_changes_observer(#filewatcher_changes{}, term()) -> ok.
filewatcher_changes_observer(#filewatcher_changes{ changes = Es }, _Ctx) ->
    handle_changes(Es).

-spec compile_all() -> ok | {error, term()}.
compile_all() ->
    gen_server:call(?MODULE, compile_all, infinity).

-spec reload_modules() -> ok | {error, term()}.
reload_modules() ->
    gen_server:call(?MODULE, reload_modules, infinity).

-spec reload_module(module()) -> ok | {error, term()}.
reload_module(Module) ->
    gen_server:call(?MODULE, {reload_module, Module}, infinity).

%% @doc Ignore directories that are busy, for example a directory
%%      in lib-src where currently a Makefile is running.
-spec ignore_dir( file:filename_all(), boolean() ) -> ok.
ignore_dir(Dir, IsIgnore) ->
    gen_server:cast(?MODULE, {ignore_dir, Dir, IsIgnore}).

%% @doc Ignore action commands, useful to prevent duplication of actions
%%      during longer runs or as side effects of running actions.
-spec ignore_action( term(), boolean() ) -> ok.
ignore_action(Action, IsIgnore) ->
    gen_server:cast(?MODULE, {ignore_action, Action, IsIgnore}).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------- gen_server callbacks ------------------------------

init([]) ->
    zotonic_notifier:observe(
            ?SYSTEM_NOTIFIER, filewatcher_changes,
            {?MODULE, filewatcher_changes_observer},
            self(), 500),
    {ok, #state{ }}.

handle_call({filewatcher_changes, Es}, _From, State) ->
    Actions = maps:fold(
        fun(F, Verb, Acc) ->
            case is_ignored_dir(F, State#state.ignore_dir) of
                true ->
                    Acc;
                false ->
                    case zotonic_filehandler_mapper:map_change(Verb, F) of
                        {ok, Actions} ->
                            (Acc -- Actions) ++ Actions;
                        {error, _} ->
                            Acc;
                        false ->
                            Acc
                    end
            end
        end,
        [],
        Es),
    Actions1 = lists:filter(
        fun(Act) ->
            not maps:is_key(Act, State#state.ignore_action)
        end,
        Actions),
    Result = perform(Actions1),
    maps:fold(
        fun(F, Verb, _) ->
            FilenameB = z_convert:to_binary(F),
            case is_ignored_dir(FilenameB, State#state.ignore_dir) of
                true ->
                    ok;
                false ->
                    Basename = filename:extension(FilenameB),
                    Extension = filename:extension(FilenameB),
                    z_sites_manager:foreach(
                        fun(Context) ->
                            z_notifier:first(#filewatcher{
                                verb = Verb,
                                file = FilenameB,
                                basename = Basename,
                                extension = Extension
                                }, Context)
                        end)
            end
        end,
        ok,
        Es),
    {reply, Result, State};

handle_call(compile_all, _From, State) ->
    Result = zotonic_filehandler_compile:all(),
    {reply, Result, State};

handle_call(reload_modules, _From, State) ->
    Result = zotonic_filehandler_compile:ld(),
    {reply, Result, State};

handle_call({reload_module, Module}, _From, State) ->
    Result = zotonic_filehandler_compile:ld(Module),
    {reply, Result, State}.

handle_cast({ignore_dir, Dir, true}, #state{ ignore_dir = IgnoreDir } = State) ->
    NormDir = normalize_dir(Dir),
    IgnoredDirs1 = IgnoreDir#{ NormDir => true },
    {noreply, State#state{ ignore_dir = IgnoredDirs1 }};

handle_cast({ignore_dir, Dir, false}, #state{ ignore_dir = IgnoreDir } = State) ->
    NormDir = normalize_dir(Dir),
    IgnoredDirs1 = maps:remove(NormDir, IgnoreDir),
    {noreply, State#state{ ignore_dir = IgnoredDirs1 }};

handle_cast({ignore_action, Action, true}, #state{ ignore_action = IgnoreAction } = State) ->
    IgnoredAction1 = IgnoreAction#{ Action => true },
    {noreply, State#state{ ignore_action = IgnoredAction1 }};

handle_cast({ignore_action, Action, false}, #state{ ignore_action = IgnoreAction } = State) ->
    IgnoredAction1 = maps:remove(Action, IgnoreAction),
    {noreply, State#state{ ignore_action = IgnoredAction1 }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% ------------------------------------- Internal functions -------------------------------------

normalize_dir(Dir) ->
    lists:reverse( filename:split( z_convert:to_binary(Dir) ) ).

is_ignored_dir(Filename, IgnoredDirs) ->
    Fileparts = normalize_dir( filename:dirname(Filename) ),
    is_ignored_dir_1(Fileparts, IgnoredDirs).

is_ignored_dir_1([], _) ->
    false;
is_ignored_dir_1(Parts, IgnoredDirs) ->
    case maps:is_key(Parts, IgnoredDirs) of
        true -> true;
        false -> is_ignored_dir_1(tl(Parts), IgnoredDirs)
    end.



perform(Actions) ->
    lists:map( fun perform_action/1, Actions ).

perform_action({M, F, A}) when is_atom(M), is_atom(F), is_list(A) ->
    erlang:apply(M, F, A).
