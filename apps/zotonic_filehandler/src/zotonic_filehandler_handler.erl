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
    reload_module/1
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

-record(state, { }).


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
            case zotonic_filehandler_mapper:map_change(Verb, F) of
                {ok, Actions} ->
                    (Acc -- Actions) ++ Actions;
                {error, _} ->
                    Acc;
                false ->
                    Acc
            end
        end,
        [],
        Es),
    Result = perform(Actions),
    maps:fold(
        fun(F, Verb, _) ->
            FilenameB = z_convert:to_binary(F),
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

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% ------------------------------------- Internal functions -------------------------------------

perform(Actions) ->
    lists:map(
        fun( {M, F, A} ) ->
            erlang:apply(M, F, A)
        end,
        Actions).
