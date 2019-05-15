%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%%
%% @doc Handle file changes.

%% Copyright 2017 Marc Worrell
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


-module(zotonic_filehandler).

-export([
    compile_all/0,
    reload_modules/0,
    reload_module/1,
    load_module/1,
    compile_options/1,
    terminal_notifier/1
]).

-type verb() :: create|modify|delete.
-export_type([verb/0]).


-spec compile_all() -> ok | {error, term()}.
compile_all() ->
    zotonic_filehandler_handler:compile_all().

-spec reload_modules() -> ok | {error, term()}.
reload_modules() ->
    zotonic_filehandler_handler:reload_modules().

-spec reload_module(module()) -> ok | {error, term()}.
reload_module(Module) ->
    zotonic_filehandler_handler:reload_module(Module).

-spec load_module(module()) -> ok | {error, term()}.
load_module(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            ok;
        {error, _} = Error ->
            Error
    end.

-spec compile_options(file:filename_all()) -> {ok, list()} | {error, term()}.
compile_options(ErlangFile) ->
    zotonic_filehandler_compile:compile_options(ErlangFile).


%% @doc Send a message to the user as a system notification
terminal_notifier(Msg) ->
    zotonic_filehandler_terminal:notify(Msg).


