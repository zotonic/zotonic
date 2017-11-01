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
    compile_options/1,
    handle_changes/1,
    terminal_notifier/1
]).

-type verb() :: create|modify|delete.
-export_type([verb/0]).


-spec compile_all() -> ok | {error, term()}.
compile_all() ->
    zotonic_filehandler_compile:all().

-spec reload_modules() -> ok | {error, term()}.
reload_modules() ->
    zotonic_filehandler_compile:ld().

-spec reload_module(module()) -> ok | {error, term()}.
reload_module(Module) ->
    zotonic_filehandler_compile:ld(Module).

-spec compile_options(file:filename_all()) -> {ok, list()} | {error, term()}.
compile_options(ErlangFile) ->
    zotonic_filehandler_compile:compile_options(ErlangFile).

-spec handle_changes( map() ) -> ok.
handle_changes(Es) ->
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
    perform(Actions).

perform(Actions) ->
    lists:map(
        fun( {M, F, A} ) ->
            erlang:apply(M, F, A)
        end,
        Actions).

%% @doc Send a message to the user as a system notification
terminal_notifier(undefined) ->
    undefined;
terminal_notifier("") ->
    undefined;
terminal_notifier(Message) ->
    terminal_notifier(os:type(), z_string:trim(Message)).

%% @doc send message to the user
terminal_notifier(_OS, "") ->
    undefined;
terminal_notifier({unix, darwin}, Msg) ->
    os:cmd("which terminal-notifier && terminal-notifier -title Zotonic  -message " ++ z_utils:os_escape(Msg));
terminal_notifier({unix, _Arch}, Msg) ->
    os:cmd("which notify-send && notify-send \"Zotonic\" " ++ z_utils:os_escape(Msg));
terminal_notifier(_OS, _Msg) ->
    undefined.
