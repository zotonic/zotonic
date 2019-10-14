#!/usr/bin/env escript
%% -*- erlang -*-
%% @author Blaise
%% @copyright 2017
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-export([main/1]).

-define(ZOTONIC, get_zotonic_path()).
-define(COMMANDS, ?ZOTONIC ++ "/apps/zotonic_launcher/src/command").

get_zotonic_path() ->
    {ok, CurrentDir} = file:get_cwd(),
    _Dir = CurrentDir.

load_mod_paths() ->
    EbinPath = filename:join([ ?ZOTONIC, "_build", "default", "lib" ]),
    case file:list_dir(EbinPath) of
        {ok, FileNames} ->
            lists:foreach(
                fun(Name) ->
                    code:add_pathz( filename:join([ EbinPath, Name, "ebin" ]) )
                end,
                FileNames);
        {error, enoent} ->
            halt()
    end.

usage() ->
    {ok, ListItems0} = file:list_dir(?COMMANDS),
    ListItems = [ unicode:characters_to_binary(Item) || Item <- ListItems0 ],
    FileNames = [ ListItem || ListItem <- ListItems, size(ListItem) > 12 ],
    CommandNames = lists:filtermap(
        fun(ListItem) ->
            case filename:basename(ListItem, <<".erl">>) of
                <<"zotonic_cmd_", Command/binary>> -> {true, Command};
                _ -> false
            end
        end,
        FileNames),

    io:format("USAGE: ~s (options) [command] ~n~n", [ filename:rootname(escript:script_name()) ]),
    io:format("Where [command] is one of: ~n"),
    lists:map(fun(Cmd) -> io:format("   ~s~n", [Cmd]) end, lists:sort(CommandNames)),
    io:format("~n"),
    io:format("See http://zotonic.com/docs/latest/manuals/cli.html for more info. ~n~n"),
    io:format("Options: ~n"),
    io:format("  -v : Prints Zotonic version ~n~n").

main([]) ->
    usage();

main([ Command | T ]) ->
    erlang:put(is_zotonic_command, true),

    load_mod_paths(),

    case Command of
        "-v" ->
            zotonic_release:run();
        _ ->
            CommandMod = list_to_atom( "zotonic_cmd_" ++ Command ),
            case code:ensure_loaded(CommandMod) of
                {module, CommandMod} ->
                    apply(CommandMod, run, [T]);
                 {error, _} ->
                    io:format(standard_error, "Command not found: ~s~n", [Command]),
                    erlang:halt(1)
            end
    end.
