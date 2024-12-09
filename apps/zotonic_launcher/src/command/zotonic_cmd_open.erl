%% @author William Fank Thomé <williamthome@hotmail.com>
%% @copyright 2022-2023 William Fank Thomé
%% @doc Open site CLI command
%% @end

%% Copyright 2022-2023 William Fank Thomé
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

-module(zotonic_cmd_open).
-author("William Fank Thomé <williamthome@hotmail.com>").

%% API
-export([
    info/0,
    usage/0,
    run/1
]).

info() ->
    "Opens the default browser (on macOS) or Chrome with the site URL".

usage() ->
    io:format("USAGE: zotonic open <site_name> ~n"),
    io:format("~n"),
    io:format("Notes: ~n"),
    io:format("  Requires Zotonic running (use the command 'zotonic start') ~n"),
    io:format("~n").

run(Args) ->
    case zotonic_command:net_start() of
        ok ->
            run_parse_args(Args);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

run_parse_args(Args) ->
    Defaults = #{args => [], options => #{secure => false}},
    case parse_args(Args, Defaults) of
        {ok, #{
            site := Site,
            args := _ParsedArgs,
            options := _Options
        }} ->
            SiteName = list_to_atom(Site),
            Res = zotonic_command:rpc(
                z_exec_browser, open, [ SiteName ]
            ),
            io:format("~p~n", [ Res ]);
        {error, _} = ZError ->
            zotonic_command:format_error(ZError)
    end.

parse_args(["--" ++ _], _Acc) ->
    {error, "The last entry must be the site name"};
parse_args([Site], Acc) ->
    {ok, Acc#{site => Site}};
parse_args([], _Acc) ->
    usage(),
    halt(1);
parse_args([Arg | _Args], _Acc) ->
    {error, "Argument '" ++ Arg ++ "' is invalid"}.
