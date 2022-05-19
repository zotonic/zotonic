%% @author William Fank Thomé <williamthome@hotmail.com>
%% @copyright 2022 Marc Worrell
%% @doc Chrome CLI command

%% Copyright 2022 Marc Worrell
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

-module(zotonic_cmd_chrome).
-author("William Fank Thomé <williamthome@hotmail.com>").

%% API
-export([
    info/0,
    usage/0,
    run/1
]).

info() ->
    "Opens Chrome in the site URL with secure certificate flags".

usage() ->
    io:format("USAGE: zotonic chrome [options] <site_name> ~n"),
    io:format("Options: ~n"),
    io:format("  See https://peter.sh/experiments/chromium-command-line-switches/ ~n"),
    io:format("Note: ~n"),
    io:format("  Requires 'zotonic start' ~n~n").

run(Args) ->
    case zotonic_command:net_start() of
        ok ->
            run_parse_args(Args);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

run_parse_args(Args) ->
    case parse_args(Args, #{args => []}) of
        {ok, #{site := Site, args := ParsedArgs}} ->
            SiteName = list_to_atom(Site),
            Res = zotonic_command:rpc(
                mod_development, exec_browser, [ chrome, SiteName, ParsedArgs ]
            ),
            io:format("~p~n", [ Res ]);
        {error, _} = ZError ->
            zotonic_command:format_error(ZError)
    end.

parse_args(["--" ++ _], _Acc) ->
    {error, "The last entry should be the site name"};
parse_args([Site], Acc) ->
    {ok, Acc#{site => Site}};
parse_args(["--" ++ _ = Arg | Rest], #{args := Args} = AccIn) ->
    AccOut = AccIn#{args => [Arg | Args]},
    parse_args(Rest, AccOut);
parse_args([], _Acc) ->
    usage(),
    halt(1);
parse_args([Arg | _Args], _Acc) ->
    {error, "All Chrome args starts with '--'. '" ++ Arg ++ "' is invalid"}.
