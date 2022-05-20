%% @author William Fank Thomé <williamthome@hotmail.com>
%% @copyright 2022 William Fank Thomé
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
    info/1,
    usage/0,
    usage/1,
    run/1,
    run/2
]).

info() ->
    info("Chrome").

info(Browser) ->
    "Opens " ++ Browser ++ " in the site URL with secure certificate flags".

usage() ->
    usage("chrome").

usage(Browser) ->
    io:format("USAGE: zotonic " ++ Browser ++ " [options] [switches] <site_name> ~n"),
    io:format("~n"),
    io:format("Options: ~n"),
    io:format("  -s <secure>            Opens site as secure and ignore certificate errors ~n"),
    io:format("~n"),
    io:format("Switches: ~n"),
    io:format("  --incognito            Launches " ++ Browser ++ " directly in Incognito private browsing mode ~n"),
    io:format("  --purge-memory-button  Add purge memory button to " ++ Browser ++ " ~n"),
    io:format("  --multi-profiles       Enable multiple profiles in " ++ Browser ++ " ~n"),
    io:format("~n"),
    io:format("  * See a list of switches here https://peter.sh/experiments/chromium-command-line-switches/ ~n"),
    io:format("~n"),
    io:format("Notes: ~n"),
    io:format("  Requires Zotonic running (use the command 'zotonic start') ~n"),
    io:format("~n").

run(Args) ->
    run(chrome, Args).

run(Browser, Args) ->
    case zotonic_command:net_start() of
        ok ->
            run_parse_args(Browser, Args);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

run_parse_args(Browser, Args) ->
    Defaults = #{args => [], options => #{secure => false}},
    case parse_args(atom_to_list(Browser), Args, Defaults) of
        {ok, #{
            site := Site,
            args := ParsedArgs,
            options := Options
        }} ->
            SiteName = list_to_atom(Site),
            Res = zotonic_command:rpc(
                mod_development, exec_browser, [ Browser, SiteName, ParsedArgs, Options ]
            ),
            io:format("~p~n", [ Res ]);
        {error, _} = ZError ->
            zotonic_command:format_error(ZError)
    end.

parse_args(_Browser, ["--" ++ _], _Acc) ->
    {error, "The last entry must be the site name"};
parse_args(_Browser, [Site], Acc) ->
    {ok, Acc#{site => Site}};
parse_args(Browser, ["--" ++ _ = Arg | Rest], #{args := Args} = AccIn) ->
    AccOut = AccIn#{args => [Arg | Args]},
    parse_args(Browser, Rest, AccOut);
parse_args(Browser, ["-s" | Rest], #{options := Options} = AccIn) ->
    AccOut = AccIn#{options => Options#{secure => true}},
    parse_args(Browser, Rest, AccOut);
parse_args(Browser, [], _Acc) ->
    usage(Browser),
    halt(1);
parse_args(Browser, [Arg | _Args], _Acc) ->
    {error, "All " ++ Browser ++ " args must start with '--'. '" ++ Arg ++ "' is invalid"}.
