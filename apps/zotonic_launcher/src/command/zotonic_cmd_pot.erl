%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Generate new Zotonic or site gettext pot file
%% @end

%% Copyright 2025 Marc Worrell
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

-module(zotonic_cmd_pot).
-author("Marc Worrell").

%% API
-export([info/0, run/1]).

info() ->
    "Generate gettext pot files for Zotonic or site translations.".

run([ "zotonic" ]) ->
    pot(zotonic);
run([ Site ]) ->
    pot(Site);
run(_) ->
    io:format("USAGE: pot [ zotonic | sitename ]~n"),
    halt(1).

pot(What) ->
    case zotonic_command:net_start() of
        ok ->
            Res = pot_rpc(What),
            case Res of
                {error, bad_name} ->
                    io:format("Unknown site '~s'~n", [ What ]);
                {error, needs_core_zotonic} ->
                    io:format("Zotonic core is not installed~n");
                {error, gettext_notfound} ->
                    io:format("Cannot find 'gettext' command line tools~n");
                {error, Reason} ->
                    io:format("Site should be running: ~p~n", [ Reason ]);
                Other ->
                    io:format("~p~n", [ Other ])
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

pot_rpc(zotonic) ->
    io:format("Generating Zotonic core modules pot file ..."),
    zotonic_command:rpc(mod_translation, generate_core, []);
pot_rpc(Site) ->
    SiteName = list_to_atom(Site),
    io:format("Generating pot file for site ~p .. ", [ SiteName ]),
    zotonic_command:rpc(mod_translation, generate, [ SiteName ]).
