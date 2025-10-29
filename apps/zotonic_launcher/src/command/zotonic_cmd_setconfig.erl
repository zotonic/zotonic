%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Set a runtime Zotonic config value.
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

-module(zotonic_cmd_setconfig).
-author("Marc Worrell").

%% API
-export([info/0, run/1]).

info() ->
    "Set runtime global Zotonic application configuration, not stored in the config files.".

run([ "zotonic", Name, Value ]) ->
    set(Name, Value);
run(_) ->
    io:format("USAGE: setconfig zotonic <config_name> <value>~n"),
    halt(1).

set(Name, "true") ->
    set(Name, true);
set(Name, "false") ->
    set(Name, false);
set(Name, "undefined") ->
    set(Name, undefined);
set(Name, Value) ->
    case zotonic_command:net_start() of
        ok ->
            NameAtom = list_to_atom(Name),
            Res = zotonic_command:rpc(z, setconfig, [zotonic, NameAtom, Value]),
            io:format("~p~n", [ Res ]);
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.
