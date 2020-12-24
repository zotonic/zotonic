%%%-------------------------------------------------------------------
%%% @author Blaise
%%%Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%	 http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%% @doc
%%%
%%% @end
%%% Created : 13. Dec 2017 7:36 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_configfiles).
-author("Blaise").

%% API
-export([info/0, run/1]).

info() ->
    "List Zotonic config files being used.".

run(_) ->
    case zotonic_command:net_start() of
        ok ->
            case zotonic_command:get_target_node() of
                {ok, Target} ->
                    Files = case net_adm:ping(Target) of
                        pong ->
                            zotonic_command:rpc(zotonic_launcher_config, config_files, [ Target ]);
                        pang ->
                            zotonic_launcher_config:config_files(Target)
                    end,
                    list_files(Files, Target);
                {error, _} = Error ->
                    zotonic_command:format_error(Error)
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end.

list_files(Fs, Target) when is_list(Fs) ->
    io:format("Zotonic config files for ~p:~n", [Target]),
    lists:foreach(
        fun(F) ->
            io:format("- ~s~n", [F])
        end,
        Fs);
list_files(Other, Target) ->
    io:format(standard_error, "Error finding config files for ~p: ~p~n", [ Target, Other ]),
    halt(1).