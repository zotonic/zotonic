%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc List backups, make a backup or restore a backup. The module
%% mod_backup must be enabled for the site.
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

-module(zotonic_cmd_backup).
-author("Marc Worrell").

%% API
-export([info/0, run/1]).

info() ->
    "Show the configuration of a site.".

run([ Site, "list" ]) ->
    case zotonic_command:net_start() of
        ok ->
            SiteName = list_to_atom(Site),
            case is_backup_running(SiteName) of
                true ->
                    case backup_list(SiteName) of
                        undefined ->
                            io:format("(No backups)");
                        Backups ->
                            io:format("Backups for ~p:~n", [ SiteName ]),
                            maps:foreach(
                                fun(_, B) ->
                                    io:format("~p~n", [ B ])
                                end,
                                Backups)
                    end;
                false ->
                    io:format("Backup modules is not enabled for '~p'~n", [ SiteName ]),
                    halt()
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end;

run(_) ->
    io:format("USAGE: backup <site_name> list|make|restore~n"),
    halt().

is_backup_running(SiteName) ->
    zotonic_command:rpc(z, is_module_enabled, [ mod_backup, SiteName ]).

backup_list(SiteName) ->
    zotonic_command:rpc(z, n1, [ backup_list, SiteName ]).
