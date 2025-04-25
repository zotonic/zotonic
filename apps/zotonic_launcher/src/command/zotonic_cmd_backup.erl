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

run([ Site, Cmd ]) when Cmd =:= "list"; Cmd =:= "start" ->
    case zotonic_command:net_start() of
        ok ->
            SiteName = list_to_atom(Site),
            case is_mod_backup_running(SiteName) of
                true when Cmd =:= "list" ->
                    case backup_list(SiteName) of
                        undefined ->
                            io:format("(No backups)");
                        Backups ->
                            io:format("Backups for ~p:~n", [ SiteName ]),
                            lists:foreach(
                                fun(B) ->
                                    io:format("~n- ~s:", [ maps:get(name, B) ]),
                                    B1 = maps:remove(name, B),
                                    zotonic_cmd_config:pretty_print_value(<<>>, B1)
                                end,
                                Backups)
                    end;
                true when Cmd =:= "start" ->
                    case backup_start(SiteName) of
                        ok ->
                            io:format("Backup started for '~p'~n", [ SiteName ]);
                        undefined ->
                            io:format("Could not start backup for '~p': no handler~n", [ SiteName ]),
                            halt();
                        {error, in_progress} ->
                            io:format("There is already a backup running for '~p'~n", [ SiteName ]),
                            halt();
                        {error, Reason} ->
                            io:format("Could not start backup for '~p': ~p~n", [ SiteName, Reason ]),
                            halt()
                    end;
                false ->
                    io:format("Backup modules is not enabled for '~p'~n", [ SiteName ]),
                    halt()
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end;
run([ Site, "restore", Backup ]) ->
    case zotonic_command:net_start() of
        ok ->
            SiteName = list_to_atom(Site),
            case is_mod_backup_running(SiteName) of
                true ->
                    io:format("Restoring backup ~s for '~p'.~n", [ Backup, SiteName ]),
                    io:format("The site will be unavailable whilst the backup is restored.~n~n"),
                    case backup_restore(SiteName, Backup) of
                        ok ->
                            io:format("Done.~n");
                        undefined ->
                            io:format("Could not restore backup ~s for '~p': no handler~n", [ Backup, SiteName ]),
                            halt();
                        {error, Reason} ->
                            io:format("Could not restore backup ~s for '~p': ~p~n", [ Backup, SiteName, Reason ]),
                            halt()
                    end;
                false ->
                    io:format("Backup modules is not enabled for '~p'~n", [ SiteName ]),
                    halt()
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end;

run(_) ->
    io:format("USAGE: backup <site_name> list|start|restore [backup-name]~n"),
    halt().

is_mod_backup_running(SiteName) ->
    zotonic_command:rpc(z, is_module_enabled, [ mod_backup, SiteName ]).

backup_list(SiteName) ->
    zotonic_command:rpc(z, n1, [ backup_list, SiteName ]).

backup_start(SiteName) ->
    zotonic_command:rpc(z, n1, [ backup_start, SiteName ]).

backup_restore(SiteName, Backup) ->
    BackupBin = iolist_to_binary(Backup),
    zotonic_command:rpc(z, n1, [ {backup_restore, BackupBin}, SiteName ]).
