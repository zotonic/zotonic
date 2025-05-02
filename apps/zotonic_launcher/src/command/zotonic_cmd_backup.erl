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
                    io:format("Backup module is not enabled for '~p'~n", [ SiteName ]),
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
                    io:format("Please type the sitename (~p) to continue, anything else to cancel.~n~n", [ SiteName ]),
                    case io:get_line("Type the sitename to continue: ") of
                        Input when is_list(Input) ->
                            case string:trim(Input) of
                                SiteNameStr when SiteNameStr =:= Site ->
                                    case restore_request_config_options() of
                                        {ok, Config} ->
                                            do_backup_restore(SiteName, Backup, Config);
                                        {error, _} ->
                                            io:format("Aborting restore.~n"),
                                            halt()
                                    end;
                                _ ->
                                    io:format("Aborting restore.~n"),
                                    halt()
                            end;
                        _ ->
                            io:format("Could not read input, aborting restore.~n"),
                            halt()
                    end;
                false ->
                    io:format("Backup module is not enabled for '~p'~n", [ SiteName ]),
                    halt()
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end;
run([ Site, "download" ]) ->
    case zotonic_command:net_start() of
        ok ->
            SiteName = list_to_atom(Site),
            case is_mod_backup_running(SiteName) of
                true ->
                    io:format("Downloading and restoring newest backup ~s for '~p'.~n", [ Backup, SiteName ]),
                    io:format("The site will be unavailable whilst the backup is restored.~n~n"),
                    io:format("Please type the sitename (~p) to continue, anything else to cancel.~n~n", [ SiteName ]),
                    case io:get_line("Type the sitename to continue: ") of
                        Input when is_list(Input) ->
                            case string:trim(Input) of
                                SiteNameStr when SiteNameStr =:= Site ->
                                    do_backup_download(SiteName);
                                _ ->
                                    io:format("Aborting download and restore.~n"),
                                    halt()
                            end;
                        _ ->
                            io:format("Could not read input, aborting restore.~n"),
                            halt()
                    end;
                false ->
                    io:format("Backup module is not enabled for '~p'~n", [ SiteName ]),
                    halt()
            end;
        {error, _} = Error ->
            zotonic_command:format_error(Error)
    end;
run(_) ->
    io:format("USAGE: backup <site_name> list|start|restore|download [backup-name-for-restore]~n"),
    halt().


restore_request_config_options() ->
    io:format("Optionally the configuration and security files can also be restored.~n"),
    io:format("The configuration are the priv/zotonic_site.* and priv/config/* files.~n"),
    io:format("The security files are the certificates and other secrets stored in the Zotonic~n"),
    io:format("security directory for the site.~n~n"),
    io:format("Per default no configuration or security files are restored.~n~n"),
    get_config_options().

get_config_options() ->
    Prompt = "Do you want to restore the configuration files? All, Config, Security or None [a/c/s/N] ",
    case io:get_line(Prompt) of
        Input when is_list(Input) ->
            case string:trim(Input) of
                "" -> {ok, [ database, files ]};
                "N" -> {ok, [ database, files ]};
                "a" -> {ok, [ database, files, security, config ]};
                "c" -> {ok, [ database, files, config ]};
                "s" -> {ok, [ database, files, security ]};
                _ -> get_config_options()
            end;
        _ ->
            {error, eof}
    end.

%% @doc Restore a backup.
do_backup_restore(SiteName, Backup, Config) ->
    case backup_restore(SiteName, Backup, Config) of
        ok ->
            io:format("Done.~n");
        undefined ->
            io:format("Could not restore backup ~s for '~p': no handler~n", [ Backup, SiteName ]),
            halt();
        {error, Reason} ->
            io:format("Could not restore backup ~s for '~p': ~p~n", [ Backup, SiteName, Reason ]),
            halt()
    end.

backup_restore(SiteName, Backup, Config) ->
    BackupBin = iolist_to_binary(Backup),
    zotonic_command:rpc(z, n1, [ {backup_restore, BackupBin, Config}, SiteName ]).

%% @doc Download and restore a backup.
do_backup_download(SiteName) ->
    case backup_download(SiteName) of
        ok ->
            io:format("Download and restore started, site will restart when ready.~n");
        undefined ->
            io:format("Could not download and restore newest backup for '~p': no handler~n", [ SiteName ]),
            halt();
        {error, in_progress} ->
            io:format("Download and restore are already in progress.~n"),
            halt();
        {error, no_env_backup} ->
            io:format("Error: the site MUST be set to 'backup' environment.~n"),
            halt();
        {error, Reason} ->
            io:format("Could not restore newest backup for '~p': ~p~n", [ SiteName, Reason ]),
            halt()
    end.


%% @doc Check if the site has mod_backup enabled.
is_mod_backup_running(SiteName) ->
    zotonic_command:rpc(z, is_module_enabled, [ mod_backup, SiteName ]).

%% @doc Return the list of available backups.
backup_list(SiteName) ->
    zotonic_command:rpc(z, n1, [ backup_list, SiteName ]).

%% @doc Start a new backup (if no backup is running).
backup_start(SiteName) ->
    zotonic_command:rpc(z, n1, [ backup_start, SiteName ]).

%% @doc Start a new backup (if no backup is running).
backup_download(SiteName) ->
    zotonic_command:rpc(z, n1, [ backup_download, SiteName ]).
