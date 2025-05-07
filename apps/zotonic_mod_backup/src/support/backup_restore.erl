%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Restore a (downloaded) backup. Replaces the database schema,
%% overwrites the archive files and unpacks the config files.
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

-module(backup_restore).

-export([
    default_options/0,
    restore_newest_backup/1,
    restore_newest_backup/2,
    restore_backup/2,
    restore_backup/3,
    is_file_missing/2
]).

-type restore_option() :: database
                        | files
                        | security
                        | config.

-type restore_options() :: [ restore_option() ].

-export_type([ restore_option/0, restore_options/0 ]).

-define(DEFAULT_RESTORE_OPTIONS, [ database, files ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec default_options() -> [ restore_option() ].
default_options() ->
    ?DEFAULT_RESTORE_OPTIONS.

%% @doc Restore the newest backup from the backup files. This
%% is called after backups are downloaded.
%% After the database backup is restored, the site is restarted.
-spec restore_newest_backup(Context) -> ok | {error, Reason} when
    Context :: z:context(),
    Reason :: nobackup | incomplete | term().
restore_newest_backup(Context) ->
    restore_newest_backup(?DEFAULT_RESTORE_OPTIONS, Context).

%% @doc Restore the newest backup from the backup files. This
%% is called after backups are downloaded.
%% After the database backup is restored, the site is restarted.
-spec restore_newest_backup(Options, Context) -> ok | {error, Reason} when
    Context :: z:context(),
    Options :: [ restore_option() ],
    Reason :: nobackup | incomplete | term().
restore_newest_backup(Options, Context) ->
    BackupData = backup_create:read_admin_file(Context),
    case newest_backup(BackupData) of
        undefined ->
            {error, nobackup};
        Backup ->
            restore_backup(Backup, Options, Context)
    end.

%% @doc Restore a specific backup from the backup list.
%% After the database is restored, the site is restarted.
-spec restore_backup(Backup, Context) -> ok | {error, Reason} when
    Backup :: binary() | map() | undefined,
    Context :: z:context(),
    Reason :: unknown_backup | incomplete | term().
restore_backup(Backup, Context) ->
    restore_backup(Backup, ?DEFAULT_RESTORE_OPTIONS, Context).

%% @doc Restore a specific backup from the backup list.
%% After the database is restored, the site is restarted.
-spec restore_backup(Backup, Options, Context) -> ok | {error, Reason} when
    Backup :: binary() | map() | undefined,
    Options :: [ restore_option() ],
    Context :: z:context(),
    Reason :: unknown_backup | incomplete | term().
restore_backup(undefined, _Options, _Context) ->
    {error, unknown_backup};
restore_backup(Name, Options, Context) when is_binary(Name) ->
    BackupData = backup_create:read_admin_file(Context),
    restore_backup(maps:get(Name, BackupData, undefined), Options, Context);
restore_backup(Backup, Options, Context) when is_map(Backup) ->
    ok = z_db_pool:pause_connections(Context),
    case restore_backup_do(Backup, Options, Context) of
        ok ->
            ?LOG_NOTICE(#{
                in => zotonic_mod_backup,
                text => <<"Restored database, restarting site">>,
                result => ok,
                backup => Backup
            }),
            % Restart site with new database (and config files)
            z_sites_manager:restart(z_context:site(Context));
        {error, Reason} = Error ->
            z_db_pool:unpause_connections(Context),
            ?LOG_ERROR(#{
                in => zotonic_mod_backup,
                text => <<"Restoring database failed">>,
                result => error,
                reason => Reason,
                backup => Backup
            }),
            Error
    end.

restore_backup_do(Backup, Options, Context) ->
    jobs:run(zotonic_singular_job, fun() ->
        steps([
                {files, fun maybe_restore_files_backup/3},
                {database, fun maybe_restore_database_backup/3},
                {security, fun maybe_restore_security_backup/3},
                {config, fun maybe_restore_config_backup/3}
            ], Backup, Options, Context)
    end).

steps([], _Backup, _Options, _Context) ->
    ok;
steps([ {Opt, Func} | Steps ], Backup, Options, Context) ->
    case proplists:get_bool(Opt, Options) of
        true ->
            case Func(Backup, Options, Context) of
                ok -> steps(Steps, Backup, Options, Context);
                {error, _} = Error -> Error
            end;
        false ->
            steps(Steps, Backup, Options, Context)
    end.

%% @doc Unpack the security files into the zotonic security directory.
%% Overwrites existing files, does not delete files that are not in the archive.
maybe_restore_security_backup(#{ <<"config_files">> := ConfigFile }, _Options, Context) when is_binary(ConfigFile) ->
    case is_file_missing(ConfigFile, Context) of
        true ->
            ?LOG_ERROR(#{
                in => zotonic_mod_backup,
                text => <<"Config backup-file missing">>,
                result => error,
                reason => incomplete,
                files => ConfigFile
            }),
            {error, incomplete};
        false ->
            % Files are there - try to unpack
            BackupPath = filename:join(backup_create:dir(Context), ConfigFile),
            case maybe_decrypt_file(BackupPath, Context) of
                {ok, BackupDecrypted} ->
                    restore_security_backup(BackupDecrypted, Context);
                {error, _} = Error ->
                    Error
            end
    end;
maybe_restore_security_backup(_Backup, _Options, _Context) ->
    ok.

restore_security_backup(BackupDecrypted, Context) ->
    % List all files - save:
    %% - config-sitename/security/...
    % to the zotonic security dir.
    case erl_tar:extract(BackupDecrypted, [ compressed, memory ]) of
        {ok, Files} ->
            Site = z_context:site(Context),
            {ok, SecurityDir} = z_sites_config:security_dir(Site),
            Sitename = atom_to_list(Site),
            Prefix = "config-" ++ Sitename ++ "/security/",
            lists:foreach(
                fun
                    ({Filename, Content}) ->
                        case string:prefix(Filename, Prefix) of
                            nomatch ->
                                ok;
                            RelPath ->
                                % Write the file in the relative path of the
                                % security directory
                                AbsPath = filename:join(SecurityDir, RelPath),
                                ok = z_filelib:ensure_dir(AbsPath),
                                case file:write_file(AbsPath, Content) of
                                    ok ->
                                        ok;
                                    {error, Reason} ->
                                        ?LOG_ERROR(#{
                                            text => <<"Restore partially failed: cannot write security file">>,
                                            in => zotonic_mod_backup,
                                            result => error,
                                            reason => Reason,
                                            filename => AbsPath
                                        })
                                end
                        end;
                    (_) ->
                        ok
                end,
                Files);
        {error, Reason} = Error ->
             ?LOG_ERROR(#{
                text => <<"Restore failed: cannot open config tar for security files">>,
                in => zotonic_mod_backup,
                result => error,
                reason => Reason,
                filename => BackupDecrypted
            }),
            Error
    end.



%% @doc Unpack the config files into the site priv and priv/config.d directory.
%% Overwrites existing files, does not delete files that are not in the archive.
maybe_restore_config_backup(#{ <<"config_files">> := ConfigFile }, _Options, Context) when is_binary(ConfigFile) ->
    case is_file_missing(ConfigFile, Context) of
        true ->
            ?LOG_ERROR(#{
                in => zotonic_mod_backup,
                text => <<"Config backup-file missing">>,
                result => error,
                reason => incomplete,
                files => ConfigFile
            }),
            {error, incomplete};
        false ->
            % Files are there - try to unpack
            BackupPath = filename:join(backup_create:dir(Context), ConfigFile),
            case maybe_decrypt_file(BackupPath, Context) of
                {ok, BackupDecrypted} ->
                    restore_config_backup(BackupDecrypted, Context);
                {error, _} = Error ->
                    Error
            end
    end;
maybe_restore_config_backup(_Backup, _Options, _Context) ->
    ok.

restore_config_backup(BackupDecrypted, Context) ->
    % List all files - save:
    % - config-sitename/config/priv/zotonic_site.ext
    % - config-sitename/config/config.d/...
    % to the site priv dir.
    case erl_tar:extract(BackupDecrypted, [ compressed, memory ]) of
        {ok, Files} ->
            Site = z_context:site(Context),
            case z_path:site_dir(Site) of
                {error, Reason} = Error ->
                     ?LOG_ERROR(#{
                        text => <<"Restore failed: no site_dir for site">>,
                        in => zotonic_mod_backup,
                        site => Site,
                        result => error,
                        reason => Reason,
                        filename => BackupDecrypted
                    }),
                    Error;
                SiteDir ->
                    PrivDir = filename:join([ SiteDir, "priv" ]),
                    Sitename = atom_to_list(Site),
                    ConfigFilePrefix = "config-" ++ Sitename ++ "/config/priv/zotonic_site.",
                    ConfigDPrefix = "config-" ++ Sitename ++ "/config/config.d/",
                    lists:foreach(
                        fun({Filename, Content}) ->
                            % Restore config.d
                            case string:prefix(Filename, ConfigDPrefix) of
                                nomatch ->
                                    ok;
                                RelPath ->
                                    % Write the file to the priv/config.d directory
                                    DAbsPath = filename:join([ PrivDir, "config.d", RelPath ]),
                                    ok = z_filelib:ensure_dir(DAbsPath),
                                    case file:write_file(DAbsPath, Content) of
                                        ok ->
                                            ok;
                                        {error, DReason} ->
                                            ?LOG_ERROR(#{
                                                text => <<"Restore partially failed: cannot write config.d file">>,
                                                in => zotonic_mod_backup,
                                                result => error,
                                                reason => DReason,
                                                filename => DAbsPath
                                            })
                                    end
                            end,
                            case string:prefix(Filename, ConfigFilePrefix) of
                                nomatch ->
                                    ok;
                                "in" ->
                                    % Skip "zotonic_site.config.in" files
                                    ok;
                                Extension ->
                                    % Write the file to the priv directory
                                    FAbsPath = filename:join([ PrivDir, "zotonic_site." ++ Extension ]),
                                    case file:write_file(FAbsPath, Content) of
                                        ok ->
                                            ok;
                                        {error, FReason} ->
                                            ?LOG_ERROR(#{
                                                text => <<"Restore partially failed: cannot write zotonic_site config file">>,
                                                in => zotonic_mod_backup,
                                                result => error,
                                                reason => FReason,
                                                filename => FAbsPath
                                            })
                                    end
                            end
                        end,
                        Files)
            end;
        {error, Reason} = Error ->
             ?LOG_ERROR(#{
                text => <<"Restore failed: cannot open config tar for config files">>,
                in => zotonic_mod_backup,
                result => error,
                reason => Reason,
                filename => BackupDecrypted
            }),
            Error
    end.


%% @doc Unpack the files tar in the archive directory.
maybe_restore_files_backup(#{ <<"files">> := BackupFile }, _Options, Context) when is_binary(BackupFile) ->
    case is_file_missing(BackupFile, Context) of
        true ->
            ?LOG_ERROR(#{
                in => zotonic_mod_backup,
                text => <<"Files backup-file missing">>,
                result => error,
                reason => incomplete,
                files => BackupFile
            }),
            {error, incomplete};
        false ->
            % Files are there - try to unpack
            BackupPath = filename:join(backup_create:dir(Context), BackupFile),
            case maybe_decrypt_file(BackupPath, Context) of
                {ok, BackupDecrypted} ->
                    restore_files_backup(BackupDecrypted, Context);
                {error, _} = Error ->
                    Error
            end
    end;
maybe_restore_files_backup(_Backup, _Options, _Context) ->
    ok.

restore_files_backup(BackupFile, Context) ->
    case backup_create:command_configuration() of
        {ok, #{ archive := Tar }} ->
            case ensure_archive_dir(Context) of
                {error, _} = Error ->
                    Error;
                ArchiveDir ->
                    Command = unicode:characters_to_list([
                        z_filelib:os_filename(Tar),
                        " -C ", z_filelib:os_filename(ArchiveDir),
                        " -x -z ",
                        " -f ", z_filelib:os_filename(BackupFile)
                    ]),
                    case z_exec:run(Command) of
                        {ok, <<>>} ->
                            ok;
                        {ok, Output} ->
                             ?LOG_ERROR(#{
                                text => <<"Restore failed: tar error">>,
                                in => zotonic_mod_backup,
                                result => error,
                                reason => tar,
                                output => Output,
                                command => unicode:characters_to_binary(Command)
                            }),
                            {error, files_archive};
                        {error, Reason} ->
                             ?LOG_ERROR(#{
                                text => <<"Restore failed: tar error">>,
                                in => zotonic_mod_backup,
                                result => error,
                                reason => Reason,
                                command => unicode:characters_to_binary(Command)
                            }),
                            {error, files_archive}
                    end
            end;
        {error, Reason} = Error ->
             ?LOG_ERROR(#{
                text => <<"Restore failed: tar command missing">>,
                in => zotonic_mod_backup,
                result => error,
                reason => Reason
            }),
            Error
    end.

ensure_archive_dir(Context) ->
    Dir = z_path:media_archive(Context),
    case z_filelib:ensure_dir(filename:join([Dir, ".empty"])) of
        ok -> Dir;
        {error, _} = Error -> Error
    end.

maybe_restore_database_backup(#{ <<"database">> := BackupFile }, _Options, Context) when is_binary(BackupFile) ->
    case is_file_missing(BackupFile, Context) of
        true ->
            ?LOG_ERROR(#{
                in => zotonic_mod_backup,
                text => <<"Database backup file missing">>,
                result => error,
                reason => incomplete,
                files => BackupFile
            }),
            {error, incomplete};
        false ->
            % Files are there - try to unpack
            BackupPath = filename:join(backup_create:dir(Context), BackupFile),
            case maybe_decrypt_file(BackupPath, Context) of
                {ok, BackupDecrypted} ->
                    restore_database_backup_file(BackupDecrypted, Context);
                {error, _} = Error ->
                    Error
            end
    end;
maybe_restore_database_backup(_Status, _Options, _Context) ->
    ok.

restore_database_backup_file(BackupDecrypted, Context) ->
    % 1. Check schema of the DB dump
    case fetch_sql_schema(BackupDecrypted) of
        {ok, BackupSchema} ->
            DbOpts = z_db_pool:get_database_options(Context),
            DbSchema = z_convert:to_binary(proplists:get_value(dbschema, DbOpts)),
            if
                DbSchema =:= BackupSchema ->
                    % 2. Pause all db activity
                    case z_db_pool:get_unpaused_connection(Context) of
                        {ok, ConnPid} ->
                            % 3. Rename the schema
                            Timeout = 60000,
                            TempSchema = <<DbSchema/binary, "_temp">>,
                            {ok, [], []} = z_db_pgsql:squery(
                                ConnPid,
                                iolist_to_binary([
                                    "DROP SCHEMA IF EXISTS \"", TempSchema, "\" CASCADE"
                                ]),
                                Timeout),
                            {ok, [], []} = z_db_pgsql:squery(
                                ConnPid,
                                iolist_to_binary([
                                    "ALTER SCHEMA \"", DbSchema, "\""
                                    "RENAME TO \"", TempSchema, "\""
                                ]),
                                30000),
                            % 4. Import the dump
                            case import_dump(BackupDecrypted, DbOpts, Context) of
                                ok ->
                                    % 5. Drop the tmp schema
                                    {ok, [], []} = z_db_pgsql:squery(
                                        ConnPid,
                                        iolist_to_binary([
                                            "DROP SCHEMA IF EXISTS \"", TempSchema, "\" CASCADE"
                                        ]),
                                        Timeout),
                                    ok;
                                {error, _} = Error ->
                                    % Recover previous schema
                                    {ok, [], []} = z_db_pgsql:squery(
                                        ConnPid,
                                        iolist_to_binary([
                                            "DROP SCHEMA IF EXISTS \"", DbSchema, "\" CASCADE"
                                        ]),
                                        Timeout),
                                    {ok, [], []} = z_db_pgsql:squery(
                                        ConnPid,
                                        iolist_to_binary([
                                            "ALTER SCHEMA \"", TempSchema, "\""
                                            "RENAME TO \"", DbSchema, "\""
                                        ]),
                                        30000),
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                true ->
                    {error, schema_mismatch}
            end;
        {error, _} = Error ->
            Error
    end.

import_dump(Filename, DbOpts, Context) ->
    Host = proplists:get_value(dbhost, DbOpts),
    Port = proplists:get_value(dbport, DbOpts),
    User = proplists:get_value(dbuser, DbOpts),
    Database = proplists:get_value(dbdatabase, DbOpts),
    {ok, PgPass} = backup_create:pg_passfile(DbOpts, Context),
    Cmd = unicode:characters_to_list([
        " ",
        "gunzip -c ", z_filelib:os_filename(Filename), " | ",
        "PGPASSFILE=", z_filelib:os_filename(PgPass), " ",
        "psql -q "
            " -X ",
            " -h ", z_filelib:os_filename(Host),
            " -p ", z_convert:to_list(Port),
            " -U ", z_filelib:os_filename(User),
            " -w ",
            Database
    ]),
    case z_exec:run(Cmd) of
        {ok, _} ->
            ok;
        {error, _} = Error ->
            Error
    end.

fetch_sql_schema(Filename) ->
    case file:open(Filename, [ read, raw, binary ]) of
        {ok, Fd} ->
            case file:read(Fd, 5000) of
                {ok, Compressed} ->
                    file:close(Fd),
                    case partial_unzip(Compressed, 10000) of
                        {ok, Data} ->
                            case re:run(Data, <<"CREATE SCHEMA \"(.*?)\"">>, [{capture, all_but_first, binary}]) of
                                {match, [Schema]} ->
                                    {ok, Schema};
                                nomatch ->
                                    {error, no_schema}
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    file:close(Fd),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

partial_unzip(Compressed, MaxLength) ->
    Z = zlib:open(),
    zlib:inflateInit(Z, 16 + 15),
    try
        Uncompressed = unzip_loop(Z, <<>>, zlib:safeInflate(Z, Compressed), MaxLength),
        {ok, Uncompressed}
    catch
        _:_ ->
            {error, gunzip}
    after
        zlib:close(Z)
    end.

unzip_loop(_Z, Acc, _, MaxLength) when size(Acc) >= MaxLength ->
    Acc;
unzip_loop(Z, Acc, {continue, Output}, MaxLength) ->
    Out1 = iolist_to_binary(Output),
    Acc1 = <<Acc/binary, Out1/binary>>,
    Next = try
        zlib:safeInflate(Z, [])
    catch
        _:_ ->
            {finished, <<>>}
    end,
    unzip_loop(Z, Acc1, Next, MaxLength);
unzip_loop(_Z, Acc, {finished, Output}, _MaxLength) ->
    Out1 = iolist_to_binary(Output),
    <<Acc/binary, Out1/binary>>.

newest_backup(BackupStatus) ->
    {Newest, _} = maps:fold(
        fun
            (_Name, #{ <<"timestamp">> := Tm } = Status, {_, Newest}) when Tm > Newest ->
                {Status, Tm};
            (_Name, _, Acc) ->
                Acc
        end,
        {undefined, 0},
        BackupStatus),
    Newest.

maybe_decrypt_file(Filename, Context) ->
    case backup_file_crypto:is_encrypted(Filename) of
        true ->
            Password = m_config:get_value(?MODULE, backup_encrypt_password, Context),
            backup_file_crypto:password_decrypt(Filename, Password);
        false ->
            {ok, Filename}
    end.

%% @doc Check if a file is missing from the backup directory.
is_file_missing(undefined, _Context) ->
    false;
is_file_missing(<<>>, _Context) ->
    false;
is_file_missing(Filename, Context) ->
    LocalFile = filename:join(backup_create:dir(Context), Filename),
    (not filelib:is_regular(LocalFile)) orelse filelib:file_size(LocalFile) =:= 0.
