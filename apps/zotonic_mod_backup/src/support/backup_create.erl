-module(backup_create).

-export([
    make_backup/4,

    dir/1,
    pg_passfile/2,

    command_configuration/0,

    update_admin_file/2,
    read_admin_file/1,
    read_json_file/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include("../backup.hrl").

make_backup(DT, Name, IsFullBackup, Context) ->
    Result = do_backup_process(Name, IsFullBackup, Context),
    Result1 = maybe_encrypt_files(Result, Context),
    register_backup_in_admin_file(DT, Name, Result1, Context).

%% @doc Return and ensure the backup directory
dir(Context) ->
    z_path:files_subdir_ensure(<<"backup">>, Context).

%% @doc Create a file with the password for the psql connectie
pg_passfile(DbOpts, Context) ->
    Host = proplists:get_value(dbhost, DbOpts),
    Port = proplists:get_value(dbport, DbOpts),
    User = proplists:get_value(dbuser, DbOpts),
    Password = proplists:get_value(dbpassword, DbOpts),
    Database = proplists:get_value(dbdatabase, DbOpts),
    PgPass = filename:join([dir(Context), ".pgpass"]),
    ok = file:write_file(PgPass, iolist_to_binary([
        Host, $:, z_convert:to_binary(Port), $:,
        Database, $:, User, $:, Password
    ])),
    ok = file:change_mode(PgPass, 8#00600),
    {ok, PgPass}.

%% @doc Let backup wait till upload is finished. This prevents a problem where a backup file
%% is overwritten during upload or the backup.json is changed by the uploader during the backup.
do_backup_process(Name, IsFullBackup, Context) ->
    case mod_backup:is_uploading(Context) of
        true ->
            timer:sleep(1000),
            do_backup_process(Name, IsFullBackup, Context);
        false ->
            do_backup_process_1(Name, IsFullBackup, Context)
    end.

do_backup_process_1(Name, IsFullBackup, Context) ->
    IsFilesBackup = IsFullBackup andalso not backup_config:is_filestore_enabled(Context),
    case command_configuration() of
        {ok, Cmds} ->
            ?LOG_INFO(#{
                text => <<"Backup starting">>,
                in => zotonic_mod_backup,
                full_backup => IsFilesBackup,
                name => Name
            }),
            case pg_dump(Name, maps:get(db_dump, Cmds), Context) of
                {ok, DumpFile} ->
                    {ok, ConfigTarFile} = archive_config(Name, Context),
                    case IsFilesBackup of
                        true ->
                            case archive(Name, maps:get(archive, Cmds), Context) of
                                {ok, TarFile} ->
                                    ?LOG_INFO(#{
                                        text => <<"Backup finished">>,
                                        in => zotonic_mod_backup,
                                        result => ok,
                                        full_backup => IsFilesBackup,
                                        name => Name,
                                        database => DumpFile,
                                        config_files => ConfigTarFile,
                                        files => TarFile
                                    }),
                                    {ok, #{
                                        database => DumpFile,
                                        database_hash => hash_file(DumpFile, Context),
                                        config_files => ConfigTarFile,
                                        config_files_hash => hash_file(ConfigTarFile, Context),
                                        files => TarFile,
                                        files_hash => hash_file(TarFile, Context)
                                    }};
                                {error, _} ->
                                    % Ignore failed tar, at least register the db dump
                                    {ok, #{
                                        database => DumpFile,
                                        database_hash => hash_file(DumpFile, Context),
                                        config_files => ConfigTarFile,
                                        config_files_hash => hash_file(ConfigTarFile, Context)
                                    }}
                            end;
                        false ->
                            ?LOG_INFO(#{
                                text => <<"Backup finished">>,
                                in => zotonic_mod_backup,
                                result => ok,
                                full_backup => IsFilesBackup,
                                name => Name,
                                database => DumpFile,
                                config_files => ConfigTarFile,
                                files => none
                            }),
                            {ok, #{
                                database => DumpFile,
                                database_hash => hash_file(DumpFile, Context),
                                config_files => ConfigTarFile,
                                config_files_hash => hash_file(ConfigTarFile, Context)
                            }}
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                text => <<"Backup failed: configuration is wrong">>,
                in => zotonic_mod_backup,
                full_backup => IsFilesBackup,
                name => Name,
                result => error,
                reason => Reason
            }),
            Error
    end.

hash_file(undefined, _Context) ->
    undefined;
hash_file(Filename, Context) ->
    Path = filename:join(dir(Context), Filename),
    {ok, Hash} = z_utils:hex_sha2_file(Path),
    Hash.

maybe_encrypt_files({ok, Files}, Context) ->
    case m_config:get_boolean(?MODULE, encrypt_backups, Context) of
        true ->
            case m_config:get_value(?MODULE, backup_encrypt_password, Context) of
                Password when is_binary(Password) andalso size(Password) > 0 ->
                    ?LOG_INFO(#{
                                text => <<"Encrypting backup">>,
                                in => zotonic_mod_backup
                               }),

                    Dir = dir(Context),
                    Files1 = maps:map(fun(_K, File) ->
                        FullName = filename:join(Dir, File),
                        {ok, FullNameEnc} = backup_file_crypto:password_encrypt(FullName, Password),
                        ok = file:delete(FullName),
                        filename:basename(FullNameEnc)
                    end,
                    Files),

                    ?LOG_INFO(#{
                        text => <<"Encryption done">>,
                        in => zotonic_mod_backup,
                        result => ok,
                        encrypted => Files1
                    }),

                    {ok, Files1};
                _ ->
                    ?LOG_WARNING(#{
                       text => <<"Could not encrypt backups. Encryption is enabled, but there is no backup password.">>,
                       in => zotonic_mod_backup,
                       result => error,
                       reason => no_backup_encrypt_password
                    }),
                    {ok, Files}
            end;
        false ->
            {ok, Files}
    end;
maybe_encrypt_files({error, _}=Error, _Context) ->
    Error.


register_backup_in_admin_file(DT, Name, {ok, Files}, Context) ->
    F = fun(Data) ->
        Data1 = Data#{
            Name => #{
                <<"timestamp">> => z_datetime:datetime_to_timestamp(DT),

                <<"database">> => maps:get(database, Files),
                <<"database_hash">> => maps:get(database_hash, Files, undefined),
                <<"config_files">> => maps:get(config_files, Files),
                <<"config_files_hash">> => maps:get(config_files_hash, Files, undefined),
                <<"files">> => maps:get(files, Files, undefined),
                <<"files_hash">> => maps:get(files_hash, Files, undefined),

                <<"is_filestore_uploaded">> => false,

                <<"is_encrypted">> => m_config:get_boolean(?MODULE, encrypt_backups, Context)
                    andalso (size(m_config:get_value(?MODULE, backup_encrypt_password, <<>>,  Context)) > 0)
            }
        },
        % Delete the Sunday dump, it has been replaced by a weekly dump.
        drop_old_sunday_dump(Data1, Context)
    end,
    update_admin_file(F, Context);
register_backup_in_admin_file(_DT, Name, {error, _}, Context) ->
    % Delete Name, backup failed
    F = fun(Data) ->
        maybe_delete_files(maps:get(Name, Data, #{}), Context),
        maps:remove(Name, Data)
    end,
    update_admin_file(F, Context).


% Delete old Sunday dump files, Sunday has been replaced by a weekly dump.
% As the dump will not be overwritten, we need to remove the files manually.
drop_old_sunday_dump(Data, Context) ->
    maps:filter(
        fun(DumpName, #{ <<"timestamp">> := Timestamp } = D) ->
            case re:run(DumpName, <<"-7$">>) of
                nomatch ->
                    true;
                {match, _} ->
                    % Delete if older than a week
                    PrevWeek = z_datetime:prev_week(calendar:universal_time()),
                    PrevWeekTm = z_datetime:datetime_to_timestamp(PrevWeek),
                    if
                        Timestamp =< PrevWeekTm ->
                            maybe_delete_files(D, Context),
                            false;
                        true ->
                            true
                    end
            end
        end,
        Data).

maybe_delete_files(D, Context) ->
    maybe_delete_file(maps:get(<<"database">>, D, undefined), Context),
    maybe_delete_file(maps:get(<<"config_files">>, D, undefined), Context),
    maybe_delete_file(maps:get(<<"files">>, D, undefined), Context).

maybe_delete_file(undefined, _Context) -> ok;
maybe_delete_file(<<>>, _Context) -> ok;
maybe_delete_file(Filename, Context) ->
    Path = filename:join(dir(Context), Filename),
    file:delete(Path).


%% @doc Read the admin file, create an empty file if it doesn't exist.
-spec read_admin_file(Context) -> AdminData when
    Context :: z:context(),
    AdminData :: map().
read_admin_file(Context) ->
    Filename = filename:join(dir(Context), "backup.json"),
    case read_json_file(Filename) of
        {ok, JSON} ->
            JSON;
        {error, enoent} ->
            ?LOG_NOTICE(#{
                text => <<"Backup admin file missing, creating an empty file">>,
                in => zotonic_mod_backup,
                result => error,
                reason => enoent,
                admin_file => Filename
            }),
            #{};
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => <<"Backup admin file corrupt, resetting file">>,
                in => zotonic_mod_backup,
                result => error,
                reason => Reason,
                admin_file => Filename
            }),
            #{}
    end.

read_json_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Bin} ->
            try
                {ok, z_json:decode(Bin)}
            catch
                _:Reason ->
                    {error, Reason}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Update the admin file. Uses a jobs regulator to prevent parallel
%% updates.
-spec update_admin_file(UpdateFun, Context) -> {ok, AdminData} | {error, Reason} when
    UpdateFun :: fun( (map()) -> map() ),
    AdminData :: map(),
    Context :: z:context(),
    Reason :: file:posix() | term().
update_admin_file(UpdateFun, Context) ->
    jobs:run(?CONFIG_UPDATE_JOB, fun() ->
        Data = read_admin_file(Context),
        Data1 = UpdateFun(Data),
        write_admin_file(Data1, Context)
    end).

write_admin_file(Data, Context) ->
    Filename = filename:join(dir(Context), "backup.json"),
    FilenameTmp = filename:join(dir(Context), "backup.json.tmp"),
    JSON = z_json:encode(Data),
    case file:write_file(FilenameTmp, JSON) of
        ok ->
            file:rename(FilenameTmp, Filename),
            {ok, Data};
        {error, _} = Error ->
            Error
    end.

%% @doc Dump the sql database into the backup directory.  The Name is the basename of the dump.
pg_dump(Name, DbDump, Context) ->
    DbOpts = z_db_pool:get_database_options(Context),
    Host = proplists:get_value(dbhost, DbOpts),
    Port = proplists:get_value(dbport, DbOpts),
    User = proplists:get_value(dbuser, DbOpts),
    Database = proplists:get_value(dbdatabase, DbOpts),
    Schema = proplists:get_value(dbschema, DbOpts),

    Filename = <<Name/binary, ".sql.gz">>,
    TmpFilename = <<Name/binary, ".sql.gz.tmp">>,
    TmpDumpFile = filename:join(dir(Context), TmpFilename),
    {ok, PgPass} = pg_passfile(DbOpts, Context),
    Command = unicode:characters_to_list([
        "PGPASSFILE=",z_filelib:os_filename(PgPass)," ", z_filelib:os_filename(DbDump),
        " -h ", z_filelib:os_filename(Host),
        " -p ", z_convert:to_list(Port),
        " -w ",
        " --compress=7 "
        " --quote-all-identifiers ",
        " -f ", z_filelib:os_filename(TmpDumpFile), " ",
        " -U ", z_filelib:os_filename(User), " ",
        case z_utils:is_empty(Schema) of
            true -> [];
            false -> [" -n ", z_filelib:os_filename(Schema), " "]
        end,
        Database
    ]),
    z_proc:spawn_md(
            fun() ->
                timer:sleep(1000),
                z_mqtt:publish(
                    <<"model/backup/event/backup">>,
                    #{ status => <<"sql_backup_started">> },
                    Context)
            end),
    Result = case z_exec:run(Command) of
        {ok, <<>>} ->
            DumpFile = filename:join(dir(Context), Filename),
            ok = file:rename(TmpDumpFile, DumpFile),
            {ok, Filename};
        {ok, Output} ->
            ?LOG_WARNING(#{
                text => <<"Backup failed: pg_dump error">>,
                in => zotonic_mod_backup,
                result => error,
                reason => pg_dump,
                output => Output,
                command => unicode:characters_to_binary(Command)
            }),
            file:delete(TmpDumpFile),
            {error, database_archive};
        {error, Reason} ->
            ?LOG_WARNING(#{
                text => <<"Backup failed: pg_dump error">>,
                in => zotonic_mod_backup,
                result => error,
                reason => Reason,
                command => unicode:characters_to_binary(Command)
            }),
            file:delete(TmpDumpFile),
            {error, database_archive}
    end,
    ok = file:delete(PgPass),
    Result.


%% @doc Make a tar archive of all the files in the archive directory.
archive(Name, Tar, Context) ->
    ArchiveDir = z_path:media_archive(Context),
    case filelib:is_dir(ArchiveDir) of
        true ->
            Filename = <<Name/binary, ".tar.gz">>,
            DumpFile = filename:join(dir(Context), Filename),
            Command = unicode:characters_to_list([
                z_filelib:os_filename(Tar),
                " -c -z ",
                "-f ", z_filelib:os_filename(DumpFile), " ",
                "-C ", z_filelib:os_filename(ArchiveDir), " ",
                " ."
            ]),
            z_proc:spawn_md(
                    fun() ->
                        timer:sleep(1000),
                        z_mqtt:publish(<<"model/backup/event/backup">>, #{ status => <<"archive_backup_started">> }, Context)
                    end),
            case z_exec:run(Command) of
                {ok, <<>>} ->
                    {ok, Filename};
                {ok, Output} ->
                    file:delete(DumpFile),
                     ?LOG_WARNING(#{
                        text => <<"Backup failed: tar error">>,
                        in => zotonic_mod_backup,
                        result => error,
                        reason => tar,
                        output => Output,
                        command => unicode:characters_to_binary(Command)
                    }),
                    {error, files_archive};
                {error, Reason} ->
                    file:delete(DumpFile),
                     ?LOG_WARNING(#{
                        text => <<"Backup failed: tar error">>,
                        in => zotonic_mod_backup,
                        result => error,
                        reason => Reason,
                        command => unicode:characters_to_binary(Command)
                    }),
                    {error, files_archive}
            end;
        false ->
            %% No files uploaded
            {ok, undefined}
    end.

%% Make an archive of the configuraton and security files of a site.
archive_config(Name, Context) ->
    Site = z_context:site(Context),
    ConfigDirName = "config-" ++ z_convert:to_list(Site),

    %% Collect all config files.
    ConfigFiles = z_sites_config:config_files(Site),
    ConfigFileList = make_config_filelist(filename:join([ConfigDirName, config]), ConfigFiles, []),

    %% Collect all the security files of the site.
    {ok, SecurityDir} = z_sites_config:security_dir(Site),
    SecurityFiles = filelib:wildcard("**", SecurityDir),
    SecurityFileList = make_filelist(filename:join([ConfigDirName, security]),
                                     SecurityDir, SecurityFiles, []),

    %% Create the tarball.
    ConfigName = <<"config-", Name/binary>>,
    Dir = dir(Context),
    ArchiveName = <<ConfigName/binary,  ".tar.gz">>,
    filename:join([Dir, ArchiveName]),
    FileList = ConfigFileList ++ SecurityFileList,

    ConfigArchiveName = filename:join([Dir, ArchiveName]),
    ok = erl_tar:create(ConfigArchiveName, FileList, [compressed]),

    {ok, ArchiveName}.

make_config_filelist(_Prefix, [], Acc) ->
    lists:reverse(Acc);
make_config_filelist(Prefix, [Filename|Rest], Acc) ->
    SplitFilename = filename:split(Filename),
    ArchiveName = filename:join([Prefix | lists:nthtail(length(SplitFilename)-2, SplitFilename)]),
    make_config_filelist(Prefix, Rest, [{ArchiveName, Filename} | Acc]).


make_filelist(_Prefix, _Dir, [], Acc) ->
    lists:reverse(Acc);
make_filelist(Prefix, Dir, [File|Rest], Acc) ->
    ArchiveName = filename:join(Prefix, File),
    FullName = filename:join(Dir, File),
    make_filelist(Prefix, Dir, Rest, [{ArchiveName, FullName} | Acc]).

%% @doc Check if we can make backups, the configuration is ok
command_configuration() ->
    DbCmd = db_dump_cmd(),
    TarCmd = archive_cmd(),
    Db = os:find_executable(DbCmd),
    Tar = os:find_executable(TarCmd),
    if
        is_list(Db) andalso is_list(Tar) ->
            {ok, #{
                db_dump => Db,
                archive => Tar
            }};
        true ->
            ?LOG_WARNING(#{
                in => zotonic_mod_backup,
                text => <<"archive and/or pg_dump not found">>,
                result => error,
                reason => not_configured,
                db_dump => Db,
                db_dump_config => DbCmd,
                archive => Tar,
                archive_config => TarCmd
            }),
            {error, not_configured}
    end.

archive_cmd() ->
    unicode:characters_to_list(z_config:get(tar, "tar")).

db_dump_cmd() ->
    unicode:characters_to_list(z_config:get(pg_dump, "pg_dump")).

