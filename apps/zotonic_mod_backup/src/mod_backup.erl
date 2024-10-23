%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2023 Marc Worrell
%% @doc Backup module. Creates backup of the database and files.  Allows downloading of the backup.
%% Support creation of periodic backups.
%% @end

%% Copyright 2010-2023 Marc Worrell
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

-module(mod_backup).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Backup").
-mod_description("Make a backup of the database and files.").
-mod_prio(600).
-mod_provides([backup]).
-mod_depends([admin]).
-mod_schema(2).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    observe_admin_menu/3,
    observe_rsc_update_done/2,
    observe_rsc_upload/2,
    observe_search_query/2,
    observe_tick_24h/2,
    observe_m_config_update/2,

    start_backup/1,
    start_backup/2,
    list_backups/1,
    backup_in_progress/1,
    file_exists/2,
    file_forbidden/2,
    dir/1,
    check_configuration/0,
    is_filestore_enabled/1,
    is_uploading/1,
    manage_schema/2,

    read_admin_file/1
]).


-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


-record(state, {
    context :: z:context(),
    backup_start :: undefined | calendar:datetime(),
    backup_pid :: undefined | pid(),
    upload_start :: undefined | calendar:datetime(),
    upload_pid :: undefined | pid(),
    upload_name :: undefined | binary(),
    timer_ref :: timer:tref()
}).

% Interval for checking for new and/or changed files.
-define(BCK_POLL_INTERVAL, 3600 * 1000).

% Config values for daily backup
-define(BACKUP_NONE, 0).
-define(BACKUP_DB, 2).
-define(BACKUP_ALL, 1).


observe_rsc_upload(#rsc_upload{} = Upload, Context) ->
    backup_rsc_upload:rsc_upload(Upload, Context).

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
        % Menu to access all backups and settings
        #menu_item{
            id=admin_backup,
            parent=admin_modules,
            label=?__("Backup", Context),
            url={admin_backup},
            visiblecheck={acl, use, mod_backup}
        },

        % Menu to view and recover deleted pages
        #menu_separator{
            parent = admin_content,
            visiblecheck = {acl, use, mod_backup},
            sort=2000000
        },
        #menu_item{id=admin_backup_deleted,
            parent=admin_content,
            label=?__("Deleted pages", Context),
            url={admin_backup_deleted},
            visiblecheck={acl, use, mod_backup},
            sort=2000000
        }
        | Acc
    ].

observe_rsc_update_done(#rsc_update_done{ action = insert, id = Id, post_props = Props }, Context) ->
    m_backup_revision:save_revision(Id, Props, Context);
observe_rsc_update_done(#rsc_update_done{ action = update, id = Id, post_props = Props }, Context) ->
    m_backup_revision:save_revision(Id, Props, Context);
observe_rsc_update_done(#rsc_update_done{ action = delete, id = Id, pre_props = Props }, Context) ->
    m_backup_revision:save_deleted(Id, Props, Context);
observe_rsc_update_done(#rsc_update_done{}, _Context) ->
    ok.

observe_search_query(#search_query{ name = <<"backup_deleted">>, offsetlimit = OffsetLimit }, Context) ->
    case z_acl:is_allowed(use, mod_backup, Context) of
        true ->
            m_backup_revision:list_deleted(OffsetLimit, Context);
        false ->
            []
    end;
observe_search_query(#search_query{}, _Context) ->
    undefined.

observe_tick_24h(tick_24h, Context) ->
    m_backup_revision:periodic_cleanup(Context).

observe_m_config_update(#m_config_update{module=ModBackup, key=EncryptBackups}, Context)
  when (ModBackup == <<"mod_backup">> orelse ModBackup == ?MODULE)
       andalso (EncryptBackups == encrypt_backups orelse EncryptBackups == <<"encrypt_backups">>) ->
    % When the backup encryption is enabled, make sure there is an encryption password
    % in the config. When there is no password, generate a new one.
    case m_config:get_boolean(?MODULE, EncryptBackups, Context) of
        true ->
            case m_config:get_value(?MODULE, backup_encrypt_password, Context) of
                Password when is_binary(Password) andalso size(Password) > 0 ->
                    ok;
                _ ->
                    m_config:set_value(?MODULE, backup_encrypt_password, z_ids:password(), Context)
            end;
        false ->
            ok
    end;
observe_m_config_update(#m_config_update{}, _Context) ->
    ok.


%% @doc Callback for controller_file. Check if the file exists and return
%% the path to the file on disk.
-spec file_exists(File, Context) -> {true, FilePath} | false when
    File :: file:filename_all(),
    Context :: z:context(),
    FilePath :: file:filename_all().
file_exists(File, Context) ->
    Root = without_extension(File),
    Admin = read_admin_file(Context),

    case maps:get(Root, Admin, undefined) of
        undefined ->
            ?LOG_WARNING(#{
                text => <<"Download of backup file failed because backup does not exist">>,
                in => zotonic_mod_backup,
                result => error,
                reason => nobackup,
                backup => Root,
                file => File
            }),
            false;
        #{
            <<"database">> := Database,
            <<"files">> := Files
        } ->
            case File of
                Database ->
                    ?LOG_INFO(#{
                        text => <<"Download of database backup requested">>,
                        in => zotonic_mod_backup,
                        result => ok,
                        backup => Root,
                        file => File
                    }),
                    {true, filename:join([dir(Context), Database])};
                Files ->
                    ?LOG_INFO(#{
                        text => <<"Download of files backup requested">>,
                        in => zotonic_mod_backup,
                        result => ok,
                        backup => Root,
                        file => File
                    }),
                    {true, filename:join([dir(Context), Files])};
                _ ->
                    false
            end
    end.

%% @doc Callback for controller_file. Check if access is allowed.
-spec file_forbidden(File, Context) -> IsForbidden when
    File :: file:filename_all(),
    Context :: z:context(),
    IsForbidden :: boolean().
file_forbidden(_File, Context) ->
    IsAllowed = (z_acl:is_admin(Context) orelse z_acl:is_allowed(use, mod_backup, Context)),
    not IsAllowed.


%% @doc Start a full backup
start_backup(Context) ->
    start_backup(true, Context).

%% @doc Start a backup, either a full backup (including archived files) or a database only backup.
start_backup(IsFullBackup, Context) ->
    gen_server:call(z_utils:name_for_site(?MODULE, z_context:site(Context)), {start_backup, IsFullBackup}).

%% @doc List all backups present.  Newest first.
-spec list_backups( z:context() ) -> list( map() ).
list_backups(Context) ->
    Files = list_backup_files(Context),
    case gen_server:call(z_utils:name_for_site(?MODULE, z_context:site(Context)), in_progress_start) of
        undefined ->
            Files;
        InProgress ->
            [
                #{
                    timestamp => InProgress,
                    is_running => true
                }
                | Files
            ]
    end.

%% @doc Check if there is a backup in progress.
-spec backup_in_progress(z:context()) -> boolean().
backup_in_progress(Context) ->
    case gen_server:call(z_utils:name_for_site(?MODULE, z_context:site(Context)), in_progress_start) of
        undefined -> false;
        _ -> true
    end.


%% @doc Check if the cloud storage of files is enabled.
-spec is_filestore_enabled(z:context()) -> boolean().
is_filestore_enabled(Context) ->
    TestPath = <<"backup/backup.json">>,
    case z_notifier:first(#filestore_credentials_lookup{ path = TestPath}, Context) of
        {ok, #filestore_credentials{}} -> true;
        {error, _} -> false;
        undefined -> false
    end.

-spec is_uploading( z:context() ) -> boolean().
is_uploading(Context) ->
    gen_server:call(z_utils:name_for_site(?MODULE, Context), is_uploading).

manage_schema(_Version, Context) ->
    m_backup_revision:install(Context).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    Context = proplists:get_value(context, Args),
    Name = z_utils:name_for_site(?MODULE, z_context:site(Context)),
    gen_server:start_link({local, Name}, ?MODULE, Args, []).



%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    z_context:logger_md(Context),
    {ok, TimerRef} = timer:send_interval(?BCK_POLL_INTERVAL, periodic_backup),
    {ok, #state{
        context = z_acl:sudo(z_context:new(Context)),
        timer_ref = TimerRef
    }}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Start a backup
handle_call({start_backup, IsFullBackup}, _From, State) ->
    case State#state.backup_pid of
        undefined ->
            Now = calendar:universal_time(),
            Context = State#state.context,
            Pid = do_backup(Now, name(Context), IsFullBackup, Context),
            {reply, ok, State#state{backup_pid=Pid, backup_start=Now}};
        _Pid ->
            {reply, {error, in_progress}, State}
    end;

%% @doc Return the start datetime of the current running backup, if any.
handle_call(in_progress_start, _From, State) ->
    {reply, State#state.backup_start, State};

%% @doc Check if there is an upload process running.
handle_call(is_uploading, _From, State) ->
    {reply, is_pid(State#state.upload_pid), State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Periodic check if a scheduled backup should start
handle_info(periodic_backup, #state{ backup_pid = Pid } = State) when is_pid(Pid) ->
    {noreply, State};
handle_info(periodic_backup, #state{ upload_pid = Pid } = State) when is_pid(Pid) ->
    {noreply, State};
handle_info(periodic_backup, State) ->
    State1 = case daily_dump_config(State#state.context) of
        ?BACKUP_NONE -> State;
        ?BACKUP_ALL -> maybe_daily_dump(true, State);
        ?BACKUP_DB -> maybe_daily_dump(false, State)
    end,
    State2 = case State1#state.backup_pid of
        undefined ->
            % Only upload backup files if there are no backups running.
            maybe_filestore_upload(State1);
        _Pid ->
            State1
    end,
    {noreply, State2};

handle_info({'EXIT', Pid, normal}, #state{ backup_pid = Pid } = State) ->
    z_mqtt:publish(
        <<"model/backup/event/backup">>,
        #{ status => <<"completed">> },
        State#state.context),
    State1 = State#state{
        backup_pid = undefined,
        backup_start = undefined
    },
    State2 = maybe_filestore_upload(State1),
    {noreply, State2};

handle_info({'EXIT', Pid, Reason}, #state{ backup_pid = Pid } = State) ->
    ?LOG_ERROR(#{
        text => <<"Backup process crashed">>,
        in => zotonic_mod_backup,
        result => error,
        reason => Reason,
        pid => Pid
    }),
    z_mqtt:publish(
        <<"model/backup/event/backup">>,
        #{ status => <<"error">>},
        State#state.context),
    State1 = State#state{
        backup_pid = undefined,
        backup_start = undefined
    },
    {noreply, State1};

handle_info({'EXIT', Pid, normal}, #state{ upload_pid = Pid } = State) ->
    z_mqtt:publish(
        <<"model/backup/event/backup">>,
        #{ status => <<"uploaded">> },
        State#state.context),
    State1 = State#state{
        upload_pid = undefined,
        upload_start = undefined,
        upload_name = undefined
    },
    {noreply, State1};

handle_info({'EXIT', Pid, Reason}, #state{ upload_pid = Pid } = State) ->
    z_mqtt:publish(
        <<"model/backup/event/backup">>,
        #{ status => <<"upload_error">> },
        State#state.context),
    ?LOG_ERROR(#{
        text => <<"Backup uploader crashed">>,
        in => zotonic_mod_backup,
        result => error,
        reason => Reason,
        pid => Pid
    }),
    State1 = State#state{
        upload_pid = undefined,
        upload_start = undefined,
        upload_name = undefined
    },
    {noreply, State1};

%% @doc Handling all non call/cast messages
handle_info(Info, State) ->
    ?DEBUG(Info),
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    timer:cancel(State#state.timer_ref),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

daily_dump_config(Context) ->
    case m_config:get_value(mod_backup, daily_dump, Context) of
        <<"1">> ->
            case is_filestore_enabled(Context) of
                true -> ?BACKUP_DB;
                false -> ?BACKUP_ALL
            end;
        <<"2">> ->
            ?BACKUP_DB;
        _ ->
            ?BACKUP_NONE
    end.

maybe_daily_dump(IsFullBackup, State) ->
    Context = State#state.context,
    Now = {Date, Time} = calendar:universal_time(),
    case Time >= {3,0,0} of
        true ->
            Ts = lists:map(
                fun(#{ timestamp := T }) -> T end,
                list_backup_files(Context)),
            DoStart = case Ts of
                [] -> true;
                _ -> lists:max(Ts) < {Date, {0,0,0}}
            end,
            case DoStart of
                true ->
                    Pid = do_backup(Now, name(Context), IsFullBackup, Context),
                    State#state{
                        backup_pid = Pid,
                        backup_start = Now
                    };
                false ->
                    State
            end;
        false ->
            State
    end.

maybe_filestore_upload(#state{ context = Context } = State) ->
    case is_filestore_enabled(Context) of
        true ->
            % Check the backup.json if any files are not yet uploaded
            Data = read_admin_file(Context),
            ToUpload = maps:fold(
                fun(Nm, Bck, Acc) ->
                    case maps:get(<<"is_filestore_uploaded">>, Bck, false) of
                        true ->
                            Acc;
                        false ->
                            Db = maps:get(<<"database">>, Bck),
                            Tm = maps:get(<<"timestamp">>, Bck),
                            [ {Tm, Nm, Db} | Acc ]
                    end
                end,
                [],
                Data),
            case lists:sort(ToUpload) of
                [] ->
                    State;
                Sorted ->
                    % Start uploader for newest backup
                    {_, Name, DatabaseFile} = lists:last(Sorted),
                    Pid = do_upload(Name, DatabaseFile, Context),
                    State#state{
                        upload_start = calendar:universal_time(),
                        upload_pid = Pid,
                        upload_name = Name
                    }
            end;
        false ->
            State
    end.

do_upload(Name, DatabaseFile, Context) ->
    spawn_link(
        fun() ->
            RemoteDbFile = <<"backup/", DatabaseFile/binary>>,
            LocalDbFile = filename:join(dir(Context), DatabaseFile),
            case z_notifier:first(
                #filestore_request{
                    action = upload,
                    remote = RemoteDbFile,
                    local = LocalDbFile
                }, Context)
            of
                ok ->
                    Data = read_admin_file(Context),
                    Bck = maps:get(Name, Data),
                    Data1 = Data#{
                        Name => Bck#{
                            <<"is_filestore_uploaded">> => true
                        }
                    },
                    ok = write_admin_file(Data1, Context),
                    RemoteAdminFile = <<"backup/backup.json">>,
                    LocalAdminFile = filename:join(dir(Context), <<"backup.json">>),
                    case z_notifier:first(
                        #filestore_request{
                            action = upload,
                            remote = RemoteAdminFile,
                            local = LocalAdminFile
                        }, Context)
                    of
                        ok ->
                            ?LOG_INFO(#{
                                text => <<"Backup uploaded database file to filestore">>,
                                in => zotonic_mod_backup,
                                result => ok,
                                remote => RemoteDbFile,
                                local => LocalDbFile
                            }),
                            ok;
                        {error, Reason} = Error ->
                            ?LOG_ERROR(#{
                                text => <<"Backup error uploading backup.json to filestore">>,
                                in => zotonic_mod_backup,
                                result => error,
                                reason => Reason,
                                remote => RemoteAdminFile,
                                local => LocalAdminFile
                            }),
                            Error
                    end;
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        text => <<"Backup error uploading database file to filestore">>,
                        in => zotonic_mod_backup,
                        result => error,
                        reason => Reason,
                        remote => RemoteDbFile,
                        local => LocalDbFile
                    }),
                    Error;
                undefined ->
                    ok
            end
        end).

%% @doc Start a backup and return the pid of the backup process, whilst linking to the process.
do_backup(DT, Name, IsFullBackup, Context) ->
    z_mqtt:publish(<<"model/backup/event/backup">>, #{ status => <<"started">> }, Context),
    spawn_link(
        fun() ->
            % NEVER have an upload and backup in parallel
            Result = do_backup_process(Name, IsFullBackup, Context),
            Result1 = maybe_encrypt_files(Result, Context),
            update_admin_file(DT, Name, Result1, Context)
        end).


%% @doc Let backup wait till upload is finished. This prevents a problem where a backup file
%% is overwritten during upload or the backup.json is changed by the uploader during the backup.
do_backup_process(Name, IsFullBackup, Context) ->
    case is_uploading(Context) of
        true ->
            timer:sleep(1000),
            do_backup_process(Name, IsFullBackup, Context);
        false ->
            do_backup_process_1(Name, IsFullBackup, Context)
    end.

do_backup_process_1(Name, IsFullBackup, Context) ->
    IsFilesBackup = IsFullBackup andalso not is_filestore_enabled(Context),
    case check_configuration() of
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
                                           config_files => ConfigTarFile,
                                           files => TarFile
                                          }};
                                {error, _} ->
                                    % Ignore failed tar, at least register the db dump
                                    {ok, #{
                                        config_files => ConfigTarFile,
                                        database => DumpFile
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
                                config_files => ConfigTarFile
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
                                              {ok, FullNameEnc} = mod_backup_file_crypto:password_encrypt(FullName, Password),
                                              ok = file:delete(FullName),
                                              filename(FullNameEnc)
                                      end,
                                      Files),

                    ?LOG_INFO(#{
                                text => <<"Encryption done">>,
                                in => zotonic_mod_backup,
                                encrypted => Files1
                               }),

                    {ok, Files1};
                _ ->
                    ?LOG_WARNING(#{
                                   text => <<"Could not encrypt backups. Encryption is enabled, but there is no backup password.">>,
                                   in => zotonic_mod_backup
                                  }),
                    %% 
                    {ok, Files}
            end;
        false ->
            {ok, Files}
    end;
maybe_encrypt_files({error, _}=Error, _Context) ->
    Error.

filename(Fullname) ->
    lists:last(filename:split(Fullname)).

update_admin_file(DT, Name, {ok, Files}, Context) ->
    Data = read_admin_file(Context),

    Data1 = Data#{
        Name => #{
            timestamp => z_datetime:datetime_to_timestamp(DT),

            database => maps:get(database, Files),
            config_files => maps:get(config_files, Files),
            files => maps:get(files, Files, undefined),

            is_filestore_uploaded => false,

            is_encrypted => m_config:get_boolean(?MODULE, encrypt_backups, Context) 
                andalso (size(m_config:get_value(?MODULE, backup_encrypt_password, <<>>,  Context)) > 0)
        }
    },

    write_admin_file(Data1, Context);
update_admin_file(_DT, Name, {error, _}, Context) ->
    % Delete Name, backup failed
    Data = read_admin_file(Context),
    Data1 = maps:remove(Name, Data),
    write_admin_file(Data1, Context).

read_admin_file(Context) ->
    Filename = filename:join(dir(Context), "backup.json"),
    case file:read_file(Filename) of
        {ok, Bin} ->
            try
                z_json:decode(Bin)
            catch
                Err:Reason ->
                    ?LOG_ERROR(#{
                        text => <<"Backup admin file corrupt, resetting file">>,
                        in => zotonic_mod_backup,
                        result => Err,
                        reason => Reason,
                        admin_file => Filename
                    }),
                    #{}
            end;
        {error, _} ->
            #{}
    end.

write_admin_file(Data, Context) ->
    Filename = filename:join(dir(Context), "backup.json"),
    FilenameTmp = filename:join(dir(Context), "backup.json.tmp"),
    JSON = z_json:encode(Data),
    case file:write_file(FilenameTmp, JSON) of
        ok ->
            file:rename(FilenameTmp, Filename);
        {error, _} = Error ->
            Error
    end.

%% @doc List all backups in the backup directory.
list_backup_files(Context) ->
    Data = read_admin_file(Context),
    List = maps:fold(
        fun(Name, Dump, Acc) ->
            Timestamp = maps:get(<<"timestamp">>, Dump),
            IsEncrypted = (maps:get(<<"is_encrypted">>, Dump, false) =:= true),
            IsDatabase = (maps:get(<<"database">>, Dump, undefined) =/= undefined),
            IsFiles = (maps:get(<<"files">>, Dump, undefined) =/= undefined),
            IsConfigFiles = (maps:get(<<"config_files">>, Dump, undefined) =/= undefined),
            [
                {Timestamp, #{
                    name => Name,
                    timestamp => z_datetime:timestamp_to_datetime(Timestamp),
                    is_encrypted => IsEncrypted,
                    is_database_present => IsDatabase,
                    is_files_present => IsFiles,
                    is_config_files_present => IsConfigFiles,
                    is_filestore_uploaded => maps:get(<<"is_filestore_uploaded">>, Dump, false)
                }}
                | Acc
            ]
        end,
        [],
        Data),
    List1 = lists:reverse(lists:sort(List)),
    [ V || {_, V} <- List1 ].

%% @doc Return and ensure the backup directory
dir(Context) ->
    z_path:files_subdir_ensure("backup", Context).

%% @doc Return the base name of the backup files.
name(Context) ->
    {Date, _} = calendar:universal_time(),
    Day = calendar:day_of_the_week(Date),
    iolist_to_binary([
        atom_to_list(z_context:site(Context)),
        "-",
        integer_to_binary(Day)
    ]).

%% @doc Dump the sql database into the backup directory.  The Name is the basename of the dump.
pg_dump(Name, DbDump, Context) ->
    DbOpts = z_db_pool:get_database_options(Context),
    Host = proplists:get_value(dbhost, DbOpts),
    Port = proplists:get_value(dbport, DbOpts),
    User = proplists:get_value(dbuser, DbOpts),
    Password = proplists:get_value(dbpassword, DbOpts),
    Database = proplists:get_value(dbdatabase, DbOpts),
    Schema = proplists:get_value(dbschema, DbOpts),

    Filename = <<Name/binary, ".sql.gz">>,
    DumpFile = filename:join(dir(Context), Filename),
    PgPass = filename:join([dir(Context), ".pgpass"]),
    ok = file:write_file(PgPass, z_convert:to_list(Host)
                                ++":"++z_convert:to_list(Port)
                                ++":"++z_convert:to_list(Database)
                                ++":"++z_convert:to_list(User)
                                ++":"++z_convert:to_list(Password)),
    ok = file:change_mode(PgPass, 8#00600),
    Command = unicode:characters_to_list([
        "PGPASSFILE=",z_filelib:os_filename(PgPass)," ", z_filelib:os_filename(DbDump),
        " -h ", z_filelib:os_filename(Host),
        " -p ", z_convert:to_list(Port),
        " -w ",
        " --compress=7 "
        " --quote-all-identifiers ",
        " -f ", z_filelib:os_filename(DumpFile), " ",
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
    Result = case os:cmd(Command) of
        [] ->
            {ok, Filename};
        Output ->
            ?LOG_WARNING(#{
                text => <<"Backup failed: pg_dump error">>,
                in => zotonic_mod_backup,
                result => error,
                reason => pg_dump,
                output => Output,
                command => unicode:characters_to_binary(Command)
            }),
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
            case os:cmd(Command) of
                "" ->
                    {ok, Filename};
                Output ->
                     ?LOG_WARNING(#{
                        text => <<"Backup failed: tar error">>,
                        in => zotonic_mod_backup,
                        result => error,
                        reason => tar,
                        output => Output,
                        command => unicode:characters_to_binary(Command)
                    }),
                    {error, files_archive}
            end;
        false ->
            %% No files uploaded
            ok
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


%%
%% Helpers
%%

%% Strip all extensions from a filename.
without_extension(Filename) ->
    case filename:rootname(Filename) of
        Filename -> Filename;
        Root -> without_extension(Root)
    end.

%% @doc Check if we can make backups, the configuration is ok
check_configuration() ->
    Db = os:find_executable(db_dump_cmd()),
    Tar = os:find_executable(archive_cmd()),
    if
        is_list(Db) andalso is_list(Tar) ->
            {ok, #{
                db_dump => Db,
                archive => Tar
            }};
        true ->
            {error, not_configured}
    end.

archive_cmd() ->
    unicode:characters_to_list(z_config:get(tar, "tar")).

db_dump_cmd() ->
    unicode:characters_to_list(z_config:get(pg_dump, "pg_dump")).
