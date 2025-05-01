%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2025 Marc Worrell
%% @doc Backup module. Creates backup of the database and files.  Allows downloading of the backup.
%% Support creation of periodic backups.
%% @end

%% Copyright 2010-2025 Marc Worrell
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
-mod_depends([]).
-mod_schema(4).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    observe_admin_menu/3,

    observe_rsc_delete/2,
    observe_rsc_update_done/2,
    observe_rsc_upload/2,

    observe_search_query/2,
    observe_tick_24h/2,
    observe_m_config_update/2,

    observe_edge_insert/2,
    observe_edge_delete/2,
    observe_media_update_done/2,

    observe_backup_list/2,
    observe_backup_start/2,
    observe_backup_restore/2,

    start_backup/1,
    start_backup/2,
    list_backups/1,

    restore_backup/1,
    restore_backup/3,

    file_exists/2,
    file_forbidden/2,

    backup_in_progress/1,
    is_uploading/1,

    manage_schema/2
]).


-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

-include("backup.hrl").

-record(state, {
    is_env_backup = false :: boolean(),
    context :: z:context(),
    backup_start :: undefined | calendar:datetime(),
    backup_pid :: undefined | pid(),
    upload_start :: undefined | calendar:datetime(),
    upload_pid :: undefined | pid(),
    upload_name :: undefined | binary(),
    download_start :: undefined | calendar:datetime(),
    download_pid :: undefined | pid(),
    timer_ref :: timer:tref()
}).

% Interval for checking for new and/or changed files.
-define(BCK_POLL_INTERVAL, 3600 * 1000).

% Number of weekly backups to keep
-define(WEEKLY_BACKUPS, 5).


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

observe_rsc_delete(#rsc_delete{ id = Id }, Context) ->
    m_backup_revision:medium_delete_check(Id, Context).

observe_rsc_update_done(#rsc_update_done{ action = insert, id = Id, post_props = Props }, Context) ->
    m_backup_revision:save_revision(Id, Props, Context);
observe_rsc_update_done(#rsc_update_done{ action = update, id = Id, post_props = Props }, Context) ->
    m_backup_revision:save_revision(Id, Props, Context);
observe_rsc_update_done(#rsc_update_done{ action = delete, id = Id, pre_props = Props }, Context) ->
    m_backup_revision:save_deleted(Id, Props, Context);
observe_rsc_update_done(#rsc_update_done{}, _Context) ->
    ok.

observe_edge_delete(#edge_delete{ subject_id = Id, predicate = Predicate, object_id = ObjId }, Context) ->
    m_backup_revision:edge_delete(Id, Predicate, ObjId, Context).

observe_edge_insert(#edge_insert{ subject_id = Id, predicate = Predicate, object_id = ObjId }, Context) ->
    m_backup_revision:edge_insert(Id, Predicate, ObjId, Context).

observe_media_update_done(#media_update_done{ id = Id, action = insert, post_props = Props }, Context) ->
    m_backup_revision:medium_insert(Id, Props, Context);
observe_media_update_done(#media_update_done{ id = Id, action = update, post_props = Props }, Context) ->
    m_backup_revision:medium_update(Id, Props, Context);
observe_media_update_done(#media_update_done{ id = Id, action = delete, pre_props = Props }, Context) ->
    m_backup_revision:medium_delete(Id, Props, Context).


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
    case m_site:environment(Context) of
        backup -> ok;
        _Env -> m_backup_revision:periodic_cleanup(Context)
    end.

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


observe_backup_list(backup_list, Context) ->
    list_backups(Context).

observe_backup_start(backup_start, Context) ->
    start_backup(Context).

observe_backup_restore({backup_restore, Backup, Options}, Context) ->
    restore_backup(Backup, Options, Context).


%% @doc Callback for controller_file. Check if the backup file exists and return
%% the path to the file on disk.
-spec file_exists(File, Context) -> {true, FilePath} | false when
    File :: file:filename_all(),
    Context :: z:context(),
    FilePath :: file:filename_all().
file_exists(File, Context) ->
    Root = without_extension(File),
    Admin = backup_create:read_admin_file(Context),

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
            <<"database">> := DatabaseDump,
            <<"files">> := FilesTar
        } ->
            if
                File =:= DatabaseDump ->
                    ?LOG_INFO(#{
                        text => <<"Download of database backup requested">>,
                        in => zotonic_mod_backup,
                        result => ok,
                        backup => Root,
                        file => File
                    }),
                    {true, filename:join([backup_create:dir(Context), DatabaseDump])};
                File =:= FilesTar ->
                    ?LOG_INFO(#{
                        text => <<"Download of files backup requested">>,
                        in => zotonic_mod_backup,
                        result => ok,
                        backup => Root,
                        file => File
                    }),
                    {true, filename:join([backup_create:dir(Context), FilesTar])};
                true ->
                    false
            end
    end.

%% @doc Callback for controller_file. Check if access to the backup file is allowed.
-spec file_forbidden(File, Context) -> IsForbidden when
    File :: file:filename_all(),
    Context :: z:context(),
    IsForbidden :: boolean().
file_forbidden(File, Context) ->
    case (z_acl:is_admin(Context) orelse z_acl:is_allowed(use, mod_backup, Context))
        andalso backup_config:allow_backup_download(Context)
    of
        true ->
            false;
        false ->
            ?LOG_WARNING(#{
                text => <<"Download of backup file failed because access is forbidden">>,
                in => zotonic_mod_backup,
                result => error,
                reason => eacces,
                file => File
            }),
            true
    end.

%% @doc Restore the most recent backup
-spec restore_backup(Context) -> ok | {error, Reason} when
    Context :: z:context(),
    Reason :: atom().
restore_backup(Context) ->
    Options = backup_restore:default_options(),
    restore_backup(recent, Options, Context).

%% @doc Restore a specific backup. Pass the name of the backup.
%% The name is like "sitename-N" where N is 1..6 or w1..w4.
-spec restore_backup(Name, Options, Context) -> ok | {error, Reason} when
    Name :: binary() | recent,
    Options :: backup_restore:restore_options(),
    Context :: z:context(),
    Reason :: atom().
restore_backup(<<"recent">>, Options, Context) ->
    restore_backup(recent, Options, Context);
restore_backup(Name, Options, Context) ->
    case backup_in_progress(Context) of
        true ->
            {error, in_progress};
        false ->
            case is_uploading(Context) of
                true ->
                    {error, uploading};
                false ->
                    restore_backup_1(Name, Options, Context)
            end
    end.

restore_backup_1(recent, Options, Context) ->
    backup_restore:restore_newest_backup(Options, Context);
restore_backup_1(Name, Options, Context) ->
    backup_restore:restore_backup(Name, Options, Context).


%% @doc Start a full backup
start_backup(Context) ->
    start_backup(true, Context).

%% @doc Start a backup, either a full backup (including archived files) or a database only backup.
start_backup(IsFullBackup, Context) ->
    gen_server:call(z_utils:name_for_site(?MODULE, Context), {start_backup, IsFullBackup}).

%% @doc List all backups present.  Newest first.
-spec list_backups( z:context() ) -> list( map() ).
list_backups(Context) ->
    Files = list_backup_files(Context),
    case gen_server:call(z_utils:name_for_site(?MODULE, Context), in_progress_start) of
        undefined ->
            Files;
        InProgress ->
            [
                #{
                    timestamp => InProgress,
                    is_running => true,
                    name => iolist_to_binary([ $(, name(Context), $) ])
                }
                | Files
            ]
    end.

%% @doc Check if there is a backup in progress.
-spec backup_in_progress(z:context()) -> boolean().
backup_in_progress(Context) ->
    case gen_server:call(z_utils:name_for_site(?MODULE, Context), in_progress_start) of
        undefined -> false;
        _ -> true
    end.

-spec is_uploading( z:context() ) -> boolean().
is_uploading(Context) ->
    gen_server:call(z_utils:name_for_site(?MODULE, Context), is_uploading).

manage_schema(_Version, Context) ->
    m_backup_revision:install(Context).


%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    Context = proplists:get_value(context, Args),
    Name = z_utils:name_for_site(?MODULE, z_context:site(Context)),
    gen_server:start_link({local, Name}, ?MODULE, Args, []).



%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    z_context:logger_md(Context),
    IsEnvBackup = (m_site:environment(Context) =:= backup),
    % A jobs queue to ensure that we only run a single Database dump at a time.
    ensure_job_queue(?MODULE, [ {regulators, [{counter, [{limit, 1} ]} ]} ]),
    ensure_job_queue(?CONFIG_UPDATE_JOB, [ {regulators, [{counter, [{limit, 1} ]} ]} ]),
    BackupTimerMessage = case IsEnvBackup of
        true -> periodic_download;
        false -> periodic_backup
    end,
    {ok, TimerRef} = timer:send_interval(?BCK_POLL_INTERVAL, BackupTimerMessage),
    {ok, #state{
        context = z_acl:sudo(z_context:new(Context)),
        is_env_backup = IsEnvBackup,
        timer_ref = TimerRef
    }}.

%% @doc Start a backup
handle_call({start_backup, IsFullBackup}, _From, #state{ backup_pid = undefined, is_env_backup = false } = State) ->
    Now = calendar:universal_time(),
    Context = State#state.context,
    Pid = do_backup(Now, name(Context), IsFullBackup, Context),
    {reply, ok, State#state{backup_pid=Pid, backup_start=Now}};
handle_call({start_backup, _IsFullBackup}, _From, #state{ backup_pid = _Pid, is_env_backup = false } = State) ->
    {reply, {error, in_progress}, State};
handle_call({start_backup, _IsFullBackup}, _From, #state{ is_env_backup = true } = State) ->
    {reply, {error, env_backup}, State};

%% @doc Return the start datetime of the current running backup, if any.
handle_call(in_progress_start, _From, #state{ backup_start = BackupStart } = State) ->
    {reply, BackupStart, State};

%% @doc Check if there is an upload process running.
handle_call(is_uploading, _From, #state{ upload_pid = Pid } = State) ->
    {reply, is_pid(Pid), State};

%% @doc Check if there is a download process running.
handle_call(is_downloading, _From, #state{ download_pid = Pid } = State) ->
    {reply, is_pid(Pid), State};

%% @doc Trap unknown calls
handle_call(Message, _From, #state{} = State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(Message, #state{} = State) ->
    {stop, {unknown_cast, Message}, State}.


handle_info(periodic_backup, #state{ backup_pid = Pid } = State) when is_pid(Pid) ->
    {noreply, State};
handle_info(periodic_backup, #state{ upload_pid = Pid } = State) when is_pid(Pid) ->
    {noreply, State};
handle_info(periodic_backup, #state{ is_env_backup = false } = State) ->
    State1 = case backup_config:daily_dump(State#state.context) of
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

handle_info(periodic_download, #state{ backup_pid = Pid } = State) when is_pid(Pid) ->
    {noreply, State};
handle_info(periodic_download, #state{ download_pid = Pid } = State) when is_pid(Pid) ->
    {noreply, State};
handle_info(periodic_download, #state{ is_env_backup = true } = State) ->
    State1 = maybe_filestore_download(State),
    {noreply, State1};

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

handle_info({'EXIT', Pid, normal}, #state{ download_pid = Pid } = State) ->
    %% TODO: check if new database dump has been downloaded, if so, then
    %% import this as in our schema.
    State1 = State#state{
        download_pid = undefined,
        download_start = undefined
    },
    {noreply, State1};

handle_info({'EXIT', Pid, Reason}, #state{ download_pid = Pid } = State) ->
    ?LOG_ERROR(#{
        text => <<"Backup downloader crashed">>,
        in => zotonic_mod_backup,
        result => error,
        reason => Reason,
        pid => Pid
    }),
    State1 = State#state{
        download_pid = undefined,
        download_start = undefined
    },
    {noreply, State1};

handle_info(Info, #state{} = State) ->
    ?DEBUG(Info),
    {noreply, State}.

terminate(_Reason, State) ->
    timer:cancel(State#state.timer_ref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

ensure_job_queue(Name, Options) ->
    jobs:run(zotonic_singular_job, fun() ->
        case jobs:queue_info(Name) of
            undefined -> jobs:add_queue(Name, Options);
            {queue, _Props} -> ok
        end
    end).

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

maybe_filestore_upload(#state{ context = Context, upload_pid = undefined } = State) ->
    case backup_config:is_filestore_enabled(Context) of
        true ->
            % Check the backup.json if any files are not yet uploaded
            Data = backup_create:read_admin_file(Context),
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
    end;
maybe_filestore_upload(State) ->
    ?LOG_INFO(#{
        in => zotonic_mod_backup,
        text => <<"Backup delaying upload of database file to filestore because another upload is busy.">>,
        reason => busy,
        upload_pid => State#state.upload_pid
    }),
    State.

do_upload(Name, DatabaseFile, Context) ->
    z_proc:spawn_link_md(
        fun() ->
            RemoteDbFile = <<"backup/", DatabaseFile/binary>>,
            LocalDbFile = filename:join(backup_create:dir(Context), DatabaseFile),
            case z_notifier:first(
                #filestore_request{
                    action = upload,
                    remote = RemoteDbFile,
                    local = LocalDbFile
                }, Context)
            of
                ok ->
                    F = fun(Data) ->
                        Bck = maps:get(Name, Data),
                        Data#{
                            Name => Bck#{
                                <<"is_filestore_uploaded">> => true
                            }
                        }
                    end,
                    backup_create:update_admin_file(F, Context),
                    RemoteAdminFile = <<"backup/backup.json">>,
                    LocalAdminFile = filename:join(backup_create:dir(Context), <<"backup.json">>),
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
                    ?LOG_INFO(#{
                        text => <<"Backup not uploading database file to filestore">>,
                        in => zotonic_mod_backup,
                        result => error,
                        reason => no_filestore
                    })
            end
        end).

maybe_filestore_download(#state{ context = Context } = State) ->
    case backup_config:is_filestore_enabled(Context) of
        true ->
            % Download the new backup.json and new files from the filestore
            Pid = do_download(Context),
            State#state{
                download_start = calendar:universal_time(),
                download_pid = Pid
            };
        false ->
            State
    end.


do_download(Context) ->
    z_proc:spawn_link_md(
        fun() ->
            RemoteAdminFile = <<"backup/backup.json">>,
            LocalAdminFile = filename:join(backup_create:dir(Context), "backup.json"),
            LocalAdminFileTmp = filename:join(backup_create:dir(Context), "backup.json.tmp"),
            case z_notifier:first(
                #filestore_request{
                    action = download,
                    remote = RemoteAdminFile,
                    local = LocalAdminFileTmp
                }, Context)
            of
                ok ->
                    case backup_create:read_json_file(LocalAdminFileTmp) of
                        {ok, NewData} ->
                            CurrentData = backup_create:read_admin_file(Context),
                            maps:foreach(
                                fun(Name, NewStatus) ->
                                    IsChanged = case maps:get(Name, CurrentData, undefined) of
                                        undefined ->
                                            true;
                                        CurrentStatus when NewStatus =:= CurrentStatus ->
                                            % Not changed - try re-downloading missing files
                                            backup_restore:is_file_missing(maps:get(<<"database">>, NewStatus, undefined), Context)
                                            orelse backup_restore:is_file_missing(maps:get(<<"config_files">>, NewStatus, undefined), Context);
                                        _ ->
                                            % New - download all files
                                            true
                                    end,
                                    if
                                        IsChanged ->
                                            try_download_file(
                                                maps:get(<<"database">>, NewStatus, undefined),
                                                maps:get(<<"database_hash">>, NewStatus, undefined),
                                                Context),
                                            try_download_file(
                                                maps:get(<<"config_files">>, NewStatus, undefined),
                                                maps:get(<<"config_files_hash">>, NewStatus, undefined),
                                                Context);
                                        true ->
                                            ok
                                    end
                                end,
                                NewData),
                            jobs:run(?CONFIG_UPDATE_JOB,
                                fun() ->
                                    _ = file:delete(LocalAdminFile),
                                    ok = file:rename(LocalAdminFileTmp, LocalAdminFile)
                                end),
                            z_proc:spawn_md(
                                fun() ->
                                    z_sites_config:maybe_set_backup_env(Context),
                                    Options = [ database, files, security, config ],
                                    backup_restore:restore_newest_backup(Options, Context)
                                end),
                            ok;
                        {error, Reason} = Error ->
                            ?LOG_ERROR(#{
                                text => <<"Backup error reading downloaded backup.json temp file">>,
                                in => zotonic_mod_backup,
                                result => error,
                                reason => Reason,
                                remote => RemoteAdminFile,
                                local => LocalAdminFileTmp
                            }),
                            Error
                    end;
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        text => <<"Backup error downloading backup.json file from filestore">>,
                        in => zotonic_mod_backup,
                        result => error,
                        reason => Reason,
                        remote => RemoteAdminFile,
                        local => LocalAdminFileTmp
                    }),
                    file:delete(LocalAdminFileTmp),
                    Error;
                undefined ->
                    ok
            end
        end).

%% @doc If a filename is given, then download that file from the filestore.
try_download_file(undefined, _Hash, _Context) ->
    ok;
try_download_file(<<>>, _Hash, _Context) ->
    ok;
try_download_file(Filename, Hash, Context) ->
    RemoteFile = <<"backup/", Filename/binary>>,
    LocalFileTmp = filename:join(backup_create:dir(Context), <<Filename/binary, ".tmp">>),
    LocalFile = filename:join(backup_create:dir(Context), Filename),
    case z_notifier:first(
        #filestore_request{
            action = download,
            remote = RemoteFile,
            local = LocalFileTmp
        }, Context)
    of
        ok ->
            case z_crypto:hex_sha2_file(LocalFileTmp) of
                {ok, TmpHash} when Hash =:= TmpHash; Hash =:= undefined ->
                    ok = file:rename(LocalFileTmp, LocalFile),
                    ?LOG_INFO(#{
                        text => <<"Backup downloaded file from filestore">>,
                        in => zotonic_mod_backup,
                        result => ok,
                        remote => RemoteFile,
                        local => LocalFile
                    }),
                    ok;
                {ok, TmpHash} ->
                    ?LOG_ERROR(#{
                        text => <<"Backup error, hash mismatch for downloaded file">>,
                        in => zotonic_mod_backup,
                        result => error,
                        reason => hash,
                        remote => RemoteFile,
                        local => LocalFileTmp,
                        remote_hash => Hash,
                        local_hash => TmpHash
                    }),
                    file:delete(LocalFileTmp),
                    {error, hash};
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        text => <<"Backup error calculating hash for downloaed file">>,
                        in => zotonic_mod_backup,
                        result => error,
                        reason => Reason,
                        remote => RemoteFile,
                        local => LocalFileTmp
                    }),
                    file:delete(LocalFileTmp),
                    Error
            end;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                text => <<"Backup error downloading file from filestore">>,
                in => zotonic_mod_backup,
                result => error,
                reason => Reason,
                remote => RemoteFile,
                local => LocalFileTmp
            }),
            file:delete(LocalFileTmp),
            Error
    end.


%% @doc Start a backup and return the pid of the backup process, whilst linking to the process.
do_backup(DT, Name, IsFullBackup, Context) ->
    z_mqtt:publish(<<"model/backup/event/backup">>, #{ status => <<"started">> }, Context),
    z_proc:spawn_link_md(
        fun() ->
            jobs:run(
                ?MODULE,
                fun() ->
                    backup_create:make_backup(DT, Name, IsFullBackup, Context)
                end)
        end).


%% @doc List all backups in the backup directory.
list_backup_files(Context) ->
    Data = backup_create:read_admin_file(Context),
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

%%
%% Helpers
%%

%% Strip all extensions from a filename.
without_extension(Filename) ->
    case filename:rootname(Filename) of
        Filename -> Filename;
        Root -> without_extension(Root)
    end.

%% @doc Return the base name of the backup files. We have 6 daily backups and 4
%% weekly backups. The daily backups have the daynumber in them, the weekly backups
%% use a week number (modulo 4) and are made on Sundays.
name(Context) ->
    {Date, _} = calendar:universal_time(),
    case calendar:day_of_the_week(Date) of
        7 ->
            GregorianDays = calendar:date_to_gregorian_days(Date),
            WeekNr = (GregorianDays div 7) rem ?WEEKLY_BACKUPS,
            iolist_to_binary([
                atom_to_list(z_context:site(Context)),
                "-w",
                integer_to_binary(WeekNr)
            ]);
        Day ->
            iolist_to_binary([
                atom_to_list(z_context:site(Context)),
                "-",
                integer_to_binary(Day)
            ])
    end.

