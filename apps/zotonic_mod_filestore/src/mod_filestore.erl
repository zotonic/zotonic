%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2024 Marc Worrell
%% @doc Module managing the storage of files on remote servers.
%% @end

%% Copyright 2014-2024 Marc Worrell
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

-module(mod_filestore).

-author("Marc Worrell <marc@worrell.nl>").
-mod_title("File Storage").
-mod_description("Store files on cloud storage services using FTP, S3 and WebDAV").
-mod_prio(500).
-mod_schema(12).
-mod_provides([filestore]).
-mod_depends([cron]).

-behaviour(supervisor).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

-define(BATCH_SIZE, 200).
-define(MAX_FILENAME_LENGTH, 64).

-export([
    observe_filestore/2,
    observe_filestore_request/2,
    observe_media_update_done/2,
    observe_filestore_credentials_lookup/2,
    observe_filestore_credentials_revlookup/2,
    observe_admin_menu/3,

    pid_observe_tick_1m/3,

    queue_all/1,
    queue_all_stop/1,

    task_queue_all/3,

    lookup/2,

    delete_ready/5,
    download_stream/5,
    manage_schema/2
    ]).

-export([
    start_link/1,
    init/1
    ]).

-export([
    shorten_filename/1
    ]).

observe_media_update_done(#media_update_done{action=insert, post_props=Props}, Context) ->
    queue_medium(Props, Context);
observe_media_update_done(#media_update_done{}, _Context) ->
    ok.

observe_filestore(#filestore{action=lookup, path=Path}, Context) ->
    lookup(Path, Context);
observe_filestore(#filestore{action=upload, path=Path, mime=undefined} = Upload, Context) ->
    Mime = z_media_identify:guess_mime(Path),
    observe_filestore(Upload#filestore{mime=Mime}, Context);
observe_filestore(#filestore{action=upload, path=Path, mime=Mime}, Context) ->
    MediaProps = #{
        <<"mime">> => Mime
    },
    maybe_queue_file(<<>>, Path, true, MediaProps, Context),
    ok;
observe_filestore(#filestore{action=delete, path=Path}, Context) ->
    case m_filestore:mark_deleted(Path, Context) of
        ok ->
            ?LOG_INFO(#{
                text => <<"Filestore marked entries as deleted.">>,
                in => zotonic_mod_filestore,
                path => Path
            }),
            ok;
        {error, enoent} ->
            ok
    end.

observe_filestore_request(#filestore_request{
            action = upload,
            remote = RemoteFile,
            local = LocalFile,
            mime = Mime
    }, Context) ->
    filestore_request:upload(LocalFile, RemoteFile, Mime, Context);
observe_filestore_request(#filestore_request{
            action = download,
            remote = RemoteFile,
            local = LocalFile
    }, Context) ->
    filestore_request:download(LocalFile, RemoteFile, Context);
observe_filestore_request(#filestore_request{
            action = delete,
            remote = RemoteFile
    }, Context) ->
    filestore_request:delete(RemoteFile, Context).


%% @doc Map the local path to the URL of the remotely stored file. This depends on the
%% service configured in the filestore config.
observe_filestore_credentials_lookup(#filestore_credentials_lookup{path=Path}, Context) ->
    Service = m_config:get_value(?MODULE, service, <<"s3">>, Context),
    S3Key = m_config:get_value(?MODULE, s3key, Context),
    S3Secret = m_config:get_value(?MODULE, s3secret, Context),
    S3Url = m_config:get_value(?MODULE, s3url, Context),
    case is_defined(S3Key) andalso is_defined(S3Secret) andalso is_defined(S3Url) of
        true ->
            Url = make_url(S3Url, Path),
            {ok, #filestore_credentials{
                    service = Service,
                    location = Url,
                    credentials = {S3Key,S3Secret}
            }};
        false ->
            undefined
    end.

%% @doc Given the service, find the credentials to do a lookup of the remote file.
observe_filestore_credentials_revlookup(#filestore_credentials_revlookup{service=Service, location=Location}, Context) ->
    ConfiguredService = m_config:get_value(?MODULE, service, <<"s3">>, Context),
    if
        Service =:= ConfiguredService ->
            S3Key = m_config:get_value(?MODULE, s3key, Context),
            S3Secret = m_config:get_value(?MODULE, s3secret, Context),
            case is_defined(S3Key) andalso is_defined(S3Secret) of
                true ->
                    {ok, #filestore_credentials{
                            service = Service,
                            location = Location,
                            credentials = {S3Key,S3Secret}
                    }};
                false ->
                    undefined
            end;
        true ->
            undefined
    end.

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_filestore,
                parent=admin_system,
                label=?__("Cloud File Store", Context),
                url={admin_filestore},
                visiblecheck={acl, use, mod_config}}

     |Acc].


make_url(S3Url, Path) ->
    make_url_1(S3Url, z_url:url_path_encode(shorten_filename(Path))).

make_url_1(S3Url, <<$/, _/binary>> = Path) ->
    <<S3Url/binary, Path/binary>>;
make_url_1(S3Url, Path) ->
    <<S3Url/binary, $/, Path/binary>>.


%% @doc Not all remote services allow the long filenames generated by
%% filters and user generated filenames. Shorten those path by truncating
%% the path's basename and adding a hash of the rootname. Also replace all
%% non "simple" ascii characters with a "-", this because some S3 compatible
%% services have a problem with characters like () and *.
-spec shorten_filename(Path) -> ShortPath when
    Path :: binary(),
    ShortPath :: binary().
shorten_filename(Path) ->
    Basename = filename:basename(Path),
    CleanedBasename = replace_special_chars(Basename, <<>>),
    Basename1 = shorten(CleanedBasename, Basename),
    case filename:dirname(Path) of
        <<".">> -> Basename1;
        Dir -> filename:join(Dir, Basename1)
    end.

shorten(Name, OrgName) when size(Name) > ?MAX_FILENAME_LENGTH; OrgName =/= Name ->
    Root = filename:rootname(Name),
    Ext = filename:extension(Name),
    case size(Ext) < 10 of
        true ->
            Short = shorten_1(Root, OrgName),
            <<Short/binary, Ext/binary>>;
        false ->
            shorten_1(Name, OrgName)
    end;
shorten(_Name, OrgName) ->
    OrgName.

shorten_1(Root, OrgName) ->
    Truncated = z_string:truncatechars(Root, 32),
    Hash = z_utils:hex_sha(OrgName),
    <<Truncated/binary, $-, Hash/binary>>.

replace_special_chars(<<>>, Acc) ->
    Acc;
replace_special_chars(<<C/utf8, Rest/binary>>, Acc) when C >= $a, C =< $z ->
    replace_special_chars(Rest, <<Acc/binary, C/utf8>>);
replace_special_chars(<<C/utf8, Rest/binary>>, Acc) when C >= $A, C =< $Z ->
    replace_special_chars(Rest, <<Acc/binary, C/utf8>>);
replace_special_chars(<<C/utf8, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    replace_special_chars(Rest, <<Acc/binary, C/utf8>>);
replace_special_chars(<<C/utf8, Rest/binary>>, Acc) when
    C =:= $_; C =:= $-; C =:= $.  ->
    replace_special_chars(Rest, <<Acc/binary, C/utf8>>);
replace_special_chars(<<_/utf8, Rest/binary>>, Acc) ->
    replace_special_chars(Rest, <<Acc/binary, $->>).


is_defined(undefined) -> false;
is_defined(<<>>) -> false;
is_defined(_) -> true.

pid_observe_tick_1m(Pid, tick_1m, Context) ->
    case m_config:get_boolean(?MODULE, is_upload_enabled, Context) of
        true ->
            start_uploaders(Pid, m_filestore:fetch_queue(Context), Context);
        false ->
            nop
    end,
    start_downloaders(m_filestore:fetch_move_to_local(Context), Context),
    case m_config:get_value(?MODULE, delete_interval, <<"0">>, Context) of
        false ->
            nop;
        <<"false">> ->
            nop;
        undefined ->
            %% For backwards compat, if the config option was not set.
            start_deleters(m_filestore:fetch_deleted(<<"0">>, Context), Context);
        Interval ->
            start_deleters(m_filestore:fetch_deleted(Interval, Context), Context)
    end.


manage_schema(What, Context) ->
    m_filestore:install(What, Context).

lookup(Path, Context) ->
    case m_filestore:lookup(Path, Context) of
        {ok, #{ location := Location } = StoreEntry} ->
            case filezcache:locate_monitor(Location) of
                {ok, {file, _Size, Filename}} ->
                    {ok, {filename, Filename, StoreEntry}};
                {ok, {pid, Pid}} ->
                    {ok, {filezcache, Pid, StoreEntry}};
                {error, enoent} ->
                    load_cache(StoreEntry, Context)
            end;
        {error, _} ->
            undefined
    end.

load_cache(#{
            service := Service,
            location := Location,
            size := Size,
            id := Id
        } = StoreEntry, Context) ->
    case z_notifier:first(
        #filestore_credentials_revlookup{ service=Service, location=Location },
        Context)
    of
        {ok, #filestore_credentials{ service=CredService, location=Location1, credentials=Cred }} when
            CredService =:= <<"s3">>;
            CredService =:= <<"webdav">>;
            CredService =:= <<"ftp">> ->
            ?LOG_DEBUG(#{
                text => <<"File store cache load">>,
                in => zotonic_mod_filestore,
                location => Location
            }),
            Ctx = z_context:prune_for_async(Context),
            StreamFun = fun(CachePid) ->
                Mod = filestore_request:filezmod(CredService),
                Mod:stream(
                    Cred,
                    Location1,
                    fun
                        ({error, FinalError}) when FinalError =:= enoent; FinalError =:= forbidden ->
                            ?LOG_ERROR(#{
                                text => <<"File store remote file has problems.">>,
                                in => zotonic_mod_filestore,
                                result => error,
                                reason => FinalError,
                                service => CredService,
                                remote => Location,
                                id => Id
                            }),
                            ok = m_filestore:mark_error(Id, FinalError, Ctx),
                            exit(normal);
                        ({error, Reason} = Error) ->
                            % Abnormal exit when receiving an error.
                            % This takes down the cache entry.
                            ?LOG_ERROR(#{
                                text => <<"File store error on cache load.">>,
                                in => zotonic_mod_filestore,
                                result => error,
                                reason => Reason,
                                service => CredService,
                                remote => Location,
                                id => Id
                            }),
                            exit(Error);
                        (stream_start) ->
                            nop;
                        (T) when is_tuple(T) ->
                            nop;
                        (B) when is_binary(B) ->
                            filezcache:append_stream(CachePid, B);
                         (eof) ->
                            filezcache:finish_stream(CachePid)
                    end)
            end,
            case filezcache:insert_stream(Location, Size, StreamFun, [monitor]) of
                {ok, Pid} ->
                    {ok, {filezcache, Pid, StoreEntry}};
                {error, {already_started, Pid}} ->
                    {ok, {filezcache, Pid, StoreEntry}}
            end;
        undefined ->
            undefined
    end.

queue_all(Context) ->
    Max = z_db:q1("select count(*) from medium", Context),
    z_pivot_rsc:insert_task(?MODULE, task_queue_all, filestore_queue_all, [0, Max], Context).

queue_all_stop(Context) ->
    z_pivot_rsc:delete_task(?MODULE, task_queue_all, filestore_queue_all, Context).

task_queue_all(Offset, Max, Context) when Offset =< Max ->
    case z_db:qmap_props("
        select *
        from medium
        order by id asc
        limit $1
        offset $2",
        [ ?BATCH_SIZE, Offset ],
        [ {keys, binary} ],
        Context)
    of
        {ok, Media} ->
            ?LOG_INFO(#{
                text => <<"Ensuring files are queued for remote upload.">>,
                in => zotonic_mod_filestore,
                count => length(Media)
            }),
            lists:foreach(fun(M) ->
                            queue_medium(M, Context)
                          end,
                          Media),
            {delay, 0, [Offset+?BATCH_SIZE, Max]};
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => <<"Error queueing files for remote upload.">>,
                in => zotonic_mod_filestore,
                result => error,
                reason => Reason
            }),
            {delay, 60, [Offset, Max]}
    end;
task_queue_all(_Offset, _Max, _Context) ->
    ok.


%% @doc Queue the medium entry and its preview for upload.
-spec queue_medium( z_media_identify:media_info(), z:context() ) -> ok | nop.
queue_medium(Medium, Context) ->
    Filename = maps:get(<<"filename">>, Medium, undefined),
    IsDeletable = maps:get(<<"is_deletable_file">>, Medium, undefined),
    Preview = maps:get(<<"preview_filename">>, Medium, undefined),
    PreviewDeletable = maps:get(<<"is_deletable_preview">>, Medium, undefined),
    Medium1 = maps:remove(<<"exif">>, Medium),
    % Queue the main medium record for upload.
    maybe_queue_file(<<"archive/">>, Filename, IsDeletable, Medium1, Context),
    % Queue the (optional) preview file.
    MediumPreview = #{
        <<"id">> => maps:get(<<"id">>, Medium, undefined)
    },
    maybe_queue_file(<<"archive/">>, Preview, PreviewDeletable, MediumPreview, Context).


maybe_queue_file(_Prefix, undefined, _IsStaticFile, _MediaInfo, _Context) ->
    nop;
maybe_queue_file(_Prefix, <<>>, _IsStaticFile, _MediaInfo, _Context) ->
    nop;
maybe_queue_file(_Prefix, _Path, false, _MediaInfo, _Context) ->
    nop;
maybe_queue_file(Prefix, Filename, true, MediaInfo, Context) ->
    FilenameBin = z_convert:to_binary(Filename),
    case m_filestore:queue(<<Prefix/binary, FilenameBin/binary>>, MediaInfo, Context) of
        ok -> ok;
        {error, duplicate} -> ok
    end.



%%% ------------------------------------------------------------------------------------
%%% Supervisor callbacks
%%% ------------------------------------------------------------------------------------

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init(_Args) ->
    {ok,{
        #{
            strategy => simple_one_for_one,
            intensity => 20,
            period => 10
        },
        [
            #{
                id => undefined,
                start => {filestore_uploader, start_link, []},
                restart => transient,
                shutdown => brutal_kill,
                type => worker,
                modules => [filestore_uploader]
            }
        ]}}.


%%% ------------------------------------------------------------------------------------
%%% Support routines
%%% ------------------------------------------------------------------------------------

-spec start_uploaders(pid(), {ok, [ m_filestore:queue_entry() ]} | {error, term()}, z:context()) -> ok.
start_uploaders(Pid, {ok, Rs}, Context) ->
    lists:foreach(
        fun(QueueEntry) ->
            start_uploader(Pid, QueueEntry, Context)
        end,
        Rs);
start_uploaders(_Pid, {error, _}, _Context) ->
    % Ignore error, will be retried later
    ok.

start_uploader(Pid, #{ id := Id, path := Path, props := MediumInfo }, Context) ->
    Path1 = z_convert:to_binary(Path),
    PathLookup = m_filestore:lookup(Path1, Context),
    supervisor:start_child(Pid, [Id, Path1, PathLookup, MediumInfo, Context]).

-spec start_deleters( {ok, [ m_filestore:filestore_entry() ]} | {error, term()}, z:context() ) -> ok.
start_deleters({ok, Rs}, Context) ->
    lists:foreach(
        fun(FilestoreEntry) ->
            start_deleter(FilestoreEntry, Context)
        end,
        Rs);
start_deleters({error, _}, _Context) ->
    % Ignore errors, will be fixed on a later retry
    ok.

start_deleter(#{
            id := Id,
            path := Path,
            service := Service,
            location := Location
        }, Context) ->
    case z_notifier:first(#filestore_credentials_revlookup{service=Service, location=Location}, Context) of
        {ok, #filestore_credentials{service=CredService, location=Location1, credentials=Cred}}
            when CredService =:= <<"s3">>;
                 CredService =:= <<"webdav">>;
                 CredService =:= <<"ftp">> ->
            ?LOG_DEBUG(#{
                text => <<"Queue delete.">>,
                in => zotonic_mod_filestore,
                path => Path,
                service => CredService,
                location => Location1,
                id => Id
            }),
            ContextAsync = z_context:prune_for_async(Context),
            Mod = filestore_request:filezmod(CredService),
            _ = Mod:queue_delete_id({?MODULE, delete, Id}, Cred, Location1, {?MODULE, delete_ready, [Id, Path, ContextAsync]});
        {ok, _} ->
            ?LOG_DEBUG(#{
                text => <<"No credentials for queue delete -- service mismatch">>,
                in => zotonic_mod_filestore,
                service => Service,
                location => Location,
                path => Path,
                id => Id
            });
        undefined ->
            ?LOG_DEBUG(#{
                text => <<"No credentials for queue delete.">>,
                in => zotonic_mod_filestore,
                service => Service,
                location => Location,
                path => Path,
                id => Id
            })
    end.

delete_ready(Id, Path, Context, _Ref, ok) ->
    ?LOG_DEBUG(#{
        text => <<"Delete remote file done.">>,
        in => zotonic_mod_filestore,
        result => ok,
        path => Path
    }),
    m_filestore:purge_deleted(Id, Context);
delete_ready(Id, Path, Context, _Ref, {error, Reason})
    when Reason =:= enoent; Reason =:= epath ->
    ?LOG_DEBUG(#{
        text => <<"Delete remote file was not found">>,
        in => zotonic_mod_filestore,
        result => error,
        reason => Reason,
        path => Path
    }),
    m_filestore:purge_deleted(Id, Context);
delete_ready(Id, Path, Context, _Ref, {error, forbidden}) ->
    ?LOG_INFO(#{
        text => <<"Delete remote file was forbidden">>,
        in => zotonic_mod_filestore,
        path => Path,
        result => error,
        reason => forbidden,
        id => Id
    }),
    m_filestore:purge_deleted(Id, Context);
delete_ready(Id, Path, _Context, _Ref, {error, Reason}) ->
    ?LOG_ERROR(#{
        text => <<"Delete remote file failed,">>,
        in => zotonic_mod_filestore,
        path => Path,
        result => error,
        reason => Reason,
        id => Id
    }).


-spec start_downloaders( {ok, [ m_filestore:filestore_entry() ]} | {error, term()}, z:context() ) -> ok.
start_downloaders({ok, Rs}, Context) ->
    lists:foreach(
        fun(FilestoreEntry) ->
            start_downloader(FilestoreEntry, Context)
        end,
        Rs);
start_downloaders({error, _}, _Context) ->
    ok.

start_downloader(#{
            id := Id,
            path := Path,
            service := Service,
            location := Location
        }, Context) ->
    case z_notifier:first(#filestore_credentials_revlookup{service=Service, location=Location}, Context) of
        {ok, #filestore_credentials{service=CredService, location=Location1, credentials=Cred}}
            when CredService =:= <<"s3">>;
                 CredService =:= <<"webdav">>;
                 CredService =:= <<"ftp">> ->
            LocalPath = z_path:files_subdir(Path, Context),
            ok = z_filelib:ensure_dir(LocalPath),
            ?LOG_DEBUG(#{
                text => <<"Queue moved to local.">>,
                in => zotonic_mod_filestore,
                service => CredService,
                location => Location1,
                path => Path,
                local_path => LocalPath,
                id => Id
            }),
            case filelib:is_file(LocalPath) of
                true ->
                    % File is present - no download needed;
                    ?LOG_DEBUG(#{
                        text => <<"Download remote file skipped, file already downloaded">>,
                        result => ok,
                        in => zotonic_mod_filestore,
                        local => LocalPath
                    }),
                    download_done(Id, Path, Context);
                false ->
                    ContextAsync = z_context:prune_for_async(Context),
                    Mod = filestore_request:filezmod(CredService),
                    _ = Mod:queue_stream_id({?MODULE, stream, Id}, Cred, Location1, {?MODULE, download_stream, [Id, Path, LocalPath, ContextAsync]})
            end;
        undefined ->
            ?LOG_DEBUG(#{
                text => <<"No credentials for downloader.">>,
                in => zotonic_mod_filestore,
                service => Service,
                location => Location,
                path => Path,
                id => Id
            })
    end.

download_stream(_Id, _Path, LocalPath, _Context, stream_start) ->
    ?LOG_DEBUG(#{
        text => <<"Download remote file stream started">>,
        result => ok,
        in => zotonic_mod_filestore,
        local => LocalPath
    }),
    file:delete(temp_path(LocalPath));
download_stream(_Id, _Path, LocalPath, _Context, {content_type, _}) ->
    ?LOG_DEBUG(#{
        text => <<"Download remote file stream started">>,
        result => ok,
        in => zotonic_mod_filestore,
        local => LocalPath
    }),
    file:delete(temp_path(LocalPath));
download_stream(_Id, _Path, LocalPath, _Context, Data) when is_binary(Data) ->
    file:write_file(temp_path(LocalPath), Data, [append,raw,binary]);
download_stream(Id, Path, LocalPath, Context, eof) ->
    ?LOG_DEBUG(#{
        text => <<"Download remote file stream ended">>,
        result => ok,
        in => zotonic_mod_filestore,
        local => LocalPath
    }),
    ok = file:rename(temp_path(LocalPath), LocalPath),
    download_done(Id, Path, Context);
download_stream(Id, Path, LocalPath, Context, {error, Reason}) ->
    ?LOG_WARNING(#{
        text => <<"Download error on file stream">>,
        in => zotonic_mod_filestore,
        result => error,
        reason => Reason,
        local => LocalPath,
        id => Id,
        path => Path
    }),
    file:delete(temp_path(LocalPath)),
    m_filestore:unmark_move_to_local(Id, Context);
download_stream(_Id, _Path, _LocalPath, _Context, _Other) ->
    ok.

download_done(Id, Path, Context) ->
    m_filestore:purge_move_to_local(Id, m_filestore:is_local_keep(Context), Context),
    filezcache:delete({z_context:site(Context), Path}),
    filestore_uploader:stale_file_entry(Path, Context).

temp_path(F) when is_list(F) ->
    F ++ ".downloading";
temp_path(F) when is_binary(F) ->
    <<F/binary, ".downloading">>.
