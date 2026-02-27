%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2026 Marc Worrell
%% @doc Module managing the storage of files on remote servers.
%% @end

%% Copyright 2014-2026 Marc Worrell
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
-moduledoc("
Support for storing uploaded and generated images and documents on external services.



Overview
--------

This module stores uploaded files and generated preview-images on an external S3-compatible service. It listens for
medium and file related notifications for any newly uploaded or generated files.

If a file is added then the file is queued in an upload queue. After a delay a separate process polls this queue and
will upload the file to the external service.

If a file is needed and not locally available then the mod_filestore module will check its file registry to see if the
file is stored on an external service. If so then then a *filezcache* process is added and a download of the file is started.

The file is served from the filezcache whilst it is being downloaded.

The filezcache will stop the entry after a random amount of time—if the entry was not recently used.



Configuration
-------------



### S3 configuration

Configure the following permissions on your S3 service for mod_filestore to work correctly:

| Resource                         | Permissions                                                                      |
| -------------------------------- | -------------------------------------------------------------------------------- |
| `/`                              | *   s3:ListBucket                                                                |
| `/-zotonic-filestore-test-file-` | *   s3:GetObject *   s3:PutObject *   s3:DeleteObject                            |
| `/preview/*`                     | *   s3:GetObject *   s3:PutObject *   s3:DeleteObject (if file deletion is enabled) |
| `/archive/*`                     | *   s3:GetObject *   s3:PutObject *   s3:DeleteObject (if file deletion is enabled) |



### mod_filestore configuration

After the mod_filestore is enabled an extra menu entry ‘Cloud File Store’ is added to the ‘System’ menu in the admin.

Selecting the menu will show the configuration panel for the Could File Store.



Here you can define where the files should be stored and give the credentials to access the storage.

If you save the url and credentials then the system will try to upload a small file to the remote storage. If it
succeeds then the configuration is saved. If it does not succeed then an error message will be displayed and the
configuration will not be changed.

It is possible to (temporarily) disable uploading new files by unchecking the checkbox *Upload new files to the cloud*.



### File deletion

You can also configure file deletion behaviour, i.e. what should happen when a file is removed from Zotonic. You can
choose to immediately remove the file from the filestore, not delete it at all (to make your store immutable) or delete
the file after a certain delay (to be able to restore accidentally deleted files).



### Statistics

The system shows statistics:

Media

All medium records and a sum of the sizes. A single medium record can have 0, 1 or 2 files attached.

Local Files

These are all files found in the `files` directory, this includes files that won’t ever be uploaded.

Cloud Files

All files registered to be on any cloud service. This is extracted from the database and not by scanning the remote
cloud service.

Queues

These are the queues being processed by mod_filestore. On a quiet (stable) system they are usually empty.



### Moving files

It is possible to move (almost) all files from the local file system to the cloud. And vice versa, from the cloud to the
local file system. This is useful when starting or changing the cloud storage location.

If a file is moved to the cloud then it is first placed in the filezcache. The filezcache will start purging the files
if the cache is bigger than configurated in the filezcache application (default 10GB for all sites combined).

The system waits 10 minutes before a queued file is uploaded. This period is meant for a *cool down* of the file, as in
the first moments after an upload some resize and preview operations will take place. The delay makes it less probable
that a freshly uploaded file vanishes (to the cache) whilst a preview-generation is starting.



Notifications
-------------

The mod_filestore hooks into the following notifications, whose definitions can be found in `zotonic_file.hrl`:

`#filestore{}`

Hooks into the Zotonic file management notifications to upload, delete or lookup files. This will trigger downloads of
external files and interfaces to the filezcache.

`#filestore_credentials_lookup{}`

Maps a local path and optional resource id to a service, external location and key/password for that external service.
This can be used to store different resources on different external services.

`#filestore_credentials_revlookup{}`

Maps a cloud file service and location to a key, password and request location.

`#medium_update_done{}`

Queues newly inserted medium files into the upload queue.

`#admin_menu{}`

To add the Cloud File Store menu to the admin.



Applications
------------

The filestore uses the s3filez and filezcache Erlang applications.



### s3filez

This application is used for uploading, downloading and deleting files on S3 compatible services. It provides
asynchronous services and is compatible with the filezcache application. It is also able to stream files to and from the
external S3 service, this makes it possible to have start serving a file before it is downloaded to the filezcache.



### filezcache

This application manages a cache of downloaded files. The cache is shared between all sites. Every cache entry is
managed by its own process, which can stream newly received data directly to any requesting processes.

The filezcache keeps a presistent *disk_log* with a description of all files in the cache. This log is read on startup
to repopulate the cache with already present files. For each file the size and a hash is stored to check cache consistency.

The filezcache has a garbage collector. It keeps a pool of randomly selected cache entries, from which it will elect
randomly processes to be garbage-collected. The processes themselves will decide if they will stop or not.

After a cache process stops it will keep running for a short period to handle late incoming requests.

Filezcache entries are started by the mod_filestore and filled by either moving a local file to the cache or by
s3filez download processes.
External file storage module for media/file offloading and synchronization.

The statistics are generated dynamically, which is not a good idea with many files. This will be changed.

Accepted Events
---------------

This module handles the following notifier callbacks:

- `observe_admin_menu`: Handle `admin_menu` notifications using `z_url:url_path_encode`.
- `observe_filestore`: Handle `filestore` notifications using `z_media_identify:guess_mime`.
- `observe_filestore_credentials_lookup`: Map the local path to the URL of the remotely stored file using `filestore_config:service`.
- `observe_filestore_credentials_revlookup`: Given the service, find the credentials to do a lookup of the remote file using `filestore_config:service`.
- `observe_filestore_request`: Handle `filestore_request` notifications using `filestore_request:upload`.
- `observe_media_update_done`: Handle `media_update_done` notifications using `z_media_identify:guess_mime`.

See also

[m_filestore](/id/doc_model_model_filestore).").

-author("Marc Worrell <marc@worrell.nl>").
-mod_title("File Storage").
-mod_description("Store files on cloud storage services using FTP, S3 and WebDAV").
-mod_prio(500).
-mod_schema(12).
-mod_provides([filestore]).
-mod_depends([cron]).
-mod_config([
        #{
            key => service,
            type => string,
            default => "",
            description => "The service to use for storing files. One of: s3, ftp, ftps, webdav, webdavs"
        },
        #{
            key => s3url,
            type => string,
            default => "",
            description => "The URL of the S3 service, e.g. https://s3.myblockstorage.com"
        },
        #{
            key => s3key,
            type => string,
            default => "",
            description => "The S3 access key or FTP/WebDAV username, used for authentication."
        },
        #{
            key => s3secret,
            type => string,
            default => "",
            description => "The S3 secret key or FTP/WebDAV password, used for authentication"
        },
        #{
            key => is_local_keep,
            type => boolean,
            default => false,
            description => "Keep a local copy of the files that are uploaded to the remote server. "
                           "If set, the storage server is used as a backup for the locally uploaded files."
        },
        #{
            key => is_upload_enabled,
            type => boolean,
            default => true,
            description => "Enable the upload of new files to the remote server."
        },
        #{
            key => delete_interval,
            type => string,
            default => <<"0">>,
            description => "The interval at which to delete files marked as deleted. "
                           "Set to 'false' to disable deletion of remote files. Use seconds, 'false', or "
                           "'N days/weeks/months' to specify the interval. "
                           "The default is '0', which means immediate deletion."
        }
    ]).

-behaviour(gen_server).

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

    testcred/1,

    queue_all/1,
    queue_all_stop/1,

    task_queue_all/3,

    lookup/2,
    lookup/3,

    update_backoff/2,
    batch_size/1,

    delete_ready/5,
    download_stream/5,
    manage_schema/2
    ]).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2
    ]).

-export([
    shorten_filename/1
    ]).

-record(state, {
        backoff :: backoff:backoff(),
        context :: z:context(),
        in_flight = 0 :: non_neg_integer()
    }).

observe_media_update_done(#media_update_done{action=insert, post_props=Props}, Context) ->
    queue_medium(Props, Context);
observe_media_update_done(#media_update_done{action=update, post_props=Props}, Context) ->
    queue_medium(Props, Context);
observe_media_update_done(#media_update_done{}, _Context) ->
    ok.

observe_filestore(#filestore{action=lookup, path=Path, local_path=OptLocalPath}, Context) ->
    lookup(Path, OptLocalPath, Context);
observe_filestore(#filestore{action=upload, path=Path, mime=undefined} = Upload, Context) ->
    Mime = z_media_identify:guess_mime(Path),
    observe_filestore(Upload#filestore{mime=Mime}, Context);
observe_filestore(#filestore{action=upload, path=Path, mime=Mime}, Context) ->
    MediaProps = #{
        <<"mime">> => Mime
    },
    maybe_queue_file(<<>>, Path, true, MediaProps, Context),
    ok;
observe_filestore(#filestore{action=delete, path=PathOrPrefix}, Context) ->
    case m_filestore:mark_deleted(PathOrPrefix, Context) of
        {ok, Count} ->
            ?LOG_INFO(#{
                text => <<"Filestore marked entries as deleted.">>,
                in => zotonic_mod_filestore,
                result => ok,
                path => PathOrPrefix,
                count => Count
            }),
            ok;
        {error, enoent} ->
            ?LOG_INFO(#{
                text => <<"Filestore no entries to delete.">>,
                in => zotonic_mod_filestore,
                result => ok,
                path => PathOrPrefix,
                count => 0
            }),
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
observe_filestore_credentials_lookup(#filestore_credentials_lookup{ path = Path }, Context) ->
    Service = filestore_config:service(Context),
    S3Key = filestore_config:s3key(Context),
    S3Secret = filestore_config:s3secret(Context),
    S3Url = filestore_config:s3url(Context),
    case is_defined(S3Key) andalso is_defined(S3Secret) andalso is_defined(S3Url) of
        true ->
            Url = make_url(S3Url, Path),
            {ok, #filestore_credentials{
                    service = Service,
                    service_url = S3Url,
                    location = Url,
                    credentials = #{
                        username => S3Key,
                        password => S3Secret,
                        tls_options => filestore_config:tls_options(Context)
                    }
            }};
        false ->
            undefined
    end.

%% @doc Given the service, find the credentials to do a lookup of the remote file.
observe_filestore_credentials_revlookup(
        #filestore_credentials_revlookup{
            service = Service,
            location = Location
        }, Context) ->
    ConfiguredService = filestore_config:service(Context),
    if
        Service =:= ConfiguredService ->
            S3Key = filestore_config:s3key(Context),
            S3Secret = filestore_config:s3secret(Context),
            S3Url = filestore_config:s3url(Context),
            case is_defined(S3Key) andalso is_defined(S3Secret) of
                true ->
                    {ok, #filestore_credentials{
                            service = Service,
                            service_url = S3Url,
                            location = Location,
                            credentials = #{
                                username => S3Key,
                                password => S3Secret,
                                tls_options => filestore_config:tls_options(Context)
                            }
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
        Dir -> z_convert:to_binary(filename:join(Dir, Basename1))
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
    Hash = z_crypto:hex_sha(OrgName),
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


is_defined(<<>>) -> false;
is_defined(_) -> true.

pid_observe_tick_1m(Pid, tick_1m, _Context) ->
    gen_server:cast(Pid, next_batch).

%% @doc Update the filestore backoff with a success or failure signal.
%% On failure, the batch size is lowered, on success it is increased.
-spec update_backoff(What, Context) -> ok when
    What :: success | fail,
    Context :: z:context().
update_backoff(What, Context) when What =:= success; What =:= fail ->
    Name = name(Context),
    gen_server:cast(Name, What).

%% @doc Return the current batch size, used for batch processing of
%% uploads, downloads and deletions. Between 0 and ?BATCH_SIZE.
-spec batch_size(Context) -> non_neg_integer() when
    Context :: z:context().
batch_size(Context) ->
    Name = name(Context),
    {ok, BatchSize} = gen_server:call(Name, batch_size),
    BatchSize.

manage_schema(What, Context) ->
    m_filestore:install(What, Context).

%% @doc Find a file in the filestore, if not found then return 'undefined'.
%% If the file is found and it is in the caching system then return a
%% reference to the cached file. If it is not cached then start a download
%% and return a reference to the download stream.
-spec lookup(Path, Context) -> Found | undefined when
    Path :: binary(),
    Context :: z:context(),
    Found :: {ok, {filename, Filename, StoreEntry}}
           | {ok, {filezcache, Pid, StoreEntry}},
    Filename :: binary(),
    Pid :: pid(),
    StoreEntry :: map().
lookup(Path, Context) ->
    lookup(Path, undefined, Context).

%% @doc If there is a local file and the 'is_local_keep' config is set then
%% return 'undefined' to let the lookup process use the local file.
%% If there is no local file then check the filestore, if not found then return 'undefined'.
%% If the file is found and it is in the caching system then return a
%% reference to the cached file. If it is not cached then start a download
%% and return a reference to the download stream.
-spec lookup(Path, LocalPath, Context) -> Found | undefined when
    Path :: binary(),
    LocalPath :: file:filename_all() | undefined,
    Context :: z:context(),
    Found :: {ok, {filename, Filename, StoreEntry}}
           | {ok, {filezcache, Pid, StoreEntry}},
    Filename :: binary(),
    Pid :: pid(),
    StoreEntry :: map().
lookup(Path, undefined, Context) ->
    lookup_1(Path, Context);
lookup(Path, LocalPath, Context) ->
    case filestore_config:is_local_keep(Context) of
        true ->
            case filelib:is_regular(LocalPath) of
                true ->
                    % There is a local file, let z_file_locate use that.
                    undefined;
                false ->
                    % No local file, let the filestore lookup proceed.
                    % TODO: on success we might want to download the remote
                    % file to the local file system.
                    lookup_1(Path, Context)
            end;
        false ->
            lookup_1(Path, Context)
    end.

lookup_1(Path, Context) ->
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
                            % Do not signal a backoff, as s3 has the strange behaviour
                            % of returning 'forbidden' on missing entries...
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
                            update_backoff(fail, Ctx),
                            exit(Error);
                        (stream_start) ->
                            nop;
                        (T) when is_tuple(T) ->
                            nop;
                        (B) when is_binary(B) ->
                            filezcache:append_stream(CachePid, B);
                         (eof) ->
                            update_backoff(success, Ctx),
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

%% @doc Try to put a file onto the remote server, testing the credentials.
-spec testcred(Context) -> ok | {error, Reason} when
    Context :: z:context(),
    Reason :: term().
testcred(Context) ->
    filestore_admin:testcred(Context).

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


name(Context) ->
    z_utils:name_for_site(?MODULE, Context).

%%% ------------------------------------------------------------------------------------
%%% Supervisor callbacks
%%% ------------------------------------------------------------------------------------

start_link(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    Name = name(Context),
    gen_server:start_link({local, Name}, ?MODULE, Args, []).

init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    z_context:ensure_logger_md(Context),
    {ok, #state{
        backoff = backoff:init(1, ?BATCH_SIZE - 1),
        context = Context,
        in_flight = 0
    }}.

handle_call(batch_size, _From, #state{ backoff = Backoff } = State) ->
    BatchSize = current_batch_size(Backoff),
    {reply, {ok, BatchSize}, State}.

handle_cast(next_batch, #state{ backoff = Backoff, context = Context } = State) ->
    BatchSize = current_batch_size(Backoff),
    case filestore_config:is_upload_enabled(Context) of
        true ->
            start_uploaders(m_filestore:fetch_queue(BatchSize, Context), Context);
        false ->
            ok
    end,
    start_downloaders(m_filestore:fetch_move_to_local(BatchSize, Context), Context),
    case filestore_config:delete_interval(Context) of
        <<"false">> ->
            ok;
        Interval ->
            start_deleters(m_filestore:fetch_deleted(Interval, BatchSize, Context), Context)
    end,
    {noreply, State};
handle_cast(success, #state{ backoff = Backoff } = State) ->
    {_, Backoff1} = backoff:succeed(Backoff),
    {noreply, State#state{ backoff = Backoff1 }};
handle_cast(fail, #state{ backoff = Backoff } = State) ->
    {_, Backoff1} = backoff:fail(Backoff),
    {noreply, State#state{ backoff = Backoff1 }}.


%%% ------------------------------------------------------------------------------------
%%% Support routines
%%% ------------------------------------------------------------------------------------

current_batch_size(Backoff) ->
    case z_sidejob:space() of
        N when N > (?BATCH_SIZE + 50) ->
            erlang:min(?BATCH_SIZE - backoff:get(Backoff), N);
        _ ->
            0
    end.

-spec start_uploaders({ok, [ m_filestore:queue_entry() ]} | {error, term()}, z:context()) -> ok.
start_uploaders({ok, Rs}, Context) ->
    lists:foreach(
        fun(QueueEntry) ->
            start_uploader(QueueEntry, Context)
        end,
        Rs);
start_uploaders({error, _}, _Context) ->
    % Ignore error, will be retried later
    ok.

start_uploader(#{ id := Id, path := Path, props := MediumInfo }, Context) ->
    Path1 = z_convert:to_binary(Path),
    PathLookup = m_filestore:lookup(Path1, Context),
    filestore_uploader:upload(Id, Path1, PathLookup, MediumInfo, Context).

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
    case z_notifier:first(#filestore_credentials_revlookup{
            service = Service,
            location = Location
        }, Context)
    of
        {ok, #filestore_credentials{
                service = CredService,
                service_url = CredServiceUrl,
                location = Location1,
                credentials = Cred
        }} when CredService =:= <<"s3">>;
                CredService =:= <<"webdav">>;
                CredService =:= <<"ftp">> ->
            case filestore_request:is_matching_url(CredServiceUrl, Location1) of
                true ->
                    ?LOG_DEBUG(#{
                        text => <<"Queue delete">>,
                        in => zotonic_mod_filestore,
                        path => Path,
                        service => CredService,
                        location => Location1,
                        id => Id
                    }),
                    ContextAsync = z_context:prune_for_async(Context),
                    Mod = filestore_request:filezmod(CredService),
                    _ = Mod:queue_delete_id({?MODULE, delete, Id}, Cred, Location1, {?MODULE, delete_ready, [Id, Path, ContextAsync]});
                false ->
                    ?LOG_WARNING(#{
                        in => zotonic_mod_filestore,
                        text => <<"Not deleting remote file as it is not matching with service url - dropping local ref">>,
                        result => error,
                        reason => service_url_mismatch,
                        service => CredService,
                        service_url => CredServiceUrl,
                        location => Location1,
                        id => Id,
                        path => Path,
                        action => delete
                    }),
                    m_filestore:purge_deleted(Id, Context)
            end;
        {ok, #filestore_credentials{ service = CredService }} ->
            ?LOG_DEBUG(#{
                text => <<"No credentials for queue delete -- service mismatch">>,
                in => zotonic_mod_filestore,
                service => Service,
                service_cred => CredService,
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
    ?LOG_INFO(#{
        text => <<"Delete remote file done">>,
        in => zotonic_mod_filestore,
        result => ok,
        path => Path,
        action => delete
    }),
    update_backoff(success, Context),
    m_filestore:purge_deleted(Id, Context);
delete_ready(Id, Path, Context, _Ref, {error, Reason})
    when Reason =:= enoent; Reason =:= epath ->
    ?LOG_INFO(#{
        text => <<"Delete remote file was not found">>,
        in => zotonic_mod_filestore,
        result => error,
        reason => Reason,
        path => Path,
        action => delete
    }),
    m_filestore:purge_deleted(Id, Context);
delete_ready(Id, Path, Context, _Ref, {error, forbidden}) ->
    ?LOG_WARNING(#{
        text => <<"Delete remote file was forbidden - dropping local ref">>,
        in => zotonic_mod_filestore,
        path => Path,
        result => error,
        reason => forbidden,
        id => Id,
        action => delete
    }),
    m_filestore:purge_deleted(Id, Context);
delete_ready(Id, Path, Context, _Ref, {error, Reason}) ->
    ?LOG_ERROR(#{
        text => <<"Delete remote file failed, will retry">>,
        in => zotonic_mod_filestore,
        path => Path,
        result => error,
        reason => Reason,
        id => Id,
        action => delete
    }),
    update_backoff(fail, Context).


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
    update_backoff(success, Context),
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
    case Reason of
        enoent -> ok;
        eacces -> ok;
        _ -> update_backoff(fail, Context)
    end,
    file:delete(temp_path(LocalPath)),
    m_filestore:unmark_move_to_local(Id, Context);
download_stream(_Id, _Path, _LocalPath, _Context, _Other) ->
    ok.

download_done(Id, Path, Context) ->
    m_filestore:purge_move_to_local(Id, filestore_config:is_local_keep(Context), Context),
    filezcache:delete({z_context:site(Context), Path}),
    filestore_uploader:stale_file_entry(Path, Context).

temp_path(F) when is_list(F) ->
    F ++ ".downloading";
temp_path(F) when is_binary(F) ->
    <<F/binary, ".downloading">>.
