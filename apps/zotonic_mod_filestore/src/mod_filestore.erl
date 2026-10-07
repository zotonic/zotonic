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
-moduledoc(#{
    zotonic_keywords => [
        "reference", "operator", "module", "file_storage", "file_store", "file_uploads", "reliability"
    ]
}).
-moduledoc("
Store uploaded media and generated previews on S3-compatible storage, FTP/FTPS,
or WebDAV. Zotonic keeps a database registry of remote files and serves them
through a shared local cache when they are no longer on the site's disk.

## Set up remote storage

1. Enable `mod_filestore` for the site.
2. Open **System → Cloud File Store** in the admin. Changing settings and starting
   bulk moves require the `use mod_admin_config` permission.
3. Enter the base URL and credentials for your storage service using the table below.
4. Select **Upload new media files to the cloud file store**. Decide whether to
   **Keep local files after upload** and choose the remote deletion delay.
5. Save the settings. Zotonic writes, reads back, and deletes a temporary
   `-zotonic-filestore-test-file-` beneath the base URL. Settings are saved only
   when this test succeeds.
6. Upload a test image, allow the upload queue to run, and check both the original
   and a resized preview. Use the bulk action to upload existing media when ready.

A successful credential test checks these operations at the configured location;
check normal media delivery as well. The test needs delete permission even when
remote deletion is set to **Never**.

### Supported services and URLs

| Service | Base URL example | Credentials | `service` value |
| --- | --- | --- | --- |
| S3-compatible | `https://mybucket.s3.amazonaws.com/mysite` | Access key and secret key | `s3` |
| FTP over TLS | `ftps://files.example.com/mysite` | Username and password | `ftp` |
| WebDAV over HTTPS | `webdavs://files.example.com/remote.php/dav/files/alice/mysite` | Username and password | `webdav` |

The admin derives the service from the URL scheme. `http:` and `https:` select
S3, not WebDAV. For WebDAV use `webdav:` or `webdavs:`; `dav:` and `davs:` are
aliases. Use `webdavs:` or `davs:` to protect the WebDAV credentials with HTTPS.

Both `ftp:` and `ftps:` select the FTP backend. The server must support TLS:
the client uses passive FTP with explicit TLS by default, or implicit TLS when
port 990 is specified, for example `ftps://files.example.com:990/mysite`.
Allow the server's passive data connections through the firewall. SFTP (SSH file
transfer) is not supported by this backend.

FTP and WebDAV create missing directories during upload. Their accounts need
permission to create directories and write, read, and delete files in the chosen
location. For S3, the admin can try to create a private bucket if it is missing;
this requires permission to create a bucket. The checkbox is a setup action, not
a saved configuration option.

### S3 permissions

Use a bucket or prefix dedicated to the site. Grant bucket listing where required
by the provider and grant the following object permissions beneath the base URL:

| Path | Permissions |
| --- | --- |
| `-zotonic-filestore-test-file-` | `s3:GetObject`, `s3:PutObject`, `s3:DeleteObject` |
| `archive/*` | `s3:GetObject`, `s3:PutObject`; `s3:DeleteObject` when remote deletion is enabled |
| `preview/*` | `s3:GetObject`, `s3:PutObject`; `s3:DeleteObject` when remote deletion is enabled |

Other applications using the filestore may write additional paths. Give those
paths the corresponding permissions. Files do not need public-read access:
Zotonic retrieves them using the configured credentials.

## Configuration reference

Site settings are stored under `mod_filestore` in `m_config`. The historical
`s3` prefixes also apply to FTP and WebDAV.

| Key | Meaning |
| --- | --- |
| `service` | Backend identifier: `s3`, `ftp`, or `webdav`. Set it explicitly outside the admin; an empty value falls back to `s3`. TLS variants are URL schemes, not backend identifiers. |
| `s3url` | Base URL, including the bucket or directory and optional site prefix. |
| `s3key` | S3 access key, or FTP/WebDAV username. |
| `s3secret` | S3 secret key, or FTP/WebDAV password. |
| `is_upload_enabled` | Allow background uploads. Set explicitly when configuring storage outside the admin. Disabling this does not disable reads, queued downloads, or remote deletion. |
| `is_local_keep` | Keep local files after successful upload. When false, uploaded files move into the evictable cache. |
| `delete_interval` | Extra delay before deleting files marked for remote deletion: `0` (the default, no extra delay), `false` (never), seconds, or a value such as `1 week`, `2 days`, or `3 months`. |
| `tls_options` | Erlang list of TLS options passed to the selected storage client. Not an admin form field; an empty list uses the client's defaults. |

### System-wide defaults and locked settings

The same keys can be set in the `zotonic_mod_filestore` application environment in
the system configuration. Nonempty site settings override these defaults unless
`is_config_locked` is true. This lock is a **system-wide** option: it makes sites
use the application settings and prevents editing them through the filestore form.

For example, merge this application entry into the system's Erlang configuration
list, using your own endpoint and credentials:

```erlang
{zotonic_mod_filestore, [
    {service, <<\"webdav\">>},
    {s3url, <<\"webdavs://files.example.com/zotonic/{{site}}\">>},
    {s3key, <<\"storage-user\">>},
    {s3secret, <<\"replace-with-password\">>},
    {is_upload_enabled, true},
    {is_local_keep, true},
    {delete_interval, <<\"false\">>},
    {is_config_locked, true}
]}
```

In a **system-configured** base URL, `{{site}}` is replaced with the site name.
If the placeholder is absent, Zotonic appends the site name as a directory.
A base URL saved in the site's admin is used as entered, without this expansion.
Configure `service` alongside the URL: URL-based service detection happens when
saving the admin form, not when reading application configuration.

## Uploads, local copies, and the cache

New media files and previews are queued for asynchronous upload. Queue entries
become eligible after one minute. Processing runs on a minute tick, with bounded
batches, database-load checks, and backoff, so completion can take longer.

With `is_local_keep` enabled, remote storage holds an additional copy of local
media. Otherwise, successfully uploaded files move from the site's files directory
into `filezcache`, where they can be evicted. When a remote-only file is requested,
Zotonic downloads it into the cache and can serve it while the download proceeds.

The `filezcache` application is shared by all sites. Its `max_bytes` application
setting controls cache capacity (default 10 GiB). Cache files are disposable;
retain the remote files and database registry. Keeping remote media copies does
not replace a backup of the site's database, configuration, and application code.

## Deletion and moving existing files

Zotonic normally retains deleted media for five weeks to allow recovery. The
filestore's `delete_interval` adds a delay after a file is marked for remote
deletion. `0` means no **extra** delay, not deletion at the instant a page is
removed. `false` keeps remote files indefinitely; it does not make the remote
service itself immutable.

Use the admin's bulk actions to queue existing local media for upload or to move
remote files back to the server's disk. These operations run in the background;
watch the queues and logs. Moving files to disk requires enough disk space and
working credentials for their existing locations. Disable uploads when bringing
files back for a storage migration.

Before switching to a different service or account, bring the files back locally
and verify that the download queue has completed. Then configure and test the new
location and queue uploads. Changing the base URL alone does not copy existing
remote objects: the registry retains their original service and location. The
default reverse lookup uses credentials for the currently configured service.
Remote deletion also checks that an object's URL matches the configured base URL.

## Statistics and troubleshooting

The admin shows registered media, estimated local file counts and sizes, remote
file counts and sizes, and upload, download, and delete queue counts. These figures
come from the site's database; they are not a scan or integrity check of remote
storage. Local totals are estimates, not a scan of the files directory.

If uploads do not progress, check that uploads are enabled, the module is active,
and the URL and credentials pass the settings test. Inspect logs for connection,
permission, TLS, or storage errors. For FTP, check passive data connections; for
WebDAV, check the full collection path and the `webdavs:` scheme. Allow for the
queue delay and backoff before assuming a newly queued file is stuck.

## Integration points

`mod_filestore` handles these notifications:

- `#media_update_done{}` queues inserted or updated media files.
- `#filestore{}` handles file lookup, upload, and deletion through the file registry and cache.
- `#filestore_request{}` handles direct storage upload, download, and deletion requests.
- `#filestore_credentials_lookup{}` maps a local path and optional resource ID to remote credentials and location.
- `#filestore_credentials_revlookup{}` resolves credentials for an existing remote service and location.
- `#admin_menu{}` adds the admin menu entry.

Applications can provide credential lookup observers to route files to different
services. See `zotonic_file.hrl` for the notification records and `model#filestore`
for the configuration and statistics model. Storage requests use `s3filez`,
`ftpfilez`, or `webdavfilez`; `filezcache` manages the shared download cache.
").

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
            description => "The service to use for storing files. One of: s3, ftp, webdav. TLS is selected by the URL scheme"
        },
        #{
            key => s3url,
            type => string,
            default => "",
            description => "The base URL of the S3, FTP/FTPS, or WebDAV service, including its bucket or directory"
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
                           "The default is '0', which means no extra delay after a file is marked for remote deletion."
        }
    ]).

-behaviour(gen_server).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

-define(BATCH_SIZE, 500).  %% Max batch size for filestore batch operations (queue_all and upload/download/delete processing)
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
    next_batch/2,

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
    % Isolate database/queue failures from the module process. A slow batch
    % must not overlap with the next tick for this site.
    case z_sidejob:start_site_unique(mod_filestore_next_batch,
        ?MODULE, next_batch, [BatchSize], Context)
    of
        {ok, _Pid} -> ok;
        {error, already_running} -> ok;
        {error, overload} -> ok
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

%% @doc Site-unique sidejob entry point. Leave queued work for the next tick
%% when foreground requests are using the database connections.
-spec next_batch(BatchSize, Context) -> ok | {error, busy} when
    BatchSize :: non_neg_integer(),
    Context :: z:context().
next_batch(BatchSize, Context) ->
    z_db:run_if_low_load(fun() -> next_batch_1(BatchSize, Context) end, Context).

next_batch_1(BatchSize, Context) ->
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
    end.

current_batch_size(Backoff) ->
    case z_sidejob:space() of
        N when N > 50 ->
            erlang:max(erlang:min(?BATCH_SIZE - backoff:get(Backoff), N - 50), 0);
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
