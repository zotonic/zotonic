%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2026 Marc Worrell
%% @doc Models for file-storage administration
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

-module(m_filestore).
-moduledoc(#{
    zotonic_keywords => ["reference", "operator", "model", "file_storage", "file_store", "reliability"]
}).
-moduledoc("
Read filestore configuration and statistics through `m.filestore`, for example
`{{ m.filestore.service }}` or `{{ m.filestore.stats.cloud }}` in an admin template.
All model paths below require an administrator context; unauthorized calls return
`{error, eacces}`. The secret is also unavailable when system configuration is locked.
See `module#mod_filestore` for setup, protocols, configuration precedence and migration.

## File registry and upload queue

The `filestore` table records each relative local path, its backend (`s3`, `ftp`,
or `webdav`), full remote URL and size. It also tracks local copies, errors, deletion
and moves back to local disk. Use the filestore APIs rather than editing the table.

Paths are relative to the site's files directory, for example
`archive/2026/10/07/image.jpg`. Remote locations retain the URL used for upload;
changing configuration does not rewrite them or move the remote files.

The `filestore_queue` table holds pending uploads. Entries become eligible after
one minute; bounded background batches and backoff determine when they complete.
Statistics are database-derived counts and estimates, not remote or local disk scans.

Remote cache reads retain failed entries and retry on demand with exponential
backoff (5 seconds initially, 80–100% jitter, capped at 10 minutes). There is no
permanent cutoff. Successful reads reset the error history. Cached copies remain
available during a cooldown. A ten-minute database claim prevents simultaneous
probes across nodes and expires if the worker dies. Only its current token can
record a result. Existing file-serving negative caches can delay the next lookup.

`health` reports completed remote cache reads in the current and preceding fourteen
minute buckets, shared across nodes. A warning requires at least five failures
and a failure rate of at least 20%. Unresolved errors older than 24 hours also warn.
User 1 can mark an error older than 24 hours as unrecoverable. This stops read
retries and excludes the entry from active error counts, while retaining its
record and details. A new successful upload resets this flag. This action never
deletes local or remote files. Historical request-rate statistics remain unchanged.
Cache hits and suppressed retries are excluded. Buckets are retained for one day
and pruned when the first result of a new minute is recorded.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/prolonged_errors/+page/...` | Return admin-only pages of 50 unresolved read errors older than 24 hours, oldest first. Includes relative file paths, service, last error, consecutive failure count, first failure time and retry eligibility. |
| `get` | `/health/...` | Return admin-only remote read attempts, failures, high-error-rate flag, and unresolved/prolonged error counts. |
| `get` | `/stats/...` | Return admin-only filestore totals: archived media count/size, queued upload count, queued move/delete counts, cloud-stored count/size, and estimated local count/size. |
| `get` | `/service/...` | Return the configured filestore backend service identifier from `filestore_config:service/1` (admin-only). |
| `get` | `/s3url/...` | Return the configured S3, FTP/FTPS or WebDAV base URL used for remote object location resolution (admin-only). |
| `get` | `/s3region/...` | Return the S3 signing region; empty selects Signature V2 (admin-only). |
| `get` | `/s3key/...` | Return the configured S3 access key or FTP/WebDAV username for filestore credentials (admin-only). |
| `get` | `/s3secret/...` | Return the configured S3 secret key or FTP/WebDAV password only when caller is admin and filestore config is not locked. |
| `get` | `/is_config_locked/...` | Return whether filestore credential/config values are locked (non-editable/hidden) in this site instance (admin-only). |
| `get` | `/is_upload_enabled/...` | Return whether background upload/offload from local `files` storage to remote filestore is enabled (admin-only). |
| `get` | `/is_local_keep/...` | Return whether local files are retained after successful remote storage, instead of being scheduled for deletion (admin-only). |
| `get` | `/delete_interval/...` | Return the configured extra delay before deleting files marked for remote deletion (admin-only). |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").

-export([
    m_get/3,

    queue/3,
    fetch_queue/1,
    fetch_queue/2,
    dequeue/2,
    mark_error/3,
    mark_unrecoverable/2,
    claim_download/2,
    complete_download/4,
    retry_delay/1,
    health/1,
    mark_deleted/2,
    fetch_deleted/2,
    fetch_deleted/3,
    purge_deleted/2,
    clear_deleted/2,

    mark_move_to_local_all/1,
    mark_move_to_local_limit/2,
    mark_move_to_local/2,

    unmark_move_to_local_all/1,
    unmark_move_to_local_limit/2,
    unmark_move_to_local/2,

    fetch_move_to_local/1,
    fetch_move_to_local/2,
    purge_move_to_local/3,

    store/6,
    lookup/2,

    is_upload_ok/1,
    is_download_ok/1,

    stats/1,

    install/2
    ]).

-type queue_entry() :: map().
-type filestore_entry() :: map().

-export_type([
    queue_entry/0,
    filestore_entry/0
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(BATCH_SIZE, 200).

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"prolonged_errors">>, Page | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {prolonged_errors(Page, Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"health">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {health(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"stats">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {stats(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"service">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {filestore_config:service(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"s3url">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {filestore_config:s3url(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"s3region">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {filestore_config:s3region(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"s3key">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {filestore_config:s3key(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"s3secret">> | Rest ], _Msg, Context) ->
    % Only show the secret if the config is not locked, the secret MUST be local
    % to the current site.
    case z_acl:is_admin(Context) andalso not filestore_config:is_config_locked() of
        true -> {ok, {filestore_config:s3secret(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"is_config_locked">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {filestore_config:is_config_locked(), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"is_upload_enabled">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {filestore_config:is_upload_enabled(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"is_local_keep">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {filestore_config:is_local_keep(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"delete_interval">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            {ok, {filestore_config:delete_interval(Context), Rest}};
        false -> {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

%% @doc Add a file/medium to the upload queue in the database. This queue is periodically
%% polled and an uploader will be started for every entry in the queue.
-spec queue(binary(), z_media_identify:media_info(), z:context()) -> ok | {error, duplicate}.
queue(Path, MediaProps, Context) ->
    % Media notifications and queue scans can enqueue the same path concurrently.
    case z_db:q("
            insert into filestore_queue (path, props)
            values ($1,$2)
            on conflict (path) do nothing",
            [Path, ?DB_PROPS(MediaProps)], Context)
    of
        1 -> ok;
        0 -> {error, duplicate}
    end.

%% @doc Fetch the next batch of queued uploads, at least 1 minute old and max 200.
-spec fetch_queue( z:context() ) -> {ok, [ queue_entry() ]} | {error, term()}.
fetch_queue(Context) ->
    fetch_queue(?BATCH_SIZE, Context).


%% @doc Fetch the next batch of queued uploads, at least 1 minute old and max BatchSize.
-spec fetch_queue(BatchSize, Context) -> {ok, [ queue_entry() ]} | {error, term()} when
    BatchSize :: non_neg_integer(),
    Context :: z:context().
fetch_queue(BatchSize, Context) ->
    case z_db:qmap("
                select *
                from filestore_queue
                where created < now() - interval '1 min'
                limit $1",
                [ BatchSize ],
                [ {keys, atom} ],
                Context)
    of
        {ok, Rs} ->
            % 0.x filestore queues can have property lists in the queue, change those
            % property lists to maps.
            Rs1 = lists:map(
                fun
                    (#{ props := L } = M) when is_list(L) ->
                        M#{ props => maps:from_list(L) };
                    (M) ->
                        M
                end,
                Rs),
            {ok, Rs1};
        {error, _} = Error ->
            % Ignore errors, later retry will fix this
            Error
    end.

%% @doc Remove an entry from the upload queue.
-spec dequeue( integer(), z:context() ) -> ok | {error, enoent}.
dequeue(Id, Context) ->
    case z_db:q("delete from filestore_queue where id = $1", [Id], Context) of
        1 -> ok;
        0 -> {error, enoent}
    end.

%% @doc Store a mapping of a local file at path to a file at the service location.
%% The path must be unique and is used to identify the file. Use an upsert so
%% concurrent uploaders storing the same path are idempotent.
-spec store(Path, Size, Service, Location, IsLocal, Context) -> Result when
    Path :: binary(),
    Size :: non_neg_integer(),
    Service :: atom() | binary(),
    Location :: binary(),
    IsLocal :: boolean(),
    Context :: z:context(),
    FileId :: integer(),
    Result :: {ok, FileId} | {error, term()}.
store(Path, Size, Service, Location, IsLocal, Context)
    when is_binary(Path), is_integer(Size), is_binary(Location) ->
    case z_db:qmap_row("
            insert into filestore
                (path, service, location, size, is_local)
            values
                ($1, $2, $3, $4, $5)
            on conflict (path)
            do update set
                location = excluded.location,
                service = excluded.service,
                size = excluded.size,
                is_deleted = false,
                is_local = excluded.is_local,
                is_move_to_local = false,
                is_unrecoverable = false,
                error = null,
                error_count = 0,
                error_since = null,
                retry_after = null,
                download_token = null,
                modified = now()
            returning id",
            [ Path, Service, Location, Size, IsLocal ],
            [ {keys, atom} ],
            Context)
    of
        {ok, #{ id := Id }} -> {ok, Id};
        {error, _} = Error -> Error
    end.

-spec lookup( binary(), z:context() ) -> {ok, map()} | {error, term()}.
lookup(Path, Context) ->
    z_db:qmap_row("
        select *
        from filestore
        where path = $1
          and is_deleted = false",
        [Path],
        [ {keys, atom} ],
        Context).

%% Check if it is ok to upload to the location of this queue entry.
%% If it is not ok then the queue entry could be deleted.
-spec is_upload_ok( undefined | queue_entry() ) -> boolean().
is_upload_ok(undefined) ->
    true;
is_upload_ok(#{ error := undefined }) ->
    false;
is_upload_ok(#{ error := _ }) ->
    true.

%% @doc Check if it is ok to download the given file.
-spec is_download_ok( undefined | filestore_entry() ) -> boolean().
is_download_ok(undefined) ->
    true;
is_download_ok(#{ error := undefined }) ->
    true;
is_download_ok(#{ error := _ }) ->
    false.

%% @doc Mark a filestore entry as being erronous, happens when the
%% remote server can't be reached or gives an error on access.
%% This can be a transient error, so we mark the entry with the
%% current time for a later retry.
-spec mark_error( integer(), binary()|atom(), z:context() ) -> ok | {error, enoent}.
mark_error(Id, Error, Context) ->
    case claim_download(Id, Context) of
        {ok, Claim} -> complete_download(Id, Claim, {error, Error}, Context);
        {error, _} = Result -> Result
    end.

%% @doc Reserve a remote read across nodes. An abandoned claim expires after ten
%% minutes. The token prevents a late callback from overwriting a newer result.
-spec claim_download(integer(), z:context()) -> {ok, map()} | {error, term()}.
claim_download(Id, Context) ->
    z_db:qmap_row("update filestore
        set download_token = $2, retry_after = now() + interval '10 minutes'
        where id = $1 and not is_deleted and not is_unrecoverable
          and (retry_after is null or retry_after <= now())
        returning download_token, error_count",
        [Id, z_ids:id()], [{keys, atom}], Context).

%% @doc Exponential delay in seconds, with 80-100% jitter and a hard ten-minute cap.
-spec retry_delay(non_neg_integer()) -> pos_integer().
retry_delay(Count) ->
    Max = min(600, 5 bsl min(Count, 7)),
    Min = max(1, Max * 4 div 5),
    Min + rand:uniform(Max - Min + 1) - 1.

%% @doc Only completed remote attempts contribute to the error rate, not cache
%% hits or requests suppressed by the cooldown. Updates and metrics are atomic.
-spec complete_download(integer(), map(), ok | {error, term()}, z:context()) -> ok | {error, term()}.
complete_download(Id, #{download_token := Token, error_count := Count}, Result, Context) ->
    z_db:transaction(fun(Ctx) ->
        {Sql, Args, Failures} = case Result of
            ok ->
                {"error = null, error_count = 0, error_since = null, retry_after = null",
                 [Id, Token], 0};
            {error, Reason} ->
                {"error = $3, error_count = least(error_count + 1, 1000000),
                  error_since = coalesce(error_since, now()),
                  retry_after = now() + $4 * interval '1 second'",
                 [Id, Token, error_name(Reason), retry_delay(Count)], 1}
        end,
        case z_db:q("update filestore set download_token = null, " ++ Sql ++
                    " where id = $1 and download_token = $2 and not is_deleted and not is_unrecoverable", Args, Ctx) of
            1 ->
                Attempts = z_db:q1("insert into filestore_read_stats (minute, attempts, failures)
                    values (date_trunc('minute', now()), 1, $1)
                    on conflict (minute) do update set
                        attempts = filestore_read_stats.attempts + 1,
                        failures = filestore_read_stats.failures + excluded.failures
                    returning attempts",
                    [Failures], Ctx),
                case Attempts of
                    1 -> z_db:q("delete from filestore_read_stats
                            where minute < now() - interval '1 day'", Ctx);
                    _ -> ok
                end,
                ok;
            0 -> ok
        end
    end, Context).

error_name(Reason) when is_atom(Reason) -> error_name(atom_to_binary(Reason));
error_name(Reason) when is_binary(Reason), byte_size(Reason) =< 32 -> Reason;
error_name(_) -> <<"remote_error">>.

%% @doc Admin-only model endpoint; minute buckets give a bounded, cluster-wide
%% fifteen-minute window. Persistent error age survives restarts and inactivity.
-spec health(z:context()) -> map().
health(Context) ->
    {ok, Rates} = z_db:qmap_row("select coalesce(sum(attempts), 0)::bigint as attempts,
            coalesce(sum(failures), 0)::bigint as failures
        from filestore_read_stats
        where minute >= date_trunc('minute', now()) - interval '14 minutes'",
        [], [{keys, atom}], Context),
    {ok, Errors} = z_db:qmap_row("select count(*) as unresolved,
            count(*) filter (where error_since < now() - interval '24 hours') as prolonged
        from filestore where error is not null and not is_deleted and not is_unrecoverable",
        [], [{keys, atom}], Context),
    #{attempts := Attempts, failures := Failures} = Rates,
    maps:merge(Errors, Rates#{
        high_error_rate => Failures >= 5 andalso Failures * 5 >= Attempts
    }).


%% @doc Stop read retries without deleting the entry or remote file. Only the
%% administrator account (user 1) can do this. Recheck age and state atomically
%% so a stale admin page cannot mark a recovered or recently failed file.
-spec mark_unrecoverable(Id, Context) -> Result when
    Id :: integer(),
    Context :: z:context(),
    Result :: ok | {error, eacces | badarg | not_eligible}.
mark_unrecoverable(Id, Context) ->
    case z_acl:user(Context) =:= 1 andalso not z_acl:is_read_only(Context) of
        false -> {error, eacces};
        true when is_integer(Id), Id > 0 ->
            case z_db:q("update filestore
                    set is_unrecoverable = true, download_token = null, retry_after = null,
                        is_move_to_local = false
                    where id = $1 and not is_deleted and not is_unrecoverable
                      and error is not null
                      and error_since < now() - interval '24 hours'", [Id], Context) of
                1 ->
                    ?LOG_NOTICE(#{
                        text => <<"Filestore entry marked unrecoverable">>,
                        in => zotonic_mod_filestore,
                        id => Id,
                        user_id => 1
                    }),
                    ok;
                0 -> {error, not_eligible}
            end;
        true -> {error, badarg}
    end.


%% @doc Paginate unresolved read failures, oldest first. Expose relative paths,
%% not remote URLs, which can contain credentials or signed query parameters.
-spec prolonged_errors(Page, Context) -> map() when
    Page :: term(),
    Context :: z:context().
prolonged_errors(Page0, Context) ->
    Page = try z_convert:to_integer(Page0) of
        N when is_integer(N), N > 0 -> min(N, 1000000);
        _ -> 1
    catch
        _:_ -> 1
    end,
    {ok, Rows} = z_db:qmap("select id, path, service, error, error_count, error_since, retry_after, is_unrecoverable
        from filestore
        where error is not null and not is_deleted
          and error_since < now() - interval '24 hours'
        order by error_since, id
        limit 51 offset $1", [(Page - 1) * 50], [{keys, atom}], Context),
    #{
        entries => lists:sublist(Rows, 50),
        page => Page,
        previous => case Page of 1 -> undefined; _ -> Page - 1 end,
        next => case length(Rows) of 51 -> Page + 1; _ -> undefined end
    }.


%% @doc Mark the file entries as deleted for the path or having the path as a prefix.
-spec mark_deleted(Path, Context) -> {ok, Count} | {error, enoent} when
    Path :: binary() | {prefix, binary()},
    Context :: z:context(),
    Count :: pos_integer().
mark_deleted({prefix, Path}, Context) when is_binary(Path) ->
    case z_db:q("update filestore
            set is_deleted = true,
                modified = now(),
                deleted = now()
            where path like $1",
            [<<Path/binary, $%>>],
            Context)
    of
        N when N > 0 -> {ok, N};
        0 -> {error, enoent}
    end;
mark_deleted(Path, Context) when is_binary(Path) ->
    case z_db:q("update filestore
            set is_deleted = true,
                modified = now(),
                deleted = now()
            where path = $1",
            [Path],
            Context)
    of
        N when N > 0 -> {ok, N};
        0 -> {error, enoent}
    end.


%% @doc Fetch all deleted file entries where the entry was marked as deleted
%% at least 'Interval' ago. The Interval comes from the mod_filestore.delete_interval
%% configuration.
-spec fetch_deleted(Interval, Context) -> {ok, [ filestore_entry() ]} | {error, term()} when
    Interval :: binary() | undefined,
    Context :: z:context().
fetch_deleted(Interval, Context) ->
    fetch_deleted(Interval, ?BATCH_SIZE, Context).

%% @doc Fetch all deleted file entries where the entry was marked as deleted
%% at least 'Interval' ago. The Interval comes from the mod_filestore.delete_interval
%% configuration.
-spec fetch_deleted(Interval, BatchSize, Context) -> {ok, [ filestore_entry() ]} | {error, term()} when
    Interval :: binary() | undefined,
    BatchSize :: non_neg_integer(),
    Context :: z:context().
fetch_deleted(Interval, BatchSize, Context) ->
    case map_interval(Interval) of
        <<"false">> ->
            {ok, []};
        <<"0">> ->
            z_db:qmap(
                "select * from filestore where is_deleted = true limit $1",
                [ BatchSize ],
                [ {keys, atom} ],
                Context);
        Interval1 ->
            z_db:qmap(<<
                "select * ",
                "from filestore ",
                "where is_deleted =true "
                "  and deleted < now() - interval '", Interval1/binary,"' ",
                "limit $1">>,
                [ BatchSize ],
                [ {keys, atom} ],
                Context)
    end.

map_interval(undefined) ->
    <<"0">>;
map_interval(Interval) ->
    try
        L = case binary:split(Interval, <<" ">>) of
            [ Number ] -> num(Number);
            [ <<"1">>, <<"day">> ] -> <<"1 day">>;
            [ Number, <<"days">> ] -> [ num(Number), " days" ];
            [ <<"1">>, <<"week">> ] -> <<"1 week">>;
            [ Number, <<"weeks">> ] -> [ num(Number), " weeks" ];
            [ <<"1">>, <<"month">> ] -> <<"1 month">>;
            [ Number, <<"months">> ] -> [ num(Number), " months" ];
            _ -> <<"false">>
        end,
        iolist_to_binary(L)
    catch
        error:badarg ->
            ?LOG_ERROR(#{
                text => <<"Filestore illegal delete interval">>,
                in => zotonic_mod_filestore,
                result => error,
                reason => badarg,
                interval => Interval
            }),
            <<"false">>
    end.

num(N) -> integer_to_binary( binary_to_integer(N) ).


%% @doc Remove the deleted file entry from the filestore table. This is done after
%% the file has been deleted from the remote service.
-spec purge_deleted( integer(), z:context() ) -> ok | {error, enoent}.
purge_deleted(Id, Context) ->
    case z_db:q("delete from filestore where id = $1 and is_deleted = true", [Id], Context) of
        1 -> ok;
        0 -> {error, enoent}
    end.

%% @doc Unmark the file entry as deleted, used when the deletion could not be done
%% due to an error or some other condition.
-spec clear_deleted(Id, Context) -> ok | {error, enoent} when
    Id :: integer(),
    Context :: z:context().
clear_deleted(Id, Context) when is_integer(Id) ->
    case z_db:q("
            update filestore
            set is_deleted = false,
                modified = now(),
                deleted = undefined
            where id = $1",
            [Id],
            Context)
    of
        1 -> ok;
        0 -> {error, enoent}
    end.


%% @doc Mark at most Limit entries to be moved from the remote service
%% to the local service. We mark all entries so that any missing files are
%% recovered.
-spec mark_move_to_local_limit( non_neg_integer(), z:context() ) -> {ok, non_neg_integer()}.
mark_move_to_local_limit(Limit, Context) ->
    z_db:transaction(fun(Ctx) ->
                        N = z_db:q1("
                                update filestore f
                                set is_move_to_local = true
                                from (
                                     select id
                                     from filestore
                                     where is_move_to_local = false
                                       and is_deleted = false
                                       and error is null
                                     limit $1
                                     for update
                                ) mv
                                where mv.id = f.id",
                                [Limit],
                                Ctx),
                        {ok, N}
                     end,
                     Context).

%% @doc Mark all filestore entries to be moved from the remote service
%% to the local service. We mark all entries so that any missing files are
%% recovered.
-spec mark_move_to_local_all( z:context() ) -> non_neg_integer().
mark_move_to_local_all(Context) ->
    z_db:q1("update filestore
             set is_move_to_local = true
             where is_move_to_local = false
               and is_deleted = false
               and error is null", Context).

%% @doc Mark the given filestore entry to be moved from the remote service
%% to the local service.
-spec mark_move_to_local( integer(), z:context() ) -> ok | {error, enoent}.
mark_move_to_local(Id, Context) ->
    case z_db:q1("
            update filestore
            set is_move_to_local = true
            where id = $1", [Id], Context)
    of
        1 -> ok;
        0 -> {error, enoent}
    end.


%% @doc Remove the "move to local" mark from at most Limit filestore entries.
-spec unmark_move_to_local_limit( non_neg_integer(), z:context() ) -> {ok, non_neg_integer()}.
unmark_move_to_local_limit(Limit, Context) ->
    z_db:transaction(
        fun(Ctx) ->
            case z_db:q("
                    update filestore f
                    set is_move_to_local = false
                    from (
                         select id
                         from filestore
                         where is_move_to_local = true
                         limit $1
                         for update
                    ) mv
                    where mv.id = f.id",
                    [Limit],
                    Ctx)
            of
                N when is_integer(N) ->
                    {ok, N}
            end
        end,
        Context).

%% @doc Remove the "move to local" mark from all filestore entries.
-spec unmark_move_to_local_all( z:context() ) -> non_neg_integer().
unmark_move_to_local_all(Context) ->
    z_db:q1("update filestore
             set is_move_to_local = false
             where is_move_to_local = true", Context).


%% @doc Remove the "move to local" mark from the given filestore entry.
-spec unmark_move_to_local( integer(), z:context() ) -> ok | {error, enoent}.
unmark_move_to_local(Id, Context) ->
    case z_db:q1("
            update filestore
            set is_move_to_local = false
            where id = $1", [Id], Context)
    of
        1 -> ok;
        0 -> {error, enoent}
    end.

%% @doc Fetch at most 200 filestore entries that are marked with "move to local".
-spec fetch_move_to_local( z:context() ) -> {ok, [ filestore_entry() ]} | {error, term()}.
fetch_move_to_local(Context) ->
    fetch_move_to_local(?BATCH_SIZE, Context).

%% @doc Fetch at most BatchSize filestore entries that are marked with "move to local".
-spec fetch_move_to_local(BatchSize, Context) -> {ok, [ filestore_entry() ]} | {error, term()} when
    BatchSize :: non_neg_integer(),
    Context :: z:context().
fetch_move_to_local(BatchSize, Context) ->
    z_db:qmap("
            select *
            from filestore
            where is_move_to_local = true
              and is_deleted = false
              and error is null
            limit $1",
            [ BatchSize ],
            [ {keys, atom} ],
            Context).

%% @doc Called after a file has been moved from the remote service to local.
%% Marks the entry as deleted and removes the 'move to local' flag.
-spec purge_move_to_local(FileId, IsLocalKeep, Context) -> Count when
    FileId :: pos_integer(),
    IsLocalKeep :: boolean(),
    Context :: z:context(),
    Count :: non_neg_integer().
purge_move_to_local(Id, true, Context) ->
    z_db:q1("update filestore
             set is_move_to_local = false,
                 is_local = true
             where id = $1", [Id], Context);
purge_move_to_local(Id, false, Context) ->
    z_db:q1("update filestore
             set is_move_to_local = false,
                 is_deleted = true,
                 deleted = now()
             where id = $1", [Id], Context).

%% @doc Return some basic stats about the filestore.
-spec stats( z:context() ) -> map().
stats(Context) ->
    {Archived, ArchiveSize} = z_db:q_row("
                            select count(*), coalesce(sum(size), 0)::bigint
                            from medium
                            where filename is not null
                              and filename <> ''
                              and is_deletable_file", Context),
    Queued = z_db:q1("select count(*) from filestore_queue", Context),
    {Cloud, CloudSize, ToLocal, Deleted} = z_db:q_row("
                            select count(*),
                                   coalesce(sum(size), 0)::bigint,
                                   coalesce(sum(is_move_to_local::integer), 0),
                                   coalesce(sum(is_deleted::integer), 0)
                            from filestore
                            where is_local = false", Context),

    {CloudLocal, CloudLocalSize, DeletedLocal} = z_db:q_row("
                            select count(*),
                                   coalesce(sum(size), 0)::bigint,
                                   coalesce(sum(is_deleted::integer), 0)
                            from filestore
                            where is_local = true", Context),


    % TODO: we need a separate index for this lookup
    {InCloudOnly, InCloudOnlySize} = z_db:q_row("
                            select count(*), coalesce(sum(m.size), 0)::bigint
                            from medium m
                                    join filestore f
                                    on f.path = 'archive/' || m.filename
                                    and f.is_local = false
                            where filename is not null
                              and filename <> ''
                              and is_deletable_file = true",
                            Context),
    #{
        archived => Archived,
        archive_size => ArchiveSize,
        queued => Queued,
        queued_local => ToLocal,
        queued_deleted => Deleted + DeletedLocal,
        cloud => Cloud + CloudLocal,
        cloud_size => CloudSize + CloudLocalSize,
        local => Archived - InCloudOnly,
        local_size => ArchiveSize - InCloudOnlySize
    }.



install(_Version, Context) ->
    ok = install_filestore(Context),
    ok = ensure_column_deleted(Context),
    ok = ensure_column_is_local(Context),
    ok = install_filequeue(Context),
    ok = ensure_size_bigint(Context),
    ok = install_retries(Context),
    ok = install_unrecoverable(Context).

install_filestore(Context) ->
    case z_db:table_exists(filestore, Context) of
        false ->
            [] = z_db:q("
                create table filestore (
                    id serial not null,
                    is_deleted boolean not null default false,
                    is_local boolean not null default false,
                    is_move_to_local boolean not null default false,
                    is_unrecoverable boolean not null default false,
                    error character varying(32),
                    error_count integer not null default 0,
                    error_since timestamp with time zone,
                    retry_after timestamp with time zone,
                    download_token character varying(32),
                    path character varying(500) not null,
                    service character varying(16) not null,
                    location character varying(400) not null,
                    size bigint not null default 0,
                    modified timestamp with time zone not null default now(),
                    created timestamp with time zone not null default now(),
                    deleted timestamp with time zone,

                    constraint filestore_pkey primary key (id),
                    constraint filestore_path_key unique (path),
                    constraint filestore_location_key unique (location)
                )
                ", Context),
            {ok, _, _} = z_db:equery("create index filestore_is_deleted_key on filestore(is_deleted) where is_deleted", Context),
            {ok, _, _} = z_db:equery("create index filestore_is_move_to_local_key on filestore(is_move_to_local) where is_move_to_local", Context),
            {ok, _, _} = z_db:equery("create index filestore_deleted on filestore(deleted)", Context),
            {ok, _, _} = z_db:equery("create index filestore_is_local on filestore(is_local)", Context),
            z_db:flush(Context),
            ok;
        true ->
            case z_db:column(filestore, path, Context) of
                {ok, #column_def{ length = Len2 }} when Len2 < 500 ->
                    [] = z_db:q("
                        alter table filestore
                        alter column path type character varying(500)
                        ",
                        Context),
                    z_db:flush(Context),
                    ok;
                {ok, _} ->
                    ok
            end
    end.

install_filequeue(Context) ->
    case z_db:table_exists(filestore_queue, Context) of
        false ->
            [] = z_db:q("
                create table filestore_queue (
                    id serial not null,
                    path character varying(500) not null,
                    props bytea,
                    created timestamp with time zone not null default now(),

                    constraint filestore_queue_pkey primary key (id),
                    constraint filestore_queue_path_key unique (path)
                )
                ", Context),
            z_db:flush(Context),
            ok;
        true ->
            case z_db:column(filestore_queue, path, Context) of
                {ok, #column_def{ length = Len1 }} when Len1 < 500 ->
                    [] = z_db:q("
                        alter table filestore_queue
                        alter column path type character varying(500)
                        ",
                        Context),
                    z_db:flush(Context),
                    ok;
                {ok, _} ->
                    ok
            end
    end.

ensure_column_deleted(Context) ->
    Columns = z_db:column_names(filestore, Context),
    case lists:member(deleted, Columns) of
        true ->
            ok;
        false ->
            [] = z_db:q("alter table filestore add column deleted timestamp with time zone", Context),
            {ok, _, _} = z_db:equery("create index filestore_deleted on filestore(deleted)", Context),
            z_db:flush(Context),
            ok
    end.

ensure_column_is_local(Context) ->
    Columns = z_db:column_names(filestore, Context),
    case lists:member(is_local, Columns) of
        true ->
            ok;
        false ->
            [] = z_db:q("alter table filestore add column is_local boolean not null default false", Context),
            {ok, _, _} = z_db:equery("create index filestore_is_local on filestore(is_local)", Context),
            z_db:flush(Context),
            ok
    end.

ensure_size_bigint(Context) ->
    case z_db:column(filestore, size, Context) of
        {ok, #column_def{ type = <<"integer">> }} ->
            % Run with a timeout of 10 minutes, as this can be a long operation:
            z_db:q("alter table filestore alter column size type bigint", [], Context, 10*60*1000),
            z_db:flush(Context);
        {ok, _} ->
            ok
    end.

%% Existing errors become immediately eligible; preserve their original age.
install_retries(Context) ->
    case lists:member(error_count, z_db:column_names(filestore, Context)) of
        true -> ok;
        false ->
            [] = z_db:q("alter table filestore
                add column error_count integer not null default 0,
                add column error_since timestamp with time zone,
                add column retry_after timestamp with time zone,
                add column download_token character varying(32)", Context),
            z_db:q("update filestore set error_since = modified where error is not null", Context),
            z_db:flush(Context)
    end,
    [] = z_db:q("create table if not exists filestore_read_stats (
        minute timestamp with time zone primary key,
        attempts bigint not null default 0,
        failures bigint not null default 0)", Context),
    [] = z_db:q("create index if not exists filestore_error_since
        on filestore(error_since) where error is not null and not is_deleted", Context),
    ok.

install_unrecoverable(Context) ->
    case lists:member(is_unrecoverable, z_db:column_names(filestore, Context)) of
        true -> ok;
        false ->
            [] = z_db:q("alter table filestore
                add column is_unrecoverable boolean not null default false", Context),
            z_db:flush(Context)
    end.
