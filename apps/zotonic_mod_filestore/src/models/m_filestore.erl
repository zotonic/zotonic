%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2022 Marc Worrell
%% @doc Models for file-storage administration
%% @end

%% Copyright 2014-2022 Marc Worrell
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

-export([
    m_get/3,

    is_local_keep/1,

    queue/3,
    fetch_queue/1,
    dequeue/2,
    mark_error/3,
    mark_deleted/2,
    fetch_deleted/2,
    purge_deleted/2,

    mark_move_to_local_all/1,
    mark_move_to_local_limit/2,
    mark_move_to_local/2,

    unmark_move_to_local_all/1,
    unmark_move_to_local_limit/2,
    unmark_move_to_local/2,

    fetch_move_to_local/1,
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

% It is ok to retry transient errors every 10 minutes
-define(RETRY_TRANSIENT_ERRORS, 600).

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"stats">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {stats(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"s3url">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {m_config:get_value(mod_filestore, s3url, Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"s3key">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {m_config:get_value(mod_filestore, s3key, Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"s3secret">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {m_config:get_value(mod_filestore, s3secret, Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"is_upload_enabled">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {m_config:get_boolean(mod_filestore, is_upload_enabled, Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"is_local_keep">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true -> {ok, {m_config:get_boolean(mod_filestore, is_local_keep, Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"delete_interval">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            Interval = case m_config:get_value(mod_filestore, delete_interval, Context) of
                undefined -> <<"0">>;
                <<>> -> <<"0">>;
                Interv -> Interv
            end,
            {ok, {Interval, Rest}};
        false -> {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

-spec is_local_keep(z:context()) -> boolean().
is_local_keep(Context) ->
    m_config:get_boolean(mod_filestore, is_local_keep, Context).

%% @doc Add a file/medium to the upload queue.
-spec queue(binary(), z_media_identify:media_info(), z:context()) -> ok | {error, duplicate}.
queue(Path, MediaProps, Context) ->
    z_db:transaction(fun(Ctx) ->
            case z_db:q1("select count(*) from filestore_queue where path = $1", [Path], Ctx) of
                0 ->
                    1 = z_db:q("
                            insert into
                            filestore_queue (path, props)
                            values ($1,$2)",
                            [Path, ?DB_PROPS(MediaProps)],
                            Ctx),
                    ok;
                1 ->
                    {error, duplicate}
            end
        end,
        Context).

%% @doc Fetch the next batch of queued uploads, at least 1 minute old and max 200.
-spec fetch_queue( z:context() ) -> {ok, [ queue_entry() ]} | {error, term()}.
fetch_queue(Context) ->
    case z_db:qmap("
                select *
                from filestore_queue
                where created < now() - interval '1 min'
                limit 200",
                [],
                [ {keys, atom} ],
                Context)
    of
        {ok, Rs} ->
            % 0.x filestore queues can have property lists in the queue, change those
            % property lists to maps.
            Rs1 = lists:map(
                fun
                    (#{ props := L } = M) when is_list(L) ->
                        M#{
                            props => maps:from_list(L)
                        };
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

-spec store( binary(), integer(), atom() | binary(), binary(), boolean(), z:context() ) -> {ok, integer()}.
store(Path, Size, Service, Location, IsLocal, Context)
    when is_binary(Path), is_integer(Size), is_binary(Location) ->
    z_db:transaction(fun(Ctx) ->
            case z_db:q1("select id from filestore where path = $1",
                        [Path], Ctx)
            of
                undefined ->
                    z_db:insert(filestore, #{
                            <<"path">> => Path,
                            <<"service">> => Service,
                            <<"location">> => Location,
                            <<"size">> => Size,
                            <<"is_local">> => IsLocal
                        }, Ctx);
                Id ->
                    1 = z_db:q("
                            update filestore
                            set location = $1,
                                service = $2,
                                size = $3,
                                is_deleted = false,
                                is_local = $5,
                                is_move_to_local = false,
                                error = null,
                                modified = now()
                            where id = $4",
                            [ Location, Service, Size, Id, IsLocal ],
                            Ctx),
                    {ok, Id}
            end
        end, Context).

-spec lookup( binary(), z:context() ) -> {ok, map()} | {error, term()}.
lookup(Path, Context) ->
    z_db:qmap_row("
        select *
        from filestore
        where path = $1
          and error is null
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
is_download_ok(#{ error := Error, modified := Modified }) ->
    is_download_ok(Error, Modified).

is_download_ok(undefined, _Modified) ->
    true;
is_download_ok(_Error, Modified) ->
    % Typical error when S3 can't find anything
    % or when the credentials were wrong for some time
    MTimestamp = z_datetime:datetime_to_timestamp(Modified),
    Now = z_datetime:timestamp(),
    case Now - MTimestamp of
        Delta when Delta > ?RETRY_TRANSIENT_ERRORS ->
            true;
        _ ->
            false
    end.

%% @doc Mark a filestore entry as being erronous, happens when the
%% remote server can't be reached or gives an error on access.
%% This can be a transient error, so we mark the entry with the
%% current time for a later retry.
-spec mark_error( integer(), binary()|atom(), z:context() ) -> ok | {error, enoent}.
mark_error(Id, Error, Context) ->
    case z_db:q("update filestore
            set error = $1,
                modified = now()
            where id = $2",
          [Error, Id], Context)
    of
        1 -> ok;
        0 -> {error, enoent}
    end.

%% @doc Mark the file entries as deleted for the path or having the path as a prefix.
-spec mark_deleted( binary() | {prefix, binary()}, z:context() ) -> ok | {error, enoent}.
mark_deleted({prefix, Path}, Context) when is_binary(Path) ->
    case z_db:q("update filestore
            set is_deleted = true,
                modified = now(),
                deleted = now()
            where path like $1",
            [<<Path/binary, $%>>],
            Context)
    of
        N when N > 0 -> ok;
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
        N when N > 0 -> ok;
        0 -> {error, enoent}
    end.

%% @doc Fetch all deleted file entries where the entry was marked as deleted
%% at least 'Interval' ago. The Interval comes from the mod_filestore.delete_interval
%% configuration.
-spec fetch_deleted( binary() | undefined, z:context() ) -> {ok, [ filestore_entry() ]} | {error, term()}.
fetch_deleted(Interval, Context) ->
    case map_interval(Interval) of
        <<"false">> ->
            [];
        <<"0">> ->
            z_db:qmap(
                "select * from filestore where is_deleted = true limit 200",
                [],
                [ {keys, atom} ],
                Context);
        Interval1 ->
            z_db:qmap(<<
                "select * ",
                "from filestore ",
                "where is_deleted =true "
                "  and deleted < now() - interval '", Interval1/binary,"' ",
                "limit 200">>,
                [],
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

%% @doc Mark at most Limit entries to be moved from the remote service
%% to the local service. We mark all entries so that any missing files are
%% recovered.
-spec mark_move_to_local_limit( non_neg_integer(), z:context() ) -> {ok, non_neg_integer()}.
mark_move_to_local_limit(Limit, Context) ->
    z_db:transaction(fun(Ctx) ->
                        N = z_db:q("
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
    z_db:q("update filestore
            set is_move_to_local = true
            where is_move_to_local = false
              and is_deleted = false
              and error is null", Context).

%% @doc Mark the given filestore entry to be moved from the remote service
%% to the local service.
-spec mark_move_to_local( integer(), z:context() ) -> ok | {error, enoent}.
mark_move_to_local(Id, Context) ->
    case z_db:q("
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
    z_db:q("update filestore
            set is_move_to_local = false
            where is_move_to_local", Context).


%% @doc Remove the "move to local" mark from the given filestore entry.
-spec unmark_move_to_local( integer(), z:context() ) -> ok | {error, enoent}.
unmark_move_to_local(Id, Context) ->
    case z_db:q("
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
    z_db:qmap("
            select *
            from filestore
            where is_move_to_local
              and is_deleted = false
              and error is null
            limit 200",
            [],
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
    z_db:q("update filestore
            set is_move_to_local = false,
                is_local = true
            where id = $1", [Id], Context);
purge_move_to_local(Id, false, Context) ->
    z_db:q("update filestore
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
    ok = ensure_size_bigint(Context).

install_filestore(Context) ->
    case z_db:table_exists(filestore, Context) of
        false ->
            [] = z_db:q("
                create table filestore (
                    id serial not null,
                    is_deleted boolean not null default false,
                    is_local boolean not null default false,
                    is_move_to_local boolean not null default false,
                    error character varying(32),
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
        {ok, #column_def{ type = "integer" }} ->
            z_db:q("alter table filestore alter column size type bigint", Context),
            z_db:flush(Context);
        {ok, _} ->
            ok
    end.
