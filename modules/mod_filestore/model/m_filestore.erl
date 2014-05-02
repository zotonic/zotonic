%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Models for file-storage administration

%% Copyright 2014 Marc Worrell
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
    m_find_value/3,

    queue/3,
    fetch_queue/1,
    dequeue/2,
    mark_error/3,
    mark_deleted/2,
    fetch_deleted/1,
    purge_deleted/2,

    mark_move_to_local/1,
    mark_move_to_local/2,
    unmark_move_to_local/1,
    unmark_move_to_local/2,
    fetch_move_to_local/1,
    purge_move_to_local/2,

    store/5,
    lookup/2,

    stats/1,

    install/2
    ]).

-include_lib("zotonic.hrl").

m_find_value(stats, #m{}, Context) ->
    stats(Context).


queue(Path, Props, Context) ->
    z_db:transaction(fun(Ctx) ->
            case z_db:q1("select count(*) from filestore_queue where path = $1", [Path], Ctx) of
                0 ->
                    1 = z_db:q("insert into filestore_queue (path, props) values ($1,$2)", [Path, ?DB_PROPS(Props)], Ctx),
                    ok;
                1 ->
                    {error, duplicate}
            end
        end, Context).

%% @doc Fetch the next batch of queued uploads, at least 10 minutes old.
fetch_queue(Context) ->
    z_db:assoc("select * 
                from filestore_queue
                where created < now() - interval '10 min'
                limit 200", Context).

dequeue(Id, Context) ->
    z_db:q("delete from filestore_queue where id = $1", [Id], Context).

store(Path, Size, Service, Location, Context) when is_binary(Path), is_integer(Size), is_binary(Location) ->
    z_db:transaction(fun(Ctx) ->
            case z_db:q1("select id from filestore where path=$1", [Path], Ctx) of
                undefined ->
                    z_db:insert(filestore, [
                            {path, Path},
                            {service, Service},
                            {location, Location},
                            {size, Size}
                        ], Ctx);
                Id ->
                    1 = z_db:q("update filestore
                            set location = $1,
                                service = $2,
                                size = $3,
                                is_deleted = false,
                                is_move_to_local = false,
                                error = null,
                                modified = now()
                            where id = $4", 
                            [Location, Service, Size, Id],
                            Ctx),
                    {ok, Id}
            end
        end, Context).

lookup(Path, Context) ->
    z_db:assoc_row("select *
                    from filestore 
                    where path = $1 
                      and error is null
                      and not is_deleted",
                   [Path], Context).

mark_error(Id, Error, Context) ->
    z_db:q("update filestore
            set error = $1,
                modified = now()
            where id = $2",
          [Error, Id], Context).

mark_deleted({prefix, Path}, Context) when is_binary(Path) ->
    z_db:q("update filestore
            set is_deleted = true,
                modified = now()
            where path like $1",
            [<<Path/binary, $%>>],
            Context);
mark_deleted(Path, Context) when is_binary(Path) ->
    z_db:q("update filestore
            set is_deleted = true,
                modified = now()
            where path = $1",
            [Path],
            Context).

fetch_deleted(Context) ->
    z_db:assoc("select * from filestore where is_deleted limit 200", Context).

purge_deleted(Id, Context) ->
    z_db:q("delete from filestore where id = $1 and is_deleted", [Id], Context).

mark_move_to_local(Context) ->
    z_db:q("update filestore
            set is_move_to_local = true
            where not is_move_to_local
              and not is_deleted
              and error is null", Context).

mark_move_to_local(Id, Context) ->
    z_db:q("update filestore
            set is_move_to_local = true
            where id = $1", [Id], Context).

unmark_move_to_local(Context) ->
    z_db:q("update filestore
            set is_move_to_local = false
            where is_move_to_local", Context).

unmark_move_to_local(Id, Context) ->
    z_db:q("update filestore
            set is_move_to_local = false
            where id = $1", [Id], Context).

fetch_move_to_local(Context) ->
    z_db:assoc("select * from filestore where is_move_to_local and not is_deleted and error is null limit 200", Context).

purge_move_to_local(Id, Context) ->
    z_db:q("update filestore
            set is_move_to_local = false,
                is_deleted = true
            where id = $1", [Id], Context).

stats(Context) ->
    {Archived, ArchiveSize} = z_db:q_row("
                            select count(*), sum(size) 
                            from medium
                            where filename is not null
                              and filename <> ''
                              and is_deletable_file", Context),
    Queued = z_db:q1("select count(*) from filestore_queue", Context),
    {Cloud, CloudSize, ToLocal, Deleted} = z_db:q_row("
                            select count(*), sum(size), 
                                   sum(is_move_to_local::integer), sum(is_deleted::integer)
                            from filestore", Context),

    % TODO: we need a separate index for this lookup
    {InCloud, InCloudSize} = z_db:q_row("
                            select count(*), coalesce(sum(m.size), 0)
                            from medium m
                                    join filestore f
                                    on f.path = 'archive/' || m.filename
                            where filename is not null
                              and filename <> ''
                              and is_deletable_file", Context),
    [
        {archived, Archived},
        {archive_size, ArchiveSize},
        {queued, Queued},
        {queued_local, ToLocal},
        {queued_deleted, Deleted},
        {cloud, Cloud},
        {cloud_size, CloudSize},
        {local, Archived - InCloud},
        {local_size, ArchiveSize - InCloudSize}
    ].



install(install, Context) ->
    ok = install_filestore(Context),
    ok = install_filequeue(Context).

install_filestore(Context) ->
    case z_db:table_exists(filestore, Context) of
        false ->
            [] = z_db:q("
                create table filestore (
                    id serial not null,
                    is_deleted boolean not null default false,
                    is_move_to_local boolean not null default false,
                    error character varying(32),
                    path character varying(255) not null,
                    service character varying(16) not null,
                    location character varying(400) not null,
                    size int not null default 0,
                    modified timestamp with time zone not null default now(),
                    created timestamp with time zone not null default now(),
                    
                    constraint filestore_pkey primary key (id),
                    constraint filestore_path_key unique (path),
                    constraint filestore_location_key unique (location)
                )
                ", Context),
            {ok, _, _} = z_db:equery("create index filestore_is_deleted_key on filestore(is_deleted) where is_deleted", Context),
            {ok, _, _} = z_db:equery("create index filestore_is_move_to_local_key on filestore(is_move_to_local) where is_move_to_local", Context),
            z_db:flush(Context), 
            ok;
        true ->
            ok
    end.

install_filequeue(Context) ->
    case z_db:table_exists(filestore_queue, Context) of
        false ->
            [] = z_db:q("
                create table filestore_queue (
                    id serial not null,
                    path character varying(255) not null,
                    props bytea,
                    created timestamp with time zone not null default now(),
                    
                    constraint filestore_queue_pkey primary key (id),
                    constraint filestore_queue_path_key unique (path)
                )
                ", Context),
            z_db:flush(Context), 
            ok;
        true ->
            ok
    end.
