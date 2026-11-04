%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Preview provenance and lazy retirement. Usage is separate from content
%% identity: different resources and URLs can share the same derivative bytes.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_cafs_preview).
-export([install/1, schema/0, fingerprint/2, prepare/5, bind/5,
    lookup/2, expired/2, maintenance/1, grace_days/1]).

-include_lib("zotonic.hrl").

-spec schema() -> [string()].
schema() -> [
    "create table if not exists cafs_preview_state (
        id boolean primary key default true check (id),
        legacy_since timestamptz not null default now()
    )",
    "insert into cafs_preview_state (id) values (true) on conflict do nothing",
    "create table if not exists cafs_preview_usage (
        request_key varchar(64) primary key,
        path text not null,
        filename varchar(400) not null,
        rsc_id integer not null,
        source_hash varchar(64),
        operations bytea,
        renderer_version bytea,
        mediaclass text,
        mediaclass_checksum text,
        edit_revision varchar(64),
        created timestamptz not null default now(),
        checked_at timestamptz not null default '-infinity',
        obsolete_since timestamptz
    )",
    "create index if not exists cafs_preview_usage_derivative
        on cafs_preview_usage(source_hash,operations,renderer_version)",
    "create index if not exists cafs_preview_usage_check on cafs_preview_usage(checked_at)",
    "create index if not exists cafs_preview_usage_resource on cafs_preview_usage(rsc_id)"
].

-spec install(Context) -> ok when Context :: z:context().
install(Context) ->
    %% Upgrade only existing older tables; fresh CAFS definitions include these.
    lists:foreach(fun(Column) ->
        case z_db:column(cafs_derivative, Column, Context) of
            {ok, _} -> ok;
            {error, enoent} ->
                [] = z_db:q("alter table cafs_derivative add column " ++ atom_to_list(Column)
                    ++ " timestamptz not null default now()", Context),
                z_db:flush(Context)
        end
    end, [created, checked_at]),
    lists:foreach(fun(Sql) -> _ = z_db:q(Sql, Context) end, schema()),
    [] = z_db:q("create index if not exists cafs_derivative_check on cafs_derivative(checked_at)", Context),
    ok.

-spec fingerprint(Id, Context) -> binary()
    when Id :: integer() | undefined, Context :: z:context().
fingerprint(undefined, _Context) -> <<"original">>;
fingerprint(Id, Context) ->
    Settings = z_notifier:foldl(#media_preview_revision{id=Id}, [], Context),
    digest(term_to_binary(lists:sort(Settings), [deterministic])).

-spec grace_days(Context) -> pos_integer() when Context :: z:context().
grace_days(Context) ->
    case catch z_convert:to_integer(m_config:get_value(site, cafs_preview_retention_days, 90, Context)) of
        Days when is_integer(Days), Days >= 1, Days =< 3650 -> Days;
        _ -> 90
    end.

%% Called only after URL signature validation. Never renew an obsolete deadline
%% on access. Rows survive eviction as small tombstones against stale URL replay.
-spec prepare(Path, Filename, Filters, Medium, Context) -> {ok, binary()} | {error, enoent}
    when Path :: binary(), Filename :: binary(), Filters :: list(),
         Medium :: map(), Context :: z:context().
prepare(Path, Filename, Filters, #{<<"id">> := Id}, Context) ->
    Key = digest(Path),
    {Class, Checksum} = case proplists:get_value(mediaclass, Filters) of
        {Name, Sum} -> {z_convert:to_binary(Name), z_convert:to_binary(Sum)};
        _ -> {undefined, undefined}
    end,
    %% Unversioned URLs cannot prove a past edit change. Record the first
    %% observed revision and retain them until a subsequent change is known.
    Revision = proplists:get_value(editrev, Filters, fingerprint(Id, Context)),
    z_db:q("insert into cafs_preview_usage
        (request_key,path,filename,rsc_id,mediaclass,mediaclass_checksum,edit_revision)
        values ($1,$2,$3,$4,$5,$6,$7)
        on conflict do nothing", [Key,Path,Filename,Id,Class,Checksum,Revision], Context),
    refresh(Key, Context),
    case expired_key(Key, Context) of
        true -> {error, enoent};
        false -> {ok, Key}
    end.

%% The caller holds the source CAFS row lock, shared with generation and GC.
-spec bind(Key, Hash, Operations, Version, Context) -> ok | {error, enoent}
    when Key :: binary() | undefined, Hash :: binary(), Operations :: binary(),
         Version :: binary(), Context :: z:context().
bind(undefined, _Hash, _Operations, _Version, _Context) -> ok;
bind(Key, Hash, Operations, Version, Context) ->
    case expired_key(Key, Context) of
        true -> {error, enoent};
        false ->
            z_db:q("update cafs_preview_usage set source_hash=$2,operations=$3,renderer_version=$4
                where request_key=$1", [Key,Hash,Operations,Version], Context),
            ok
    end.

%% Allows an old, still-retained URL to use its existing output even when the old
%% mediaclass checksum is no longer present in ETS. It cannot generate new bytes.
-spec lookup(Key, Context) -> {ok, binary()} | {error, enoent}
    when Key :: binary(), Context :: z:context().
lookup(Key, Context) ->
    case z_db:q1("select d.output_hash from cafs_preview_usage u join cafs_derivative d
        on d.source_hash=u.source_hash and d.operations=u.operations and d.renderer_version=u.renderer_version
        where u.request_key=$1 and d.output_hash is not null
        and u.obsolete_since is not null
        and u.obsolete_since>now()-($2 * interval '1 day')", [Key,grace_days(Context)], Context) of
        undefined -> {error, enoent};
        Hash -> {ok, Hash}
    end.

-spec expired(Path, Context) -> boolean() when Path :: binary(), Context :: z:context().
expired(Path, Context) ->
    m_cafs:is_enabled(Context) andalso
        binary:match(Path, <<"(">>) =/= nomatch andalso
        expired_key(digest(Path), Context).

expired_key(Key, Context) ->
    z_db:q1("select obsolete_since<=now()-($2 * interval '1 day')
        from cafs_preview_usage where request_key=$1", [Key,grace_days(Context)], Context) =:= true.

refresh(Key, Context) ->
    case z_db:qmap_row("select * from cafs_preview_usage where request_key=$1", [Key], Context) of
        {ok, Usage0} ->
            Usage = legacy_revision(Usage0, Context),
            case is_current(Usage, Context) of
                true ->
                    z_db:q("update cafs_preview_usage set obsolete_since=null,checked_at=now()
                        where request_key=$1", [Key], Context);
                false ->
                    z_db:q("update cafs_preview_usage set obsolete_since=coalesce(obsolete_since,now()),
                        checked_at=now() where request_key=$1", [Key], Context)
            end;
        _ -> ok
    end.

%% Upgrade old usage rows lazily. The former blanket upgrade deadline is not
%% evidence of obsolescence; normal source/class/revision checking replaces it.
legacy_revision(#{<<"edit_revision">> := undefined, <<"rsc_id">> := Id,
        <<"request_key">> := Key} = Usage, Context) ->
    Revision = fingerprint(Id, Context),
    z_db:q("update cafs_preview_usage set edit_revision=$2 where request_key=$1 and edit_revision is null",
        [Key, Revision], Context),
    Usage#{<<"edit_revision">> => Revision};
legacy_revision(Usage, _Context) -> Usage.

is_current(#{<<"rsc_id">> := Id, <<"filename">> := Filename, <<"edit_revision">> := Revision} = Usage, Context) ->
    %% Use the current medium, not its retained recovery mapping.
    IsSource = z_db:q1("select exists(select 1 from medium
        where id=$1 and (filename=$2 or preview_filename=$2))", [Id,Filename], Context),
    IsSource andalso current_class(Usage, Context) andalso current_edits(Id, Revision, Context).

current_class(#{<<"mediaclass">> := undefined}, _Context) -> true;
current_class(#{<<"mediaclass">> := Name, <<"mediaclass_checksum">> := Checksum}, Context) ->
    case z_mediaclass:get(Name, Context) of
        {ok, _, Checksum} when Checksum =/= <<>> -> true;
        _ -> false
    end.

current_edits(_Id, undefined, _Context) -> false;
current_edits(_Id, <<"original">>, _Context) -> true;
current_edits(Id, Revision, Context) -> fingerprint(Id, Context) =:= Revision.

-spec maintenance(Context) -> ok when Context :: z:context().
maintenance(Context) ->
    case m_cafs:is_enabled(Context) of
        false -> ok;
        true -> maintenance_db(Context)
    end.

maintenance_db(Context) ->
    %% Ordered checking rotates through the whole registry without resource/file
    %% scans. Batches do not fetch originals or remote objects.
    lists:foreach(fun({Key, Path}) ->
        refresh(Key, Context),
        case expired_key(Key, Context) of
            true ->
                z_file_request:force_stale(Path, Context),
                %% Retry on subsequent passes; do not erase the URL tombstone.
                z_media_cleanup_server:delete_preview(Path, Context);
            false -> ok
        end
    end, z_db:q("select request_key,path from cafs_preview_usage order by checked_at limit 100", Context)),
    lists:foreach(fun({Hash, Ops, Version}) ->
        collect(Hash, Ops, Version, Context)
    end, z_db:q("select source_hash,operations,renderer_version from cafs_derivative
        order by checked_at limit 100", Context)),
    ok.

collect(Hash, Ops, Version, Context) ->
    z_db:transaction(fun(Ctx) ->
        z_db:q1("select hash from cafs where hash=$1 for update", [Hash], Ctx),
        Args = [Hash,Ops,Version,grace_days(Ctx)],
        %% Sources with active generators/readers are pinned. Check both ends:
        %% the output might currently be read through another derivative.
        Removed = z_db:q("delete from cafs_derivative d where source_hash=$1
            and operations=$2 and renderer_version=$3
            and created<now()-($4 * interval '1 day')
            and (claim_until is null or claim_until<now())
            and not exists(select 1 from cafs_pin p where p.hash in (d.source_hash,d.output_hash)
                and (p.expires is null or p.expires>now()))
            and not exists(select 1 from cafs_preview_usage u
                where u.source_hash=d.source_hash and u.operations=d.operations and u.renderer_version=d.renderer_version
                and (u.obsolete_since is null or u.obsolete_since>now()-($4 * interval '1 day')))
            returning output_hash", Args, Ctx),
        lists:foreach(fun({undefined}) -> ok; ({Output}) ->
            z_db:q("update cafs set gc_after=least(gc_after,now()) where hash=$1", [Output], Ctx)
        end, Removed),
        z_db:q("update cafs_derivative set checked_at=now()
            where source_hash=$1 and operations=$2 and renderer_version=$3", [Hash,Ops,Version], Ctx)
    end, Context),
    ok.

digest(Data) -> binary:encode_hex(crypto:hash(sha256, Data), lowercase).
