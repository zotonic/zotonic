%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Site-local content-addressable media storage. Internal API only: callers
%% authorize resources; hashes never confer access. Immutable objects live below
%% archive/cafs so existing archive backups include their bytes.
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

-module(m_cafs).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "model", "file_storage",
        "media_management", "image_management"
    ]
}).
-moduledoc("""
Site-local content-addressable file storage (CAFS) for originals, explicit previews
such as video posters, and generated image variants. Identical bytes share one
SHA-256 object, while resources keep their own logical filenames and permissions.

## Interface and permissions

This is an internal Erlang API used by `m_media`, file resolution, filestore, and
background processing. It has no template, HTTP model, or MQTT model paths.
Functions do not enforce resource ACLs: callers must authorize the resource and
operation before resolving or changing storage. Knowing a hash never grants access.
Use `m_media` for ordinary uploads, replacement, duplication, and deletion.

For example, after authorizing access to a medium's logical filename:

```erlang
case m_cafs:lookup(Filename, Context) of
    {ok, Hash} ->
        m_cafs:with_local_file(Hash, fun file:read_file/1, Context);
    {error, enoent} ->
        {error, legacy_or_missing}
end.
```

`lookup/2` returns `{ok, Hash}` or `{error, enoent}`. An absent mapping does not
prove the file is missing: it may still use legacy storage. `with_local_file/3`
resolves local or remote storage, pins the object, and passes a temporary readable
filename to the callback. It returns the callback result or a storage error.
The callback runs in a monitored worker; its filename is valid only during the
callback and must not be modified or retained for later use.

## Storage and attachment

Objects live at `archive/cafs/sha256/ab/cd/<hash>`, relative to the site's files
directory. Hashes are 64 lowercase hexadecimal bytes; the directory components
are their first four characters. The path has no filename extension. `path/1`
constructs this relative path; `path_hash/1` validates and extracts its hash.
Filestore uses the same physical key and can move the bytes to remote storage.

`filename_cafs` maps logical filenames to objects in `cafs`. These mappings are
independent of physical storage: cloning a mapped filename requires no file read
or remote download. Existing files are promoted only when their bytes are already
available for an operation; installation never downloads or hashes all old files.

Database-free sites (`dbdatabase = none`) do not use CAFS. `is_enabled/1` checks
configuration, not pool availability, so a database outage never enables a legacy
storage fallback. On these sites, `lookup/2` returns `{error, enoent}`,
`stage_file/2` returns `{error, nodb}`, and installation/maintenance are no-ops.
Library previews use the local preview directory without provenance tracking or GC.

`stage_file/2` hashes and atomically publishes a local file, returning
`{ok, #{hash => Hash, token => Token}}`. Do this outside the resource transaction.
The token holds a one-day staging pin. `attach/3` attaches a unique logical filename
and consumes that pin; use the resource transaction's context to commit the medium
and mapping together. `stage_medium/2` and `attach_medium/2` perform this handoff
for owned originals and posters. `stage_medium/2` returns `{ok, Staged}` or a
controlled error and releases partial staging pins on failure. `release/2` releases remaining staging pins from
a list of `{Filename, Stage}` pairs, including after failure.

`stage_medium/3` accepts the logical filename of a fresh private archive copy
created by the upload routine. Only that original uses a single hash pass over
its staging copy; the medium digest reuses this hash. The default staging APIs,
legacy migration, extracted previews and observer-supplied replacement filenames
hash both source and copy to detect changes while copying. Publication always
uses the hash of the staged bytes, with an atomic rename on the CAFS filesystem.

Attachment can return `{error, expired}` for an expired staging token or
`{error, deleting}` while dispatched physical deletion is pending. Staging an
identical file can cancel an undispatched deletion using the filestore row fence;
it never revives an object that an in-flight remote delete could remove. Pending
collection and failed deletion are rescheduled so they do not starve other objects.
A filename already mapped
to different bytes is a conflict. `clone_filename/3` shares an existing mapping;
`promote/3` maps an available legacy file and returns `{ok, Hash}`. It adopts an
existing remote registry entry before allowing a canonical upload, and queues
retryable cleanup of redundant legacy paths. `stage_original/2` and
`attach_original/3` provide that same handoff for maintenance migration: newly
staged originals are not uploadable until remote adoption has been checked.
Busy or unavailable backends defer adoption and cleanup; they do not trigger a
second upload. Remote retirement follows the backend retention policy. Cleanup
is queued in the publication transaction and defers local unlinking while legacy
readers remain. With local retention disabled, the backend queues adopted objects
for local offloading without another upload; active pins and readers defer it.

Requests for owned non-CAFS originals schedule background promotion through
`z_media_cafs_migrate`. Local bytes or the completed, locked download cache are
used without starting another download or delaying the response for hashing.
Attachment rechecks the medium under its row lock and records the original digest;
replaced/deleted media and unowned files are not attached. Failed or overloaded
attempts leave the legacy file usable, and successful attempts queue durable cleanup.

## Shared previews and retention

On a miss for a revision-bearing URL, a current source, mediaclass and edit
revision may reuse the legacy URL for the exact same rendering operations. Only
`editrev` is removed; dimensions, operation order, class checksum and output
extension stay unchanged, and the legacy signature is reconstructed. Existing
local or remote preview bytes are promoted without running the resizer when the
source hash is available. Unknown remote originals are not downloaded for this.
Background cleanup adopts the remote preview and removes redundant legacy copies;
new local outputs are uploaded only after remote absence is confirmed. Registered
current URLs resolve directly to their CAFS output without legacy path probes or
usage writes, while checking the current source, edits, class and renderer version.


`derivative/5` keys generation by source hash, ordered effective operations, and
renderer version. Its zero-argument generator returns a staged output from
`stage_file/2`. The result is `{ok, OutputHash}` or an error; concurrent callers
share a generation claim. Errors include `timeout`, `stale_claim`, and generator
errors. Include every output-affecting input in the key; do not sort operations.

`derivative/6` additionally binds a validated preview usage key from
`z_cafs_preview`. An expired usage returns `{error, enoent}`. Usage provenance is
separate from rendering identity, so resources with different histories can still
share identical output. Generated previews are retired lazily after source removal,
mediaclass changes, or image-edit changes. Requests do not renew an obsolete URL's
deadline. Reverting settings can make a usage current again. Unversioned legacy
URLs retain their first observed edit revision; the upgrade timestamp alone never
expires them. Expired usages also delete their exact legacy preview paths, with
retry on failure. Rendering identity includes the actual metadata snapshot used
by the converter, excluding resource IDs.

Medium deletion retains filename mappings for the existing five-week recovery
period. `retire_filename/2` is a cleanup operation called after that period, not a
replacement for deleting a resource. `maintenance/1` performs bounded reconciliation,
preview retirement, upload queuing, and collection. Other filenames, derivatives,
and pins preserve shared bytes. Remote deletion honors filestore retention and
keeps a tombstone until deletion is confirmed.

`pin/3` creates a durable processing reference owned by a caller-supplied token;
the caller must balance it with `unpin/3`, including cancellation and failure.
`with_path_pin/3` scopes a renewable pin to a callback for CAFS physical paths.
`track/2` and `readers/2` manage file-entry cache subscriptions, not retention pins.

## Site configuration

The core installer creates and upgrades the schema; no CAFS module needs activation.
Remote storage requires the usual `mod_filestore` configuration.

| `m_config` key | Default | Meaning |
| --- | --- | --- |
| `site.cafs_preview_retention_days` | `90` | Obsolete-preview grace period, in days. Integers from 1 to 3650 are accepted; other values use the default. |
| `site.cafs_renderer_version` | `1` | Additional generation-version value used by image preview resolution. Change it after rendering changes not represented by the operations or renderer identity. |

After changing the renderer version, call `z_file_sup:refresh/0` to invalidate
running file entries. These are per-site `m_config` settings, not system settings.
They are listed in the admin configuration catalog by `mod_base`, under the `site`
namespace. For example, to set a 180-day preview grace period:

```erlang
m_config:set_value(site, cafs_preview_retention_days, 180, Context).
```

`queue_all/1` marks every ready CAFS object for upload, including generated variants
and durable task inputs. `queue_uploads/1` submits a bounded batch to filestore.
Neither function discovers or downloads legacy files.
""").

-export([delete_uploaded_file/2, is_enabled/1, install/1, schema/0, stage_file/2, stage_original/2, stage_remote/4, attach/3, attach_original/3, lookup/2,
    clone_filename/3, promote/3, stage_medium/2, stage_medium/3, attach_medium/2,
    release/2, pin/3, unpin/3, with_local_file/3, with_path_pin/3,
    path/1, path_hash/1, aliases/2, retire_filename/2, maintenance/1,
    queue_uploads/1, queue_all/1, collect_unused/2, derivative/5, derivative/6, track/2, readers/2]).

-include_lib("zotonic.hrl").
-include_lib("zotonic_file.hrl").

%% Configuration, not current pool availability: a database outage must never
%% silently change the storage model of a database-backed site.
-spec is_enabled(Context) -> boolean() when Context :: z:context().
is_enabled(Context) ->
    case m_site:get(dbdatabase, Context) of
        none -> false;
        <<"none">> -> false;
        "none" -> false;
        _ -> true
    end.

%% No model callbacks: this API is not exposed through MQTT or templates.
-spec install(Context) -> ok when Context :: z:context().
install(Context) ->
    case is_enabled(Context) of
        false -> ok;
        true -> install_db(Context)
    end.

install_db(Context) ->
    lists:foreach(fun(Sql) -> [] = z_db:q(Sql, Context) end, schema()),
    z_db:flush(Context),
    z_cafs_preview:install(Context).

-spec schema() -> [string()].
schema() -> [
    "create table if not exists cafs (
        hash varchar(64) primary key check (hash ~ '^[0-9a-f]{64}$'),
        size bigint not null check (size >= 0),
        mime varchar(128),
        props bytea,
        metadata_version integer not null default 1,
        state varchar(16) not null default 'ready' check (state in ('ready','deleting')),
        upload_pending boolean not null default true,
        created timestamptz not null default now(),
        modified timestamptz not null default now(),
        gc_after timestamptz not null default now() + interval '5 weeks'
    )",
    "create table if not exists filename_cafs (
        filename varchar(400) primary key,
        hash varchar(64) not null references cafs(hash),
        created timestamptz not null default now(),
        retired_at timestamptz
    )",
    "create index if not exists filename_cafs_hash on filename_cafs(hash)",
    "create index if not exists medium_upload_digest on medium(digest, size)
        where is_deletable_file and digest is not null",
    "create table if not exists cafs_pin (
        owner varchar(200) not null,
        hash varchar(64) not null references cafs(hash),
        expires timestamptz,
        primary key (owner, hash)
    )",
    "create index if not exists cafs_pin_hash on cafs_pin(hash)",
    "create table if not exists cafs_derivative (
        source_hash varchar(64) not null references cafs(hash),
        operations bytea not null,
        renderer_version bytea not null,
        output_hash varchar(64) references cafs(hash),
        claim_token varchar(64),
        claim_until timestamptz,
        created timestamptz not null default now(),
        checked_at timestamptz not null default now(),
        primary key (source_hash, operations, renderer_version)
    )",
    "create index if not exists cafs_derivative_output on cafs_derivative(output_hash)",
    "create index if not exists cafs_gc on cafs(gc_after)",
    %% Compare catalog definitions before DDL: unchanged installs must not rewrite
    %% pg_proc or take the table lock required to replace the medium trigger.
    "do $install$
    declare
        body text := $body$
    begin
        if tg_op = 'DELETE' then
            update filename_cafs set retired_at = now()
            where filename in (old.filename, old.preview_filename);
        elsif tg_op = 'UPDATE' then
            update filename_cafs set retired_at = now()
            where (filename = old.filename and old.filename is distinct from new.filename)
               or (filename = old.preview_filename and old.preview_filename is distinct from new.preview_filename);
        end if;
        if tg_op <> 'DELETE' then
            perform c.hash from cafs c join filename_cafs f on f.hash=c.hash
            where f.filename in (new.filename,new.preview_filename)
            order by c.hash for update of c;
            update filename_cafs set retired_at = null
            where filename in (new.filename, new.preview_filename);
        end if;
        return null;
    end; $body$;
    begin
        if not exists (
            select 1 from pg_proc p join pg_language l on l.oid = p.prolang
            where p.oid = to_regprocedure('cafs_medium_retire()')
              and p.prosrc = body and l.lanname = 'plpgsql'
              and p.prorettype = 'trigger'::regtype
        ) then
            execute 'create or replace function cafs_medium_retire() returns trigger as '
                || quote_literal(body) || ' language plpgsql';
        end if;
        if not exists (
            select 1 from pg_trigger
            where tgrelid = 'medium'::regclass and tgname = 'cafs_medium_retire'
              and tgfoid = to_regprocedure('cafs_medium_retire()')
              and tgtype = 29 -- AFTER ROW INSERT, UPDATE, DELETE
              and tgenabled = 'O' and not tgisinternal
              and tgnargs = 0 and tgattr = ''::int2vector and tgqual is null
              and tgconstraint = 0
        ) then
            drop trigger if exists cafs_medium_retire on medium;
            create trigger cafs_medium_retire after insert or update or delete on medium
                for each row execute procedure cafs_medium_retire();
        end if;
    end;
    $install$"
].

-spec path(Hash) -> binary() when Hash :: binary().
path(<<A:2/binary, B:2/binary, _:60/binary>> = Hash) ->
    true = valid_hash(Hash),
    <<"archive/cafs/sha256/", A/binary, "/", B/binary, "/", Hash/binary>>.

valid_hash(Hash) ->
    byte_size(Hash) =:= 64 andalso
        lists:all(fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) end,
            binary_to_list(Hash)).

-spec path_hash(Path) -> binary() | undefined when Path :: binary().
path_hash(<<"archive/cafs/sha256/", _:2/binary, "/", _:2/binary, "/", Hash:64/binary>> = Path) ->
    case valid_hash(Hash) andalso path(Hash) =:= Path of
        true -> Hash;
        false -> undefined
    end;
path_hash(_) -> undefined.

%% Hash and copy outside the transaction. Publication is an atomic rename of a
%% complete file on the same filesystem, serialized against attachment and GC.
-spec stage_file(File, Context) -> {ok, map()} | {error, term()}
    when File :: file:filename_all(), Context :: z:context().
stage_file(File, Context) ->
    case is_enabled(Context) of
        false -> {error, nodb};
        true -> stage_file_db(File, true, Context)
    end.

%% @doc Stage a known remote original without fetching its bytes. The backend
%% retargets its existing object under the canonical path in this transaction.
%% The caller must hold the medium lock and attach the returned stage in the same
%% transaction, so existing logical URLs remain resolvable throughout migration.
-spec stage_remote(Filename, Hash, Size, Context) -> {ok, map()} | {error, term()} when
    Filename :: binary(), Hash :: binary(), Size :: non_neg_integer(), Context :: z:context().
stage_remote(Filename, Hash, Size, Context) ->
    transaction(fun(Ctx) ->
        z_db:q("insert into cafs (hash,size) values ($1,$2) on conflict do nothing",
            [Hash, Size], Ctx),
        case stage_state(Hash, false, Ctx) of
            <<"ready">> ->
                case z_notifier:first(#filestore{action=cafs_adopt,
                        path= <<"archive/", Filename/binary>>, target_path=path(Hash)}, Ctx) of
                    ok ->
                        Token = z_ids:id(),
                        z_db:q("insert into cafs_pin (owner,hash,expires)
                            values ($1,$2,now()+interval '1 day')", [Token, Hash], Ctx),
                        z_db:q("update cafs set upload_pending=false where hash=$1", [Hash], Ctx),
                        {ok, #{hash => Hash, token => Token}};
                    undefined -> {error, unavailable};
                    Error -> Error
                end;
            _ -> {error, deleting}
        end
    end, Context).

%% @doc Stage a legacy original without making it eligible for upload yet.
%% attach_original/3 first tries to adopt its existing remote location.
-spec stage_original(File, Context) -> {ok, map()} | {error, term()} when
    File :: file:filename_all(), Context :: z:context().
stage_original(File, Context) ->
    case is_enabled(Context) of
        true -> stage_file_db(File, false, Context);
        false -> {error, nodb}
    end.

stage_file_db(File, UploadPending, Context) ->
    case z_crypto:hex_sha2_file(File) of
        {ok, Hash} -> stage_hashed(File, Hash, UploadPending, Context);
        {error, _} = Error -> Error
    end.

stage_hashed(File, ExpectedHash, UploadPending, Context) ->
    Token = z_ids:id(),
    %% An upload has no hash yet. Place its private copy on the CAFS filesystem
    %% in the existing staging layout, so interrupted copies remain GC-visible.
    StagingHash = case ExpectedHash of
        undefined -> z_crypto:hex_sha2(Token);
        _ -> ExpectedHash
    end,
    StagingPath = z_path:files_subdir(path(StagingHash), Context),
    Tmp = <<(z_convert:to_binary(StagingPath))/binary, ".", Token/binary, ".staging">>,
    try
        ok = z_filelib:ensure_dir(Tmp),
        {ok, Size} = file:copy(File, Tmp),
        {ok, Hash} = z_crypto:hex_sha2_file(Tmp),
        %% Mutable sources must still match the hash taken before copying.
        case ExpectedHash of
            undefined -> ok;
            Hash -> ok;
            _ -> throw(source_changed)
        end,
        Target = z_path:files_subdir(path(Hash), Context),
        ok = z_filelib:ensure_dir(Target),
        transaction(fun(Ctx) ->
            z_db:q("
                insert into cafs (hash, size, upload_pending)
                values ($1,$2,$3)
                on conflict do nothing",
                [Hash, Size, UploadPending], Ctx),
            case stage_state(Hash, UploadPending, Ctx) of
                <<"ready">> ->
                    case filelib:is_regular(Target) of
                        true -> ok;
                        false -> ok = file:rename(Tmp, Target)
                    end,
                    z_db:q("
                        insert into cafs_pin (owner,hash,expires)
                        values ($1, $2, now() + interval '1 day')",
                        [Token, Hash], Ctx),
                    {ok, #{hash => Hash, token => Token}};
                _ -> {error, deleting}
            end
        end, Context)
    catch
        error:{badmatch, {error, Reason}} -> {error, Reason};
        throw:source_changed -> {error, source_changed}
    after
        file:delete(Tmp)
    end.

%% A deletion can be cancelled only before the filestore dispatcher fences it.
%% Both cancellation and dispatch lock the same filestore row. Once dispatched,
%% keep the tombstone until remote absence is confirmed, even after a timeout.
stage_state(Hash, UploadPending, Context) ->
    case lock(Hash, Context) of
        <<"deleting">> ->
            case cancel_pending_delete(path(Hash), Context) of
                true ->
                    z_db:q("
                        update cafs
                        set state = 'ready',
                            upload_pending = $2,
                            gc_after = now()+interval '5 weeks'
                        where hash=$1",
                        [Hash, UploadPending], Context),
                    <<"ready">>;
                false ->
                    <<"deleting">>
            end;
        State -> State
    end.

cancel_pending_delete(Path, Context) ->
    %% No handler means no confirmation: a disabled backend may still have an
    %% outstanding remote deletion. Only the backend can safely cancel it.
    z_notifier:first(#filestore{action=cancel_delete, path=Path}, Context) =:= true.

-spec lookup(Filename, Context) -> {ok, binary()} | {error, term()}
    when Filename :: file:filename_all(), Context :: z:context().
lookup(Filename, Context) ->
    case is_enabled(Context) of
        false -> {error, enoent};
        true -> lookup_db(Filename, Context)
    end.

lookup_db(Filename, Context) ->
    case z_db:q1("select hash from filename_cafs where filename=$1", [Filename], Context) of
        undefined -> {error, enoent};
        Hash when is_binary(Hash) -> {ok, Hash}
    end.

-spec attach(Filename, Staged, Context) -> ok | {error, term()}
    when Filename :: file:filename_all(), Staged :: map(), Context :: z:context().
attach(Filename, #{hash := Hash, token := Token}, Context) ->
    transaction(fun(Ctx) ->
        case lock(Hash, Ctx) of
            <<"ready">> ->
                case z_db:q1("
                        select 1
                        from cafs_pin
                        where owner = $1
                          and hash = $2
                          and (expires is null or expires > now())",
                        [Token, Hash], Ctx)
                of
                    1 ->
                        ok = attach_hash(Filename, Hash, Ctx),
                        unpin(Hash, Token, Ctx);
                    undefined -> {error, expired}
                end;
            _ -> {error, deleting}
        end
    end, Context).

attach_hash(Filename, Hash, Context) ->
    z_db:q("
        insert into filename_cafs (filename,hash)
        values ($1,$2)
        on conflict do nothing",
        [Filename, Hash], Context),
    case lookup(Filename, Context) of
        {ok, Hash} -> ok;
        _ -> error(filename_conflict)
    end.

-spec clone_filename(Source, Destination, Context) -> ok | {error, term()}
    when Source :: binary(), Destination :: binary(), Context :: z:context().
clone_filename(Source, Destination, Context) ->
    transaction(fun(Ctx) ->
        case lookup(Source, Ctx) of
            {ok, Hash} ->
                case lock(Hash, Ctx) of
                    <<"ready">> ->
                        %% Recheck after the lock: cleanup may have removed the alias.
                        case lookup(Source, Ctx) of
                            {ok, Hash} -> attach_hash(Destination, Hash, Ctx);
                            Error -> Error
                        end;
                    _ -> {error, deleting}
                end;
            Error -> Error
        end
    end, Context).

%% @doc Attach an original and adopt its remote object in the same transaction.
%% Busy/unavailable backends defer adoption to the durable cleanup task; only
%% confirmed absence (or no backend) allows a new canonical upload.
-spec attach_original(Filename, Staged, Context) -> ok | {error, term()} when
    Filename :: binary(), Staged :: map(), Context :: z:context().
attach_original(Filename, #{hash := Hash} = Staged, Context) ->
    transaction(fun(Ctx) ->
        case attach(Filename, Staged, Ctx) of
            ok ->
                Result = z_notifier:first(#filestore{action=cafs_adopt,
                    path= <<"archive/", Filename/binary>>, target_path=path(Hash)}, Ctx),
                Pending = Result =:= {error, enoent} orelse
                    (Result =:= undefined andalso not m_config:get_boolean(site, filestore_used, Ctx)),
                z_db:q("update cafs set upload_pending=$2 where hash=$1", [Hash, Pending], Ctx),
                ok = z_media_cafs_migrate:queue_cleanup(Filename, Hash, Ctx),
                ok;
            Error -> Error
        end
    end, Context).

-spec promote(Filename, File, Context) -> {ok, binary()} | {error, term()}
    when Filename :: binary(), File :: file:filename_all(), Context :: z:context().
promote(Filename, File, Context) ->
    Result = case lookup(Filename, Context) of
        {ok, _} = Found -> Found;
        {error, enoent} ->
            case stage_original(File, Context) of
                {ok, #{hash := Hash} = Staged} ->
                    try
                        case attach_original(Filename, Staged, Context) of
                            ok -> {ok, Hash};
                            Error -> Error
                        end
                    after
                        release([{Filename, Staged}], Context)
                    end;
                Error -> Error
            end
    end,
    case Result of
        {ok, PromotedHash} ->
            case z_media_cafs_migrate:queue_cleanup(Filename, PromotedHash, Context) of
                ok -> Result;
                Error1 -> Error1
            end;
        Error1 -> Error1
    end.

-spec stage_medium(Medium, Context) -> {ok, [{binary(), map()}]} | {error, term()}
    when Medium :: map(), Context :: z:context().
stage_medium(Medium, Context) ->
    stage_medium(Medium, undefined, Context).

%% @doc Stage a freshly archived upload with one hash pass. ControlledFilename
%% must identify a new private copy owned by this upload; legacy files and
%% observer-supplied replacements must use the verified default path.
-spec stage_medium(Medium, ControlledFilename, Context) -> {ok, [{binary(), map()}]} | {error, term()}
    when Medium :: map(), ControlledFilename :: binary() | undefined, Context :: z:context().
stage_medium(Medium, ControlledFilename, Context) ->
    stage_medium([
            {<<"filename">>, <<"is_deletable_file">>},
            {<<"preview_filename">>, <<"is_deletable_preview">>}
        ], Medium, ControlledFilename, [], Context).

stage_medium([], _Medium, _Controlled, Staged, _Context) -> {ok, lists:reverse(Staged)};
stage_medium([{Key, Owned} | Rest], Medium, Controlled, Acc, Context) ->
    case {maps:get(Key, Medium, undefined), maps:get(Owned, Medium, false)} of
        {Name, true} when is_binary(Name), Name =/= <<>> ->
            case stage_medium_file(Name, Key, Medium, Controlled, Context) of
                {ok, Staged} -> stage_medium(Rest, Medium, Controlled, [{Name, Staged} | Acc], Context);
                {error, _} = Error ->
                    release(Acc, Context),
                    Error
            end;
        _ -> stage_medium(Rest, Medium, Controlled, Acc, Context)
    end.

stage_medium_file(Name, Key, Medium, Controlled, Context) ->
    case lookup(Name, Context) of
        {ok, Hash} ->
            Token = z_ids:id(),
            case pin(Hash, Token, Context) of
                ok ->
                    1 = z_db:q("
                        update cafs_pin
                        set expires = now()+interval '1 day'
                        where owner = $1
                          and hash = $2",
                        [Token, Hash], Context),
                    {ok, #{hash => Hash, token => Token}};
                Error -> Error
            end;
        {error, enoent} ->
            File = z_media_archive:abspath(Name, Context),
            Result = case Key =:= <<"filename">> andalso Name =:= Controlled
                andalso is_enabled(Context)
            of
                true -> stage_hashed(File, undefined, true, Context);
                false -> stage_file(File, Context)
            end,
            case Result of
                {ok, Staged} ->
                    Props = case Key of
                        <<"filename">> -> maps:with([<<"mime">>, <<"width">>, <<"height">>,
                            <<"orientation">>, <<"exif">>, <<"extracted_text">>], Medium);
                        _ -> #{ <<"mime">> => z_media_identify:guess_mime(Name) }
                    end,
                    {ok, Staged#{metadata => Props, cleanup => true}};
                Error -> Error
            end
    end.

-spec attach_medium(Staged, Context) -> ok when Staged :: list(), Context :: z:context().
attach_medium(Staged, Context) ->
    lists:foreach(fun({Name, S}) ->
        ok = attach(Name, S, Context),
        case S of
            #{hash := Hash, metadata := Props} ->
                z_db:q("update cafs set mime=$2,props=$3,modified=now() where hash=$1",
                    [Hash, maps:get(<<"mime">>, Props, undefined), ?DB_PROPS(Props)], Context);
            _ -> ok
        end
    end, Staged).

-spec release(Staged, Context) -> ok when Staged :: list(), Context :: z:context().
release(Staged, Context) ->
    lists:foreach(fun({_Name, #{hash := H, token := T}}) -> unpin(H, T, Context) end, Staged).

-spec pin(Hash, Owner, Context) -> ok | {error, term()}
    when Hash :: binary(), Owner :: binary(), Context :: z:context().
pin(Hash, Owner, Context) ->
    transaction(fun(Ctx) ->
        case lock(Hash, Ctx) of
            <<"ready">> ->
                z_db:q("
                    insert into cafs_pin (owner,hash)
                    values ($1,$2)
                    on conflict do nothing",
                    [Owner, Hash], Ctx),
                ok;
            _ -> {error, deleting}
        end
    end, Context).

-spec unpin(Hash, Owner, Context) -> ok
    when Hash :: binary(), Owner :: binary(), Context :: z:context().
unpin(Hash, Owner, Context) ->
    z_db:q("delete from cafs_pin where owner=$1 and hash=$2", [Owner, Hash], Context),
    ok.

-spec with_path_pin(Path, Fun, Context) -> term()
    when Path :: binary(), Fun :: function(), Context :: z:context().
with_path_pin(Path, Fun, Context) ->
    case path_hash(Path) of
        undefined -> Fun();
        Hash -> with_pin(Hash, Fun, Context)
    end.

with_pin(Hash, Fun, Context) ->
    Owner = z_ids:id(),
    case pin(Hash, Owner, Context) of
        ok ->
            1 = z_db:q("
                update cafs_pin
                set expires = now()+interval '5 minutes'
                where owner = $1
                  and hash = $2",
                [Owner, Hash], Context),
            Pid = self(),
            Guard = spawn_link(fun() ->
                Ref = monitor(process, Pid),
                pin_guard(Ref, Hash, Owner, Context)
            end),
            try Fun() after unlink(Guard), Guard ! done, unpin(Hash, Owner, Context) end;
        Error -> Error
    end.

-spec with_local_file(Hash, Fun, Context) -> term()
    when Hash :: binary(), Fun :: function(), Context :: z:context().
with_local_file(Hash, Fun, Context) ->
    %% Filestore lookup itself registers the caller with the cache. Scope the
    %% entire lookup and callback to a worker, not just fetch_file/2.
    file_callback(fun() ->
        with_pin(Hash, fun() ->
            Path = path(Hash),
            Local = z_path:files_subdir(Path, Context),
            case z_notifier:first(#filestore{action=lookup, path=Path, local_path=Local}, Context) of
                undefined -> local_callback(Local, Fun);
                {ok, {filename, File, _}} -> local_callback(File, Fun);
                {ok, {filezcache, Pid, _}} ->
                    case filezcache:lookup_file(Pid, [lock]) of
                        {ok, {file, _, File}} -> Fun(File);
                        Error -> Error
                    end;
                Error -> Error
            end
        end, Context)
    end).

-spec aliases(Path, Context) -> [binary()] when Path :: binary(), Context :: z:context().
aliases(Path, Context) ->
    case path_hash(Path) of
        undefined -> [];
        Hash -> [F || {F} <- z_db:q("select filename from filename_cafs where hash=$1", [Hash], Context)]
    end.

-spec retire_filename(Filename, Context) -> ok when Filename :: binary(), Context :: z:context().
retire_filename(Filename, Context) ->
    transaction(fun(Ctx) ->
        case lookup(Filename, Ctx) of
            {ok, Hash} ->
                lock(Hash, Ctx),
                %% Called after medium_deleted's five-week retention interval.
                z_db:q("
                    delete from filename_cafs
                    where filename=$1
                      and not exists (select 1 from medium where filename=$1 or preview_filename=$1)",
                    [Filename], Ctx),
                ok;
            {error, enoent} -> ok
        end
    end, Context).

%% Include generated variants and durable task inputs in an explicit bulk move.
%% This only queues existing objects; it never hashes or fetches legacy files.
-spec queue_all(Context) -> ok when Context :: z:context().
queue_all(Context) ->
    z_db:q("
        update cafs
        set upload_pending = true
        where state='ready'",
        Context),
    ok.

-spec queue_uploads(Context) -> ok when Context :: z:context().
queue_uploads(Context) ->
    lists:foreach(
        fun({Hash, Mime}) ->
            transaction(
                fun(Ctx) ->
                    case lock(Hash, Ctx) of
                        <<"ready">> ->
                            case z_notifier:first(#filestore{action=upload, path=path(Hash), mime=Mime}, Ctx) of
                                ok -> z_db:q("update cafs set upload_pending=false where hash=$1", [Hash], Ctx);
                                _ -> ok
                            end;
                        _ ->
                            ok
                    end
                end,
                Context)
        end,
        z_db:q("
            select hash, mime
            from cafs
            where upload_pending
              and state='ready'
            limit 100", Context)),
    ok.

-spec maintenance(Context) -> ok when Context :: z:context().
maintenance(Context) ->
    case is_enabled(Context) of
        false -> ok;
        true -> maintenance_db(Context)
    end.

maintenance_db(Context) ->
    ok = z_cafs_file:reconcile(Context),
    ok = z_cafs_preview:maintenance(Context),
    queue_uploads(Context),
    z_db:q("delete from cafs_pin where expires < now()", Context),
    %% Abandoned aliases (e.g. a failed medium insertion) receive the same grace
    %% period as deleted media. Never scan or fetch legacy remote files.
    lists:foreach(
        fun({Filename}) -> retire_filename(Filename, Context) end,
        z_db:q("
            select filename
            from filename_cafs f
            where coalesce(retired_at,created) < now()-interval '5 weeks'
              and not exists (select 1 from medium where filename=f.filename or preview_filename=f.filename)
            limit 100", Context)),
    lists:foreach(
        fun({Hash}) -> collect(Hash, Context) end,
        z_db:q("select hash from cafs where gc_after<now() order by gc_after limit 100", Context)),
    ok.

%% @doc Collect a generated object after removing its derivative reference.
%% Ordinary references and pins still protect shared original/preview bytes.
-spec collect_unused(Hash, Context) -> ok when Hash :: binary(), Context :: z:context().
collect_unused(Hash, Context) ->
    z_db:q("update cafs set gc_after=now() where hash=$1", [Hash], Context),
    collect(Hash, Context),
    ok.

collect(Hash, Context) ->
    Result = transaction(fun(Ctx) ->
        State = lock(Hash, Ctx),
        case State of
            <<"deleting">> -> deleting;
            <<"ready">> ->
                case z_db:q1("
                    select exists(select 1 from filename_cafs where hash=$1)
                        or exists(select 1 from cafs_pin where hash=$1)
                        or exists(select 1 from cafs_derivative where output_hash=$1 and source_hash<>$1)",
                    [Hash], Ctx) orelse readers(path(Hash), Ctx) =/= []
                of
                    true ->
                        z_db:q("update cafs set gc_after=now()+interval '1 day' where hash=$1", [Hash], Ctx),
                        retained;
                    false ->
                        z_db:q("delete from cafs_derivative where source_hash=$1", [Hash], Ctx),
                        z_db:q("update cafs set state='deleting' where hash=$1", [Hash], Ctx),
                        deleting
                end;
            _ -> retained
        end
    end, Context),
    case Result of
        deleting -> delete_object(Hash, Context);
        _ -> ok
    end.

delete_object(Hash, Context) ->
    transaction(fun(Ctx) ->
        case lock(Hash, Ctx) of
            <<"deleting">> -> delete_object_locked(Hash, Ctx);
            _ -> ok
        end
    end, Context).

delete_object_locked(Hash, Context) ->
    %% Pending remote work must not monopolize the oldest-first collection batch.
    z_db:q("update cafs set gc_after=now()+interval '1 hour' where hash=$1", [Hash], Context),
    Path = path(Hash),
    %% Keep tombstones until the remote delete has completed. A disabled module,
    %% read-only backend, or 'Never' retention leaves the tombstone intact.
    case z_file_request:delete_remote(Path, Context) of
        absent ->
            case readers(Path, Context) of
                [] -> delete_local_object(Hash, Path, Context);
                _ -> ok
            end;
        _ -> ok
    end.

delete_local_object(Hash, Path, Context) ->
    case file:delete(z_path:files_subdir(Path, Context)) of
        ok -> z_db:q("delete from cafs where hash=$1 and state='deleting'", [Hash], Context);
        {error, enoent} -> z_db:q("delete from cafs where hash=$1 and state='deleting'", [Hash], Context);
        _ -> ok
    end.

%% Operations are an ordered pipeline; sorting would change their meaning.
%% Expanded mediaclasses and output extension are part of the caller's key.
-spec derivative(Hash, Operations, Version, Generate, Context) -> {ok, binary()} | {error, term()}
    when Hash :: binary(), Operations :: term(), Version :: term(),
         Generate :: function(), Context :: z:context().
derivative(Hash, Operations, Version, Generate, Context) ->
    derivative(Hash, Operations, Version, undefined, Generate, Context).

-spec derivative(Hash, Operations, Version, Usage, Generate, Context) -> {ok, binary()} | {error, term()}
    when Hash :: binary(), Operations :: term(), Version :: term(), Usage :: binary() | undefined,
         Generate :: function(), Context :: z:context().
derivative(Hash, Operations, Version, Usage, Generate, Context) ->
    with_pin(Hash, fun() ->
        derivative_wait(Hash, term_to_binary(Operations), term_to_binary(Version), Usage, Generate,
            erlang:monotonic_time(millisecond) + 120000, Context)
    end, Context).

derivative_wait(Hash, Ops, Version, Usage, Generate, Deadline, Context) ->
    Token = z_ids:id(),
    Claim = transaction(fun(Ctx) ->
        lock(Hash, Ctx),
        case z_cafs_preview:bind(Usage, Hash, Ops, Version, Ctx) of
            ok -> ok;
            {error, enoent} -> throw({rollback, enoent})
        end,
        z_db:q("
            insert into cafs_derivative
                (source_hash,operations,renderer_version)
            values
                ($1,$2,$3)
            on conflict do nothing",
            [Hash, Ops, Version], Ctx),
        case z_db:q1("
                select output_hash
                from cafs_derivative
                where source_hash = $1
                  and operations = $2
                  and renderer_version = $3",
                  [Hash, Ops, Version], Ctx) of
            undefined ->
                case z_db:q1("
                    update cafs_derivative
                    set claim_token = $4,
                        claim_until = now()+interval '5 minutes'
                    where source_hash = $1
                      and operations = $2
                      and renderer_version = $3
                      and (claim_until is null or claim_until<now())
                    returning claim_token",
                    [Hash, Ops, Version, Token], Ctx)
                of
                    Token -> claimed;
                    undefined -> waiting
                end;
            Output -> {ok, Output}
        end
    end, Context),
    case Claim of
        {ok, _} = Ready -> Ready;
        claimed -> generate_derivative(Hash, Ops, Version, Token, Generate, Context);
        waiting ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true -> timer:sleep(100), derivative_wait(Hash, Ops, Version, Usage, Generate, Deadline, Context);
                false -> {error, timeout}
            end;
        Error -> Error
    end.

generate_derivative(Hash, Ops, Version, Token, Generate, Context) ->
    Args = [Hash, Ops, Version, Token],
    Parent = self(),
    Renewer = spawn_link(fun() -> renew_claim(Parent, Args, Context) end),
    try
        case Generate() of
            {ok, #{hash := Output, token := Pin} = Staged} ->
                transaction(fun(Ctx) ->
                    case z_db:q1("
                        update cafs_derivative
                        set output_hash = $5,
                            claim_token = null,
                            claim_until = null
                        where source_hash = $1
                          and operations = $2
                          and renderer_version = $3
                          and claim_token = $4
                        returning output_hash",
                        Args ++ [Output], Ctx)
                    of
                        Output ->
                            case maps:get(preview_cleanup, Staged, undefined) of
                                undefined -> ok;
                                Path -> ok = z_media_cafs_migrate:queue_preview_cleanup(Path, Output, true, Ctx)
                            end,
                            unpin(Output, Pin, Ctx),
                            {ok, Output};
                        undefined -> {error, stale_claim}
                    end
                end, Context);
            Error -> Error
        end
    after
        unlink(Renewer),
        Renewer ! stop,
        z_db:q("
            update cafs_derivative
            set claim_token = null,
                claim_until = null
            where source_hash = $1
              and operations = $2
              and renderer_version = $3
              and claim_token = $4",
              Args, Context)
    end.

renew_claim(Parent, Args, Context) ->
    Ref = monitor(process, Parent),
    renew_claim_loop(Ref, Args, Context).

renew_claim_loop(Ref, Args, Context) ->
    receive
        stop -> demonitor(Ref, [flush]);
        {'DOWN', Ref, process, _, _} -> ok
    after 60000 ->
        1 = z_db:q("
            update cafs_derivative
            set claim_until = now() + interval '5 minutes'
            where source_hash = $1
              and operations = $2
              and renderer_version = $3
              and claim_token = $4", Args, Context),
        renew_claim_loop(Ref, Args, Context)
    end.

lock(Hash, Context) ->
    z_db:q1("select state from cafs where hash=$1 for update", [Hash], Context).

transaction(Fun, Context) ->
    case z_db:transaction(Fun, Context) of
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end.

%% @doc Remove a local CAFS copy after the backend confirmed a healthy remote
%% object. The uploader must hold its path pin. Other pins/readers postpone the
%% removal, and the row lock excludes new pins until the unlink is complete.
-spec delete_uploaded_file(Path, Context) -> ok | {error, term()} when
    Path :: binary(), Context :: z:context().
delete_uploaded_file(Path, Context) ->
    case path_hash(Path) of
        undefined -> {error, unsupported};
        Hash -> transaction(fun(Ctx) ->
            case lock(Hash,Ctx) of
                <<"ready">> ->
                    Pins = z_db:q1("select count(*) from cafs_pin where hash=$1 and expires>now()",[Hash],Ctx),
                    case Pins =< 1 andalso readers(Path,Ctx) =:= [] of
                        true ->
                            case file:delete(z_path:files_subdir(Path,Ctx)) of
                                ok -> ok;
                                {error,enoent} -> ok;
                                Error -> Error
                            end;
                        false -> {error,busy}
                    end;
                _ -> {error,busy}
            end
        end,Context)
    end.

%% File-entry processes subscribe to physical objects, including shared variants.
%% gproc removes subscriptions automatically when a file entry exits.
-spec track(Path, Context) -> ok when Path :: binary(), Context :: z:context().
track(Path, Context) ->
    Register = fun() ->
        gproc:ensure_reg({p, l, {?MODULE, z_context:site(Context), tracking_key(Path)}},
            z_context:get(file_entry_owner, Context, self())),
        ok
    end,
    case path_hash(Path) of
        undefined -> Register();
        Hash ->
            %% Serialize registration with collection and local offloading.
            %% After this lock is released, the registered reader protects the
            %% file until its resolver exits, including before opening it.
            case transaction(fun(Ctx) ->
                case lock(Hash, Ctx) of
                    <<"ready">> -> Register();
                    _ -> {error, deleting}
                end
            end, Context) of
                ok -> ok;
                {error, Reason} -> throw({file_unavailable, Reason})
            end
    end.

-spec readers(Path, Context) -> [pid()] when Path :: binary(), Context :: z:context().
readers(Path, Context) ->
    lists:usort([Owner || {_, Owner} <-
        gproc:lookup_values({p, l, {?MODULE, z_context:site(Context), tracking_key(Path)}})]).

%% Root-specific entries can also be reading a legacy path during promotion.
%% Filestore relocation must invalidate all of them, not just the public entry.
tracking_key(Path) ->
    case path_hash(Path) of
        undefined -> Path;
        Hash -> Hash
    end.

pin_guard(Ref, Hash, Owner, Context) ->
    receive
        done -> demonitor(Ref, [flush]);
        {'DOWN', Ref, process, _, _} -> unpin(Hash, Owner, Context)
    after 60000 ->
        1 = z_db:q("
            update cafs_pin
            set expires = now() + interval '5 minutes'
            where owner = $1
              and hash = $2",
            [Owner, Hash], Context),
        pin_guard(Ref, Hash, Owner, Context)
    end.

file_callback(Fun) ->
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() ->
        Result = try {return, Fun()}
            catch Class:Reason:Stack -> {raise, Class, Reason, Stack} end,
        Parent ! {self(), Result}
    end),
    spawn(fun() ->
        ParentRef = monitor(process, Parent),
        WorkerRef = monitor(process, Pid),
        receive
            {'DOWN', ParentRef, process, Parent, _} -> exit(Pid, kill);
            {'DOWN', WorkerRef, process, Pid, _} -> ok
        end
    end),
    receive
        {Pid, Result} ->
            receive {'DOWN', Ref, process, Pid, _} -> ok end,
            case Result of
                {return, Value} -> Value;
                {raise, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack)
            end;
        {'DOWN', Ref, process, Pid, Reason} -> {error, {file_callback, Reason}}
    end.

%% A private hard link survives filestore's rename/unlink after upload. A copy is
%% only needed when temporary storage is on another filesystem.
local_callback(File, Fun) ->
    Tmp = z_tempfile:new(),
    Result = case file:make_link(File, Tmp) of
        ok -> ok;
        {error, exdev} ->
            case file:copy(File, Tmp) of
                {ok, _} -> ok;
                Error -> Error
            end;
        Error -> Error
    end,
    try
        case Result of
            ok -> Fun(Tmp);
            Error1 -> Error1
        end
    after
        file:delete(Tmp)
    end.
