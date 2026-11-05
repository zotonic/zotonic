%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2026 Marc Worrell
%% @doc Model for medium database
%% @end

%% Copyright 2009-2026 Marc Worrell
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

-module(m_media).
-moduledoc(#{
    zotonic_keywords => ["reference", "backend_developer", "model", "media_management", "media_resource", "file_storage", "query"]
}).
-moduledoc("
Access to data about uploaded files and other media.

The `medium` (singular form of media) table stores all information of uploaded files or other media. Every resource can
contain a single medium. A resource with a medium is most often of the category image, audio, video or document.

In template the `m_media` model is used to fetch the medium record by the resource id: `m.media[id]`. This is the same
function as with `m.rsc[id].medium` except, that the m_rsc model does access control checks and the m_media does not.

The `m_media` model implements all functions to handle media files and is used by other Erlang modules.

Owned uploaded files and explicit previews use `m_cafs` for content-addressable
storage. `filename` and `preview_filename` remain logical filenames used by image
and media URLs; they need not exist as individual files in the archive directory.
Resolve files through Zotonic's file APIs instead of constructing local paths.
Identical bytes can share storage without sharing resource permissions.

The optional `digest` property is the lowercase hexadecimal SHA-256 digest of the
original bytes. Legacy media can lack a digest or CAFS mapping; existing files are
promoted on demand when their bytes are available. Reading a medium does not fetch
remote bytes to calculate a digest. See `model#m_cafs` for the storage lifecycle,
preview retention, and internal Erlang APIs.


Copy a medium to another resource
--------------------------------

`m_media:migrate_to_cafs(Context)` queues a resumable scan of all current medium
records. It requires `use mod_admin_config`. Owned originals and extracted preview
images are deduplicated into CAFS; logical filenames remain unchanged. Local bytes
are hashed, while remote-only originals with a valid SHA-256 digest reuse their
existing remote location. Remote-only files without a digest are logged and skipped,
never downloaded for hashing. Retry tasks handle unavailable backends separately
from the scan. Legacy remote copies are retired only after a healthy canonical
remote copy exists; deletion follows filestore retention. Rerunning is safe.

`m_media:delete_generated_previews(Context)` queues background deletion of resized
variants and requires `use` permission on `mod_admin_config`. Original files and
`preview_filename` images are preserved, including CAFS bytes shared with them.
The maintenance page exposes this operation. Remote deletion follows filestore
retention; requests may regenerate previews during or after cleanup.

Use `m_media:copy(Source, Destination, Context)` to replace an existing resource's
medium with a copy of another resource's medium. Both arguments accept resource
ids or names. The result is `ok` or `{error, Reason}`.

```erlang
m_media:copy(SourceId, DestinationId, Context).
```

The caller must be allowed to view the source, edit the destination, and insert
media of the source's MIME type and size. Permission failures return
`{error, eacces}`; missing resources or a source without a medium return
`{error, enoent}`. Copying a resource onto itself is a no-op after these checks.
A missing source medium never clears the destination.

The source is preserved. Destination resource properties, including category,
title, and image-edit settings, are unchanged. Medium properties are copied with
the destination id and a new creation timestamp. Owned originals and explicit
previews receive independent logical filenames sharing CAFS bytes; mapped remote
objects need no download. Legacy owned files are promoted on demand. Non-owned
files, including library assets, are snapshotted into CAFS under new owned filenames
without changing the source reference. Failure to prepare either owned file leaves the
destination medium untouched and releases newly created aliases.

Replacement uses the normal medium notifications, cache invalidation, MQTT update,
and recovery retention for the destination's previous files. This is an Erlang API;
no HTTP or MQTT copy endpoint is provided. `duplicate/3` remains the internal
insertion-only helper used when duplicating a whole resource.



Properties of a medium record
-----------------------------

A medium record has minimally the following properties, other properties can be added by modules.

| Property                 | Description                                                                      | Example value                          |
| ------------------------ | -------------------------------------------------------------------------------- | -------------------------------------- |
| id | Id of the medium record, equal to the page id. | `512` |
| filename | Logical filename of the uploaded file. For legacy storage, this is relative to the archive directory. | `<<\"2009/10/20/zotonic-datamodel.jpg\">>` |
| rootname | Root name of the filename. | `<<\"zotonic-datamodel\">>` |
| original_filename | Filename as suggested by the user agent when uploading the file. Directory paths are removed and unsafe characters replaced with underscores. The root name is limited to 80 Unicode characters; the extension is preserved. | `<<\"Zotonic-datamodel.jpg\">>` |
| mime | Mime type of the medium. | `<<\"image/jpeg\">>` |
| width | Width in pixels. | `536` |
| height | Height in pixels. | `737` |
| orientation | Exif oritentation of the image. | `1` |
| sha1                     | Optional sha1 checksum of uploaded file. Undefined when not present.             |                                        |
| digest | Optional SHA-256 digest of the original bytes as 64 lowercase hexadecimal characters. Legacy media can lack this value. | |
| size | Size in bytes of the uploaded file. | `71585` |
| preview_filename       | Optional filename for a generated file preview.                                  |                                        |
| preview_width          | Optional. Width of the generated preview.                                        |                                        |
| preview_height         | Optional. Height of the generated preview.                                       |                                        |
| is_deletable_file | If the file should be deleted when the medium record is deleted. A boolean. | `true` |
| is_deletable_preview | If the optionally generated preview file should be deleted when the medium record is deleted. A boolean. | `false` |
| created | Timestamp when the medium record is created. | `{{2009,10,20},{13,47,27}}` |

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/+id/...` | Return medium record for resource `+id` when the resource is visible; returns `enoent` for unknown resource and `eacces` when not visible. |
| `post` | `/upload_check` | Check payload `digest` (SHA-256), `size`, and `filename` for a visible current CAFS original. Returns `needed: true` on a miss, or `needed: false` plus `media_upload_token`. |
| `post` | `/+id` | Replace medium of existing resource `+id` from payload `url` or uploaded `file` (`#upload`); optional payload `rsc` map updates resource props. No further lookups. |
| `post` | `/` | Create a new resource+medium from payload `url` or uploaded `file` (`#upload`), with optional payload `rsc` properties for the new resource. No further lookups. |
| `delete` | `/+id` | Delete medium linked to resource `+id` (queues file cleanup, emits media events); requires editable resource access. No further lookups. |

Upload clients can first call `/upload_check`. Unknown, inaccessible and unavailable
files return the same miss response. A successful token is signed, user-bound,
and valid for one hour. Pass `file: {media_upload_token: Token}` to the insert or
replace API, or use this map as a wired form field value. Source visibility and
its current CAFS mapping are rechecked at submission. Destination ACLs, scanning,
sanitization and media observers still run normally. Remote-only CAFS bytes can
be fetched server-side for this processing. Visible legacy originals with a
matching checksum are lazily promoted before reuse. Originals without a known checksum and files changed by previous
conversion require a normal upload.

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,
    m_post/3,
    m_delete/3,

    identify/2,
    get/2,
    get_by_filename/2,
    exists/2,
    depiction/2,
    depicts/2,
    delete/2,
    migrate_to_cafs/1,
    delete_generated_previews/1,
    replace/3,
    copy/3,
    duplicate/3,
    merge/3,
    insert_file/2,
    insert_file/3,
    insert_file/4,
    insert_medium/4,
    replace_file/3,
    replace_file/4,
    replace_file/5,
    replace_file/6,
    replace_medium/5,
    insert_url/2,
    insert_url/3,
    insert_url/4,
    replace_url/4,
    replace_url/5,
    reupload/2,
    recover_medium/2,
    save_preview_url/3,
    save_preview/4,
    make_preview_unique/3,
    is_unique_file/2,
    download_file/2,
    download_file/3,
    mime_to_category/1
]).

-include_lib("zotonic.hrl").
-include_lib("zotonic_file.hrl").

-define(MEDIA_MAX_LENGTH_PREVIEW, 10 * 1024 * 1024).
-define(MEDIA_MAX_LENGTH_DOWNLOAD, 500 * 1024 * 1024).
-define(MEDIA_TIMEOUT_DOWNLOAD, 60 * 1000).
-define(MEDIA_MAX_ROOTNAME_LENGTH, 80).

-type media_url() :: binary() | string().
-type options() :: [ option() ].
-type option() :: {preferred_category, m_rsc:resource()}
                | {preview_url, media_url()}
                | {max_length, non_neg_integer()}
                | {timeout, non_neg_integer()}
                | {expected_medium, {binary(), term()}}
                | m_rsc:update_option().

-export_type([media_url/0, options/0, option/0]).

%% @doc Queue a resumable scan of owned medium files for CAFS deduplication.
%% Remote-only files without a SHA-256 digest are skipped, never downloaded.
-spec migrate_to_cafs(Context) -> ok | {error, term()} when Context :: z:context().
migrate_to_cafs(Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        false -> {error, eacces};
        true ->
            case m_cafs:is_enabled(Context) of
                false -> {error, nodb};
                true ->
                    Last = z_db:q1("select coalesce(max(id), 0) from medium", Context),
                    case z_pivot_rsc:insert_task_after(0, z_media_cafs_migrate, run,
                            <<"medium">>, [0, Last], Context) of
                        {ok, _} -> ok;
                        {error, _} = Error -> Error
                    end
            end
    end.

%% @doc Queue deletion of resized variants, preserving originals and extracted
%% preview_filename images. Remote deletion follows the backend retention policy.
-spec delete_generated_previews(Context) -> ok | {error, term()} when Context :: z:context().
delete_generated_previews(Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        false -> {error, eacces};
        true ->
            case m_cafs:is_enabled(Context) of
                false -> {error, nodb};
                true ->
                    Before = z_db:q1("select clock_timestamp()::text", Context),
                    Args = [Before, erlang:system_time(second), cafs],
                    case z_pivot_rsc:insert_task_after(0, z_media_preview_purge, run,
                            z_ids:id(), Args, Context) of
                        {ok, _} -> ok;
                        {error, _} = Error -> Error
                    end
            end
    end.


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ Id | Rest ], _Msg, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, enoent};
        RscId ->
            case z_acl:rsc_visible(RscId, Context) of
                true -> {ok, {get(RscId, Context), Rest}};
                false -> {error, eacces}
            end
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

%% @doc API to update or insert resources.
-spec m_post( list( binary() ), zotonic_model:opt_msg(), z:context() ) -> {ok, term()} | ok | {error, term()}.
m_post([ <<"upload_check">> ], #{payload := Payload}, Context) when is_map(Payload) ->
    z_media_upload:check(Payload, Context);
m_post(Path, #{payload := #{<<"file">> := #{<<"media_upload_token">> := _} = File} = Payload}, Context) ->
    case z_media_upload:materialize(File, Context) of
        {ok, Upload} -> m_post(Path, #{payload => Payload#{<<"file">> => Upload}}, Context);
        Error -> Error
    end;
m_post([ Id ], #{ payload := Payload }, Context) when is_map(Payload) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, enoent};
        RscId ->
            RscProps = maps:get(<<"rsc">>, Payload, #{}),
            case Payload of
                #{ <<"url">> := Url } ->
                    replace_url(Url, RscId, RscProps, Context);
                #{ <<"file">> := #upload{ tmpmonitor = Pid } = File } when is_pid(Pid) ->
                    replace_file(File, RscId, RscProps, Context);
                _ ->
                    {error, unacceptable}
            end
    end;
m_post([], #{ payload := Payload }, Context) when is_map(Payload) ->
    RscProps = maps:get(<<"rsc">>, Payload, #{}),
    case Payload of
        #{ <<"url">> := Url } ->
            insert_url(Url, RscProps, Context);
        #{ <<"file">> := #upload{ tmpmonitor = Pid } = File } when is_pid(Pid) ->
            insert_file(File, RscProps, Context);
        _ ->
            {error, unacceptable}
    end.

%% @doc API to delete the medium of a resource
-spec m_delete( list( binary() ), zotonic_model:opt_msg(), z:context() ) -> {ok, term()} | ok | {error, term()}.
m_delete([ Id ], _Msg, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, enoent};
        RscId ->
            delete(RscId, Context)
    end.


%% @doc Return the identification of a medium. Used by z_media_identify:identify()
-spec identify(IdOrFilename, Context) -> {ok, z_media_identify:media_info()} | {error, term()}
    when IdOrFilename :: m_rsc:resource_id() | file:filename_all(), Context :: z:context().
identify(Id, Context) when is_integer(Id) ->
    z_db:qmap_props_row("
        select id, mime, width, height, orientation, frame_count
        from medium
        where id = $1",
        [ Id ],
        [ {keys, binary} ],
        Context);
identify(ImageFile, Context) ->
    case z_media_archive:is_archived(ImageFile, Context) of
        true ->
            RelFile = z_media_archive:rel_archive(ImageFile, Context),
            identify_medium_filename(RelFile, Context);
        false ->
            identify_medium_filename(ImageFile, Context)
    end.

identify_medium_filename(MediumFilename, Context) ->
    z_db:qmap_props_row("
        select id, mime, width, height, orientation, frame_count
        from medium
        where filename = $1",
        [ MediumFilename ],
        [ {keys, binary} ],
        Context).


%% @doc Check if a medium record exists. The argument should be undefined, or
%% a (textual) integer.
-spec exists( undefined | string() | binary() | integer(), z:context() ) -> boolean().
exists(undefined, _Context) ->
    false;
exists([C | _] = Name, Context) when is_integer(C) ->
    case z_utils:only_digits(Name) of
        true -> exists(list_to_integer(Name), Context);
        false -> false
    end;
exists(Id, Context) when is_binary(Id) ->
    exists(binary_to_list(Id), Context);
exists(Id, Context) ->
    case z_db:q1("select id from medium where id = $1", [Id], Context) of
        undefined -> false;
        _ -> true
    end.


%% @doc Get the medium record with the id
-spec get( m_rsc:resource() | undefined, z:context() ) -> z_media_identify:media_info() | undefined.
get(Id, Context) when is_integer(Id) ->
    F = fun() ->
        case z_db:qmap_props_row(
            "select * from medium where id = $1",
            [Id],
            [ {keys, binary} ],
            Context)
        of
            {ok, Map} -> Map;
            {error, nodb} -> undefined;
            {error, enoent} -> undefined
        end
    end,
    z_depcache:memo(F, {medium, Id}, ?WEEK, [Id], Context);
get(undefined, _Context) ->
    undefined;
get(RscId, Context) ->
    get(m_rsc:rid(RscId, Context), Context).


%% @doc Fetch a medium by filename
-spec get_by_filename( binary(), z:context() ) -> z_media_identify:media_info() | undefined.
get_by_filename(Filename, Context) ->
    case z_depcache:get({medium, Filename}, Context) of
        undefined ->
            Row = z_db:qmap_props_row("select * from medium where filename = $1", [Filename], Context),
            case Row of
                {ok, #{ <<"id">> := Id } = Medium} ->
                    z_depcache:set({medium, Filename}, Medium, ?HOUR, [Id], Context),
                    Medium;
                {error, enoent} ->
                    z_depcache:set({medium, Filename}, undefined, ?HOUR, Context),
                    undefined;
                {error, nodb} ->
                    z_depcache:set({medium, Filename}, undefined, ?HOUR, Context),
                    undefined
            end;
        {ok, Row} ->
            Row
    end.


%% @doc Get the medium record that depicts the resource id. "depiction" Predicates are preferred, if
%% they are missing then the attached medium record itself is returned.  We must be able to generate a preview
%% from the medium.
-spec depiction( m_rsc:resource_id(), z:context() ) -> map() | undefined.
depiction(Id, Context) ->
    try
        z_depcache:memo(
            fun() -> depiction([Id], [], Context) end,
            {depiction, Id},
            ?WEEK, [Id, {medium, Id}], Context)
    catch
        exit:{timeout, _} ->
            undefined
    end.

depiction([], _Visited, _Context) ->
    undefined;
depiction([Id | Ids], Visited, Context) ->
    case lists:member(Id, Visited) of
        true ->
            undefined;
        false ->
            Depictions = m_edge:objects(Id, depiction, Context) ++ [Id],
            case depiction(Depictions, [Id | Visited], Context) of
                undefined ->
                    case get(Id, Context) of
                        undefined ->
                            depiction(Ids, [Id | Visited], Context);
                        Media ->
                            Media
                    end;
                Media when is_map(Media) ->
                    Media
            end
    end.


%% @doc Return the list of resources that is depicted by the medium (excluding the rsc itself)
-spec depicts( m_rsc:resource_id(), z:context() ) -> list( m_rsc:resource_id() ).
depicts(Id, Context) ->
    m_edge:subjects(Id, depiction, Context).


%% @doc Delete the medium at the id. The file is queued for later deletion by the
%% database trigger that adds the file to medium delete log.
-spec delete( m_rsc:resource_id(), z:context() ) -> ok  | {error, term()}.
delete(Id, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            Depicts = depicts(Id, Context),
            medium_delete(Id, Context),
            [z_depcache:flush(DepictId, Context) || DepictId <- Depicts],
            m_rsc:touch(Id, Context),
            z_notifier:notify(#media_replace_file{ id = Id, medium = undefined }, Context),
            z_mqtt:publish(
                [ <<"model">>, <<"media">>, <<"event">>, Id, <<"delete">> ],
                #{ id => Id },
                Context),
            ok;
        false ->
            {error, eacces}
    end.


%% @doc Replace or insert a medium record for the page.  This is useful for non-file related media.
%% Resets all non mentioned attributes.
-spec replace(Id, MediaProps, Context) -> ok  | {error, term()} when
    Id :: m_rsc:resource_id(),
    MediaProps :: map() | list(),
    Context :: z:context().
replace(Id, MediaProps, Context) when is_list(MediaProps) ->
    {ok, Map} = z_props:from_list(MediaProps),
    replace(Id, Map, Context);
replace(Id, MediaProps, Context) ->
    Mime = maps:get(<<"mime">>, MediaProps, undefined),
    Size = maps:get(<<"size">>, MediaProps, 1),
    case z_acl:rsc_editable(Id, Context) andalso
        z_acl:is_allowed(insert, #acl_media{mime = Mime, size = Size}, Context)
    of
        true ->
            Depicts = depicts(Id, Context),
            #media_upload_preprocess{ medium = Props1 } = set_av_flag(
                #media_upload_preprocess{
                    medium = MediaProps,
                    mime = Mime
                },
                Context),
            F = fun(Ctx) ->
                {ok, _} = medium_delete(Id, Ctx),
                {ok, Id} = medium_insert(Id, Props1#{ <<"id">> => Id }, Ctx)
            end,
            case z_db:transaction(F, Context) of
                {ok, _} ->
                    [z_depcache:flush(DepictId, Context) || DepictId <- Depicts],
                    z_depcache:flush(Id, Context),
                    Medium = get(Id, Context),
                    z_notifier:notify(#media_replace_file{id = Id, medium = Medium}, Context),
                    z_mqtt:publish(
                        [ <<"model">>, <<"media">>, <<"event">>, Id, <<"update">> ],
                        mqtt_event_info(Medium),
                        Context),
                    ok;
                {rollback, {error, _} = Error} ->
                    Error;
                {rollback, {{error, _} = Error, _Trace}} ->
                    Error;
                {rollback, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, eacces}
    end.


%% @doc Copy a medium onto an existing resource, replacing its current medium.
%% Authorize before copying files. File preparation happens outside the replacement
%% transaction; failed preparation leaves the destination untouched.
-spec copy(From, To, Context) -> ok | {error, term()} when
    From :: m_rsc:resource(),
    To :: m_rsc:resource(),
    Context :: z:context().
copy(From, To, Context) ->
    FromId = m_rsc:rid(From, Context),
    ToId = m_rsc:rid(To, Context),
    case m_rsc:exists(FromId, Context) andalso m_rsc:exists(ToId, Context) of
        false -> {error, enoent};
        true ->
            case z_acl:rsc_visible(FromId, Context) andalso z_acl:rsc_editable(ToId, Context) of
                false -> {error, eacces};
                true -> copy_visible_medium(FromId, ToId, Context)
            end
    end.

copy_visible_medium(FromId, ToId, Context) ->
    case z_db:qmap_props_row("select * from medium where id=$1", [FromId], Context) of
        {ok, Medium} ->
            Mime = maps:get(<<"mime">>, Medium, undefined),
            Size = maps:get(<<"size">>, Medium, 1),
            case z_acl:is_allowed(insert, #acl_media{mime=Mime, size=Size}, Context) of
                false -> {error, eacces};
                true when FromId =:= ToId -> ok;
                true -> copy_medium(ToId, Medium, Context)
            end;
        {error, _} = Error -> Error
    end.

copy_medium(ToId, Medium, Context) ->
    case maybe_duplicate_file(Medium, Context) of
        {ok, WithFile} ->
            with_copy_alias(<<"filename">>, Medium, WithFile, fun() ->
                case copy_medium_preview(WithFile, Context) of
                    {ok, Copied} ->
                        with_copy_alias(<<"preview_filename">>, Medium, Copied, fun() ->
                            replace(ToId, maps:without([<<"id">>, <<"created">>], Copied), Context)
                        end, Context);
                    {error, _} = Error -> Error
                end
            end, Context);
        {error, _} = Error -> Error
    end.

with_copy_alias(Key, Source, Copied, Fun, Context) ->
    try Fun() of
        ok -> ok;
        {error, _} = Error ->
            release_copy_alias(Key, Source, Copied, Context),
            Error
    catch
        Class:Reason:Stack ->
            release_copy_alias(Key, Source, Copied, Context),
            erlang:raise(Class, Reason, Stack)
    end.

%% Unlike resource duplication's best-effort preview, copying onto an existing
%% resource must not replace its medium with an incomplete copy.
copy_medium_preview(#{<<"preview_filename">> := Filename} = Medium, Context)
    when is_binary(Filename), Filename =/= <<>> ->
    Owned = maps:get(<<"is_deletable_preview">>, Medium, false),
    case duplicate_file(preview, Filename, Owned, Context) of
        {ok, NewFile} -> {ok, Medium#{<<"preview_filename">> => NewFile, <<"is_deletable_preview">> => true}};
        {error, _} = Error -> Error
    end;
copy_medium_preview(Medium, _Context) ->
    {ok, Medium}.

%% On failure retire only the new aliases, never the source's filenames.
%% After success leave cleanup to the normal medium recovery lifecycle.
release_copy_alias(Key, Source, Copied, Context) ->
    case maps:get(Key, Copied, undefined) of
        Filename when is_binary(Filename), Filename =/= <<>> ->
            case maps:get(Key, Source, undefined) of
                Filename -> ok;
                _ -> m_cafs:retire_filename(Filename, Context)
            end;
        _ -> ok
    end.


%% @doc Move a medium between resources, iff the destination doesn't have an associated medium.
%%      This is called when merging two resources (and the FromId is subsequently deleted).
merge(WinnerId, LoserId, Context) ->
    case z_acl:rsc_editable(LoserId, Context) andalso z_acl:rsc_editable(WinnerId, Context) of
        true ->
            case z_db:q1("select count(*) from medium where id = $1", [WinnerId], Context) of
                1 ->
                    ok;
                0 ->
                    case z_db:q1("select count(*) from medium where id = $1", [LoserId], Context) of
                        0 ->
                            ok;
                        1 ->
                            Depicts = depicts(LoserId, Context),
                            1 = z_db:q("update medium set id = $1 where id = $2",
                                [ WinnerId, LoserId ],
                                Context),
                            [ z_depcache:flush(DepictId, Context) || DepictId <- Depicts ],
                            z_depcache:flush(LoserId, Context),
                            z_depcache:flush(WinnerId, Context),
                            ok
                    end
            end;
        false ->
            {error, eacces}
    end.

%% @doc Duplicate the media item from the id to the new-id. Called by m_rsc:duplicate/3
-spec duplicate( m_rsc:resource(), m_rsc:resource(), z:context() ) -> ok | {error, term()}.
duplicate(FromId, ToId, Context) ->
    FromId1 = m_rsc:rid(FromId, Context),
    ToId1 = m_rsc:rid(ToId, Context),
    case z_db:qmap_props_row("select * from medium where id = $1", [FromId1], Context) of
        {ok, Ms} ->
            case maybe_duplicate_file(Ms, Context) of
                {ok, Ms1} ->
                    {ok, Ms2} = maybe_duplicate_preview(Ms1, Context),
                    case medium_insert(ToId1, Ms2, Context) of
                        {ok, _} -> ok;
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

maybe_duplicate_file(#{ <<"filename">> := <<>> } = Ms, _Context) ->
    {ok, Ms};
maybe_duplicate_file(#{ <<"filename">> := undefined } = Ms, _Context) ->
    {ok, Ms};
maybe_duplicate_file(#{ <<"filename">> := Filename } = Ms, Context) ->
    Owned = maps:get(<<"is_deletable_file">>, Ms, false),
    case duplicate_file(archive, Filename, Owned, Context) of
        {ok, NewFile} ->
            {ok, Hash} = m_cafs:lookup(NewFile, Context),
            RootName = z_string:truncatechars(
                filename:rootname(filename:basename(NewFile)),
                ?MEDIA_MAX_ROOTNAME_LENGTH, <<>>),
            Ms2 = Ms#{
                <<"filename">> => NewFile,
                <<"rootname">> => RootName,
                <<"digest">> => Hash,
                <<"is_deletable_file">> => true
            },
            {ok, Ms2};
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                in => zotonic_core,
                text => <<"Error duplicating medium archive file">>,
                filename => Filename,
                result => error,
                reason => Reason
            }),
            Error
    end.

maybe_duplicate_preview(#{ <<"preview_filename">> := <<>> } = Ms, _Context) ->
    {ok, Ms};
maybe_duplicate_preview(#{ <<"preview_filename">> := undefined } = Ms, _Context) ->
    {ok, Ms};
maybe_duplicate_preview(#{ <<"preview_filename">> := Filename } = Ms, Context) ->
    Owned = maps:get(<<"is_deletable_preview">>, Ms, false),
    case duplicate_file(preview, Filename, Owned, Context) of
        {ok, NewFile} ->
            Ms1 = Ms#{
                <<"preview_filename">> => NewFile,
                <<"is_deletable_preview">> => true
            },
            {ok, Ms1};
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => <<"Error duplicating preview">>,
                in => zotonic_core,
                result => error,
                reason => Reason,
                filename => Filename
            }),
            Ms1 = Ms#{
                <<"preview_filename">> => undefined,
                <<"is_deletable_preview">> => false
            },
            {ok, Ms1}
    end;
maybe_duplicate_preview(#{} = Ms, _Context) ->
    {ok, Ms}.

duplicate_file(Type, Filename, Owned, Context) ->
    NewFile = case Type of
        archive -> z_media_archive:archive_filename(filename:basename(Filename), Context);
        preview -> z_media_archive:preview_filename(filename:basename(Filename), Context)
    end,
    case m_cafs:clone_filename(Filename, NewFile, Context) of
        ok -> {ok, NewFile};
        {error, enoent} ->
            Roots = case Filename of <<"lib/", _/binary>> -> []; _ -> [archive] end,
            case z_file_request:lookup_file(Filename, Roots, [], Context) of
                {ok, FileInfo} ->
                    case z_file_request:content_file(FileInfo, Context) of
                        {ok, File} -> duplicate_legacy_file(Filename, File, NewFile, Owned, Context);
                        Error -> Error
                    end;
                Error -> Error
            end;
        Error -> Error
    end.

%% Owned archive files can be promoted in place. Non-owned references can be
%% mutable library assets: snapshot their bytes without masking the source path.
duplicate_legacy_file(Filename, File, NewFile, true, Context) ->
    case m_cafs:promote(Filename, File, Context) of
        {ok, _} ->
            case m_cafs:clone_filename(Filename, NewFile, Context) of
                ok -> {ok, NewFile};
                Error -> Error
            end;
        Error -> Error
    end;
duplicate_legacy_file(_Filename, File, NewFile, _Owned, Context) ->
    case m_cafs:stage_file(File, Context) of
        {ok, Staged} ->
            try
                case m_cafs:attach(NewFile, Staged, Context) of
                    ok -> {ok, NewFile};
                    Error -> Error
                end
            after
                m_cafs:release([{NewFile, Staged}], Context)
            end;
        Error -> Error
    end.

%% @doc Make a new resource for the file, when the file is not in the archive
%% dir then a copy is made in the archive dir
-spec insert_file(File, Context) -> {ok, RscId} | {error, term()} when
    File :: file:filename_all() | #upload{},
    Context :: z:context(),
    RscId :: m_rsc:resource_id().
insert_file(File, Context) ->
    insert_file(File, #{}, [], Context).

%% @doc Make a new resource for the file, when the file is not in the archive
%% dir then a copy is made in the archive dir. The given resource properties are
%% merged over the properties extracted from the file.
-spec insert_file(File, RscProps, Context) -> {ok, RscId} | {error, term()} when
    File :: file:filename_all() | #upload{},
    RscProps :: m_rsc:props_all(),
    Context :: z:context(),
    RscId :: m_rsc:resource_id().
insert_file(File, RscProps, Context) ->
    insert_file(File, RscProps, [], Context).

%% @doc Make a new resource for the file, when the file is not in the archive
%% dir then a copy is made in the archive dir. The given resource properties are
%% merged over the properties extracted from the file.
-spec insert_file(File, RscProps, Options, Context) -> {ok, RscId} | {error, term()} when
    File :: file:filename_all() | #upload{},
    RscProps :: m_rsc:props_all(),
    Options :: options(),
    Context :: z:context(),
    RscId :: m_rsc:resource_id().
insert_file(File, RscProps, Options, Context) when is_list(RscProps) ->
    {ok, PropsMap} = z_props:from_list(RscProps),
    insert_file(File, PropsMap, Options, Context);
insert_file(#upload{ data = Data, tmpfile = undefined } = Upload, RscProps, Options, Context) when Data =/= undefined ->
    TmpFile = z_tempfile:new(),
    case file:write_file(TmpFile, Data) of
        ok ->
            Result = insert_file(Upload#upload{ tmpfile = TmpFile }, RscProps, Options, Context),
            file:delete(TmpFile),
            Result;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                text => <<"Could not write temporary file">>,
                in => zotonic_core,
                result => error,
                reason => Reason,
                size => iolist_size(Data),
                filename => TmpFile
            }),
            file:delete(TmpFile),
            Error
    end;
insert_file(#upload{filename = OriginalFilename, tmpfile = TmpFile}, RscProps, Options, Context) ->
    case z_tempfile:is_tempfile(TmpFile) of
        true ->
            RscProps1 = RscProps#{
                <<"original_filename">> => OriginalFilename
            },
            MediaProps = #{
                <<"original_filename">> => OriginalFilename
            },
            MediaProps1 = add_medium_info(TmpFile, OriginalFilename, MediaProps, Context),
            insert_file(TmpFile, RscProps1, MediaProps1, Options, Context);
        false ->
            {error, upload_not_tempfile}
    end;
insert_file(File, RscProps, Options, Context) ->
    OriginalFilename = maps:get(<<"original_filename">>, RscProps, File),
    MediaProps = #{
        <<"original_filename">> => OriginalFilename
    },
    MediaProps1 = add_medium_info(File, OriginalFilename, MediaProps, Context),
    insert_file(File, RscProps, MediaProps1, Options, Context).

%% Internal helper used by the public insert_file entry points.
insert_file(File, RscProps, #{<<"mime">> := Mime} = MediaProps, Options, Context)
    when is_binary(Mime) ->
    MimeCat = mime_to_category(Mime, Options, Context),
    case z_acl:is_allowed(insert, #acl_rsc{category = MimeCat, props = RscProps}, Context) andalso
        z_acl:is_allowed(insert, #acl_media{mime = Mime, size = filelib:file_size(File)}, Context) of
        true ->
            insert_file_mime_ok(File, RscProps, MediaProps, Options, Context);
        false ->
            {error, file_not_allowed}
    end;
insert_file(_File, _RscProps, _MediaProps, _Options, _Context) ->
    %% Identification failed: do not pass an undefined MIME type to ACL observers.
    {error, identify}.

%% @doc Insert a medium, together with rsc props and an optional preview_url. This is used for importing media.
insert_medium(Medium, RscProps, Options, Context) ->
    update_medium_1(insert_rsc, Medium, RscProps, Options, Context).

replace_medium(Medium, RscId, RscProps, Options, Context) ->
    case z_acl:rsc_editable(RscId, Context) of
        true -> update_medium_1(RscId, Medium, RscProps, Options, Context);
        false -> {error, eacces}
    end.

update_medium_1(RscId, Medium, #{ <<"category">> := Cat } = RscProps, Options, Context) ->
    RscProps1 = maps:remove(<<"category">>, RscProps),
    RscProps2 = RscProps1#{ <<"category_id">> => Cat },
    update_medium_1(RscId, Medium, RscProps2, Options, Context);
update_medium_1(RscId, Medium, RscProps, Options, Context) ->
    case is_update_medium_allowed(RscId, Medium, RscProps, Context) of
        true ->
            #media_upload_preprocess{ medium = Medium1 } = set_av_flag(
                #media_upload_preprocess{
                    medium = Medium,
                    mime = maps:get(<<"mime">>, Medium)
                }, Context),
            case replace_file_acl_ok(undefined, RscId, RscProps, Medium1, Options, Context) of
                {ok, NewRscId} ->
                    case proplists:get_value(preview_url, Options) of
                        None when None =:= undefined; None =:= <<>>; None =:= [] ->
                            nop;
                        PreviewUrl ->
                            save_preview_url(NewRscId, PreviewUrl, Context)
                    end,
                    {ok, NewRscId};
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, file_not_allowed}
    end.

is_update_medium_allowed(insert_rsc, #{ <<"mime">> := Mime }, #{ <<"category_id">> := Category } = RscProps, Context) ->
    z_acl:is_allowed(insert, #acl_rsc{category = Category, props = RscProps}, Context)
    andalso z_acl:is_allowed(insert, #acl_media{mime=Mime, size=0}, Context);
is_update_medium_allowed(insert_rsc, _Medium, _RscProps, _Context) ->
    % No category - fail
    {error, no_category};
is_update_medium_allowed(RscId, #{ <<"mime">> := Mime }, #{ <<"category_id">> := Category } = RscProps, Context) ->
    % Changing the category, check insert rights for the new category
    z_acl:is_allowed(insert, #acl_rsc{id = RscId, category = Category, props = RscProps}, Context)
    andalso z_acl:is_allowed(insert, #acl_media{mime=Mime, size=0}, Context);
is_update_medium_allowed(_RscId, #{ <<"mime">> := Mime }, _RscProps, Context) ->
    % Update check was already done, only check the Mime type
    z_acl:is_allowed(insert, #acl_media{mime=Mime, size=0}, Context).

%% @doc Make a new resource for the file at a URL. The file is downloaded and then inserted as a
%% new media item.
-spec insert_url(Url, Context) -> {ok, RscId} | {error, term()} when
    Url :: media_url(),
    Context :: z:context(),
    RscId :: m_rsc:resource_id().
insert_url(Url, Context) ->
    insert_url(Url, #{}, [], Context).

%% @doc Make a new resource for the file at the URL. The given props are merged
%% over the properties extracted from the file at the URL.
-spec insert_url(Url, RscProps, Context) -> {ok, RscId} | {error, term()} when
    Url :: media_url(),
    RscProps :: m_rsc:props_all(),
    Context :: z:context(),
    RscId :: m_rsc:resource_id().
insert_url(Url, RscProps, Context) ->
    insert_url(Url, RscProps, [], Context).

%% @doc Make a new resource for the file at the URL. The given props are merged
%% over the properties extracted from the file at the URL. The options are passed
%% to the download function and the file insert function.
-spec insert_url(Url, RscProps, Options, Context) -> {ok, RscId} | {error, term()} when
    Url :: media_url(),
    RscProps :: m_rsc:props_all(),
    Options :: options(),
    Context :: z:context(),
    RscId :: m_rsc:resource_id().
insert_url(Url, RscProps, Options, Context) when is_list(RscProps) ->
    {ok, PropsMap} = z_props:from_list(RscProps),
    insert_url(Url, PropsMap, Options, Context);
insert_url(Url, RscProps, Options, Context) ->
    case download_file(Url, Options, Context) of
        {ok, TmpFile, Filename} ->
            RscProps1 = RscProps#{
                <<"original_filename">> => Filename
            },
            Result = insert_file(TmpFile, RscProps1, Options, Context),
            file:delete(TmpFile),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

%% Perform the resource management around inserting a file. The ACL is already checked for the mime type.
%% Runs the final insert inside a transaction so that we can rollback.
insert_file_mime_ok(File, RscProps, MediaProps, Options, Context) ->
    IsPublished = z_convert:to_bool( maps:get(<<"is_published">>, RscProps, true) ),
    RscProps1 = RscProps#{
        <<"is_published">> => IsPublished
    },
    replace_file_mime_ok(File, insert_rsc, RscProps1, MediaProps, Options, Context).

filename_to_title(undefined) -> <<>>;
filename_to_title("") -> <<>>;
filename_to_title(<<>>) -> <<>>;
filename_to_title(Filename) ->
    F1 = z_convert:to_binary(Filename),
    F2 = filename:basename(F1),
    F3 = lists:last(
    binary:split(F2, [ <<"/">>, <<"\\">> ], [global,trim])),
    filename:rootname(F3).

%% @doc Replaces a medium file, if the file is not in archive then a copy is
%% made in the archive. When the resource is in the media category, then the
%% category is adapted depending on the mime type of the uploaded file.
-spec replace_file(File, RscId, Context) -> {ok, RscId} | {error, term()} when
      File :: file:filename_all() | #upload{},
      RscId :: m_rsc:resource_id(),
      Context :: z:context().
replace_file(File, RscId, Context) ->
    replace_file(File, RscId, #{}, #{}, [], Context).

%% @doc Replaces a medium file, if the file is not in archive then a copy is
%% made in the archive. When the resource is in the media category, then the
%% category is adapted depending on the mime type of the uploaded file.
%% The resource is updated with the given RscProps.
-spec replace_file(File, RscId, RscProps, Context) -> {ok, RscId} | {error, term()} when
      File :: file:filename_all() | #upload{},
      RscId :: m_rsc:resource_id(),
      RscProps :: m_rsc:props_all(),
      Context :: z:context().
replace_file(File, RscId, RscProps, Context) ->
    replace_file(File, RscId, RscProps, #{}, [], Context).

%% @doc Replaces a medium file, if the file is not in archive then a copy is
%% made in the archive. When the resource is in the media category, then the
%% category is adapted depending on the mime type of the uploaded file.
%% The resource is updated with the given RscProps.
-spec replace_file(File, RscId, RscProps, Options, Context) -> {ok, RscId} | {error, term()} when
      File :: file:filename_all() | #upload{},
      RscId :: m_rsc:resource_id(),
      RscProps :: m_rsc:props_all(),
      Options :: options(),
      Context :: z:context().
replace_file(File, RscId, RscProps, Opts, Context) ->
    replace_file(File, RscId, RscProps, #{}, Opts, Context).

%% @doc Replaces a medium file, if the file is not in archive then a copy is
%% made in the archive. When the resource is in the media category, then the
%% category is adapted depending on the mime type of the uploaded file.
%% The resource is updated with the given RscProps.
-spec replace_file(File, RscId, RscProps, MediaInfo, Options, Context) -> {ok, RscId} | {error, term()} when
      File :: file:filename_all() | #upload{},
      RscId :: m_rsc:resource_id(),
      RscProps :: m_rsc:props_all(),
      MediaInfo :: z_media_identify:media_info() | list(),
      Options :: options(),
      Context :: z:context().
replace_file(File, RscId, RscProps, MediaInfo, Opts, Context) when is_list(RscProps) ->
    {ok, RscMap} = z_props:from_list(RscProps),
    replace_file(File, RscId, RscMap, MediaInfo, Opts, Context);
replace_file(File, RscId, RscProps, MediaInfo, Opts, Context) when is_list(MediaInfo) ->
    {ok, MediaInfoMap} = z_props:from_list(MediaInfo),
    replace_file(File, RscId, RscProps, MediaInfoMap, Opts, Context);
replace_file(
        #upload{filename = OriginalFilename, data = Data, tmpfile = undefined},
        RscId, RscProps, MInfo, Opts, Context) when Data =/= undefined ->
    TmpFile = z_tempfile:new(),
    ok = file:write_file(TmpFile, Data),
    replace_file(#upload{filename = OriginalFilename, tmpfile = TmpFile}, RscId, RscProps, MInfo, Opts, Context);
replace_file(#upload{filename = OriginalFilename, tmpfile = TmpFile}, RscId, RscProps, MInfo, Opts, Context) ->
    case z_tempfile:is_tempfile(TmpFile) of
        true ->
            MInfo1 = MInfo#{
                <<"original_filename">> => OriginalFilename
            },
            MediaProps = add_medium_info(TmpFile, OriginalFilename, MInfo1, Context),
            RscProps1 = RscProps#{
                <<"original_filename">> => OriginalFilename
            },
            replace_file_mime_check(TmpFile, RscId, RscProps1, MediaProps, Opts, Context);
        false ->
            {error, upload_not_tempfile}
    end;
replace_file(File, RscId, RscProps, MInfo, Opts, Context) when is_list(File) ->
    File1 = unicode:characters_to_binary(File),
    replace_file(File1, RscId, RscProps, MInfo, Opts, Context);
replace_file(File, RscId, RscProps, MInfo, Opts, Context) ->
    OriginalFilename = maps:get(<<"original_filename">>, RscProps, File),
    MInfo1 = MInfo#{
        <<"original_filename">> => OriginalFilename
    },
    MediaProps = add_medium_info(File, OriginalFilename, MInfo1, Context),
    replace_file_mime_check(File, RscId, RscProps, MediaProps, Opts, Context).

replace_file_mime_check(File, RscId, RscProps, #{<<"mime">> := Mime} = MediaProps, Opts, Context)
    when is_binary(Mime) ->
    case z_acl:is_allowed(insert, #acl_media{ mime = Mime, size = filelib:file_size(File) }, Context) of
        true ->
            replace_file_mime_ok(File, RscId, RscProps, MediaProps, Opts, Context);
        false ->
            {error, file_not_allowed}
    end;
replace_file_mime_check(_File, _RscId, _RscProps, _MediaProps, _Opts, _Context) ->
    {error, identify}.

%% @doc Check the ACL for the category/content-group combination.
replace_file_mime_ok(File, insert_rsc, RscProps, MediaProps, Opts, Context) ->
    % The category/content-group is also checked during the m_rsc:insert/2 call.
    % This is an early check to prevent preprocessing files for resources that
    % are not allowed to be created.
    CatId = case maps:find(<<"category_id">>, RscProps) of
        {ok, CId} when CId =/= undefined ->
            CId;
        error ->
            case maps:find(<<"category">>, RscProps) of
                {ok, CName} when CName =/= undefined ->
                    m_rsc:rid(CName, Context);
                error ->
                    Mime = maps:get(<<"mime">>, MediaProps, undefined),
                    m_rsc:rid(mime_to_category(Mime, Opts, Context), Context)
            end
    end,
    case m_category:id_to_name(CatId, Context) of
        undefined ->
            {error, eacces};
        Cat ->
            case z_acl:is_allowed(insert, #acl_rsc{ category = Cat, props = RscProps }, Context) of
                true ->
                    replace_file_acl_ok(File, insert_rsc, RscProps, MediaProps, Opts, Context);
                false ->
                    {error, eacces}
            end
    end;
replace_file_mime_ok(File, RscId, RscProps, MediaProps, Opts, Context) ->
    case z_acl:rsc_editable(RscId, Context) of
        true ->
            replace_file_acl_ok(File, RscId, RscProps, MediaProps, Opts, Context);
        false ->
            {error, eacces}
    end.

%% @doc Preprocess the data, examples are virus scanning and video preprocessing
replace_file_acl_ok(File, RscId, RscProps, Medium, Opts, Context) ->
    Mime = maps:get(<<"mime">>, Medium, undefined),
    OriginalFilename = maps:get(
        <<"original_filename">>,
        RscProps,
        maps:get(<<"original_filename">>, Medium, File)),
    PreProc = #media_upload_preprocess{
        id = RscId,
        mime = Mime,
        file = File,
        original_filename = OriginalFilename,
        medium = Medium
    },
    case notify_first_preproc(PreProc, true, Context) of
        {ok, PreProc1} ->
            PreProc2 = set_av_flag(PreProc1, Context),
            replace_file_sanitize(RscId, PreProc2, RscProps, Opts, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Set a flag that there was an av scan. This needs to be more generic.
set_av_flag( #media_upload_preprocess{ medium = Medium } = PreProc, Context ) ->
    IsAvScanned = not maps:get(<<"is_av_sizelimit">>, Medium, false)
        andalso lists:member(antivirus, z_module_manager:get_provided(Context)),
    Medium1 = Medium#{
        <<"is_av_scanned">> => IsAvScanned
    },
    PreProc#media_upload_preprocess{ medium = Medium1 }.

notify_first_preproc(PreProc, IsFirstTry, Context) ->
    case z_notifier:first(PreProc, Context) of
        undefined ->
            {ok, PreProc};
        #media_upload_preprocess{} = MappedPreProc ->
            {ok, MappedPreProc};
        {error, av_sizelimit} when IsFirstTry ->
            Medium1 = (PreProc#media_upload_preprocess.medium)#{
                <<"is_av_sizelimit">> => true
            },
            PreProc1 = PreProc#media_upload_preprocess{ medium = Medium1 },
            notify_first_preproc(PreProc1, false, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Clean up the uploaded data, removing bits that might be harmful.
replace_file_sanitize(RscId, PreProc, Props, Opts, Context) ->
    PreProc1 = z_media_sanitize:sanitize(PreProc, Context),
    replace_file_db(RscId, PreProc1, Props, Opts, Context).

-spec replace_file_db( m_rsc:resource_id() | insert_rsc, #media_upload_preprocess{}, map(), list(), z:context() ) ->
    {ok, m_rsc:resource_id()} | {error, term()}.
replace_file_db(RscId, PreProc, Props, Opts, Context) ->
    SafeRootName = z_string:truncate(
        z_string:to_rootname(PreProc#media_upload_preprocess.original_filename),
        ?MEDIA_MAX_ROOTNAME_LENGTH),
    PreferExtension = z_convert:to_binary(
        filename:extension(PreProc#media_upload_preprocess.original_filename)),
    Mime = z_convert:to_binary(PreProc#media_upload_preprocess.mime),
    SafeFilename = iolist_to_binary([
        SafeRootName, z_media_identify:extension(Mime, PreferExtension, Context)
    ]),
    ArchiveFile = case PreProc#media_upload_preprocess.file of
        undefined -> undefined;
        UploadFile -> z_media_archive:archive_copy_opt(UploadFile, SafeFilename, Context)
    end,
    RootName = case ArchiveFile of
        undefined -> undefined;
        _ -> filename:rootname(filename:basename(ArchiveFile))
    end,
    Medium0 = #{
        <<"mime">> => Mime,
        <<"filename">> => ArchiveFile,
        <<"rootname">> => RootName,
        <<"is_deletable_file">> => is_deletable_file(PreProc#media_upload_preprocess.file, Context)
    },
    Medium = maps:merge(Medium0, PreProc#media_upload_preprocess.medium),
    Medium1 = z_notifier:foldl(
        #media_upload_props{
            id = RscId,
            mime = Mime,
            archive_file = ArchiveFile,
            options = Opts
        },
        Medium,
        Context),

    PropsM = z_notifier:foldl(
        #media_upload_rsc_props{
            id = RscId,
            mime = Mime,
            archive_file = ArchiveFile,
            options = Opts,
            medium = Medium1
        },
        Props,
        Context),

    PropsM1 = case RscId =:= insert_rsc andalso z_utils:is_empty(maps:get(<<"title">>, PropsM, <<>>)) of
        true ->
            OriginalFilename = maps:get(<<"original_filename">>, Medium1, undefined),
            PropsM#{
                <<"title">> => filename_to_title(OriginalFilename)
            };
        false ->
            PropsM
    end,

    %% Only the fresh archive copy made by this upload is exclusively ours.
    %% Existing archive files and observer-supplied filenames keep verification.
    ControlledFilename = case is_deletable_file(PreProc#media_upload_preprocess.file, Context) of
        true -> ArchiveFile;
        false -> undefined
    end,
    case m_cafs:stage_medium(Medium1, ControlledFilename, Context) of
        {ok, Staged} ->
            replace_file_staged(RscId, PreProc, Props, PropsM1, Medium1, Opts, Staged, Context);
        {error, _} = Error -> Error
    end.

replace_file_staged(RscId, PreProc, Props, PropsM1, Medium1, Opts, Staged, Context) ->
    Mime = z_convert:to_binary(PreProc#media_upload_preprocess.mime),
    Digest = media_digest(maps:get(<<"filename">>, Medium1, undefined), Staged, Context),
    F = fun(Ctx) ->
        ok = check_expected_medium(RscId, Opts, Ctx),
        ok = m_cafs:attach_medium(Staged, Ctx),
        %% If the resource is in the media category, then move it to the correct sub-category depending
        %% on the mime type of the uploaded file.
        PropsCat = case maps:is_key(<<"category">>, PropsM1)
            orelse maps:is_key(<<"category_id">>, PropsM1)
        of
            true ->
                PropsM1;
            false ->
                PropsM1#{
                    <<"category_id">> => mime_to_category(Mime, Opts, Context)
                }
        end,
        {ok, Id} = case RscId of
            insert_rsc ->
                m_rsc_update:insert(PropsCat, Opts, Ctx);
            _ ->
                case rsc_is_media_cat(RscId, Context) of
                    true ->
                        {ok, RscId} = m_rsc_update:update(RscId, PropsCat, Opts, Ctx);
                    false when map_size(Props) > 0 ->
                        {ok, RscId} = m_rsc_update:update(RscId, Props, Opts, Ctx);
                    false ->
                        ok
                end,
                medium_delete(RscId, Ctx),
                {ok, RscId}
        end,
        Medium2 = Medium1#{
            <<"id">> => Id,
            <<"digest">> => Digest
        },
        case medium_insert(Id, Medium2, Ctx) of
            {ok, _MediaId} ->
                {ok, Id};
            Error ->
                % TODO: remove the created
                Error
        end
    end,

    Result = try z_db:transaction(F, Context)
        after m_cafs:release(Staged, Context) end,
    case Result of
        {ok, Id} ->
            Depicts = depicts(Id, Context),
            [z_depcache:flush(DepictId, Context) || DepictId <- Depicts],
            z_depcache:flush(Id, Context),

            %% Flush categories
            CatList = m_rsc:is_a(Id, Context),
            lists:foreach(
                fun(Cat) ->
                    z_depcache:flush(Cat, Context)
                end,
                CatList),

            _ = m_rsc:get(Id, Context), %% Prevent side effect that empty things are cached?

            % Run possible post insertion function.
            case PreProc#media_upload_preprocess.post_insert_fun of
                undefined -> ok;
                PostFun when is_function(PostFun, 3) ->
                    PostFun(Id, Medium1, Context)
            end,

            %% Pass the medium record along in the notification; this also fills the depcache (side effect).
            NewMedium = get(Id, Context),
            z_notifier:notify(#media_replace_file{id = Id, medium = NewMedium}, Context),
            z_mqtt:publish(
                [ <<"model">>, <<"media">>, <<"event">>, Id, <<"update">> ],
                mqtt_event_info(NewMedium),
                Context),
            lists:foreach(fun
                ({Name, #{cleanup := true}}) -> file:delete(z_media_archive:abspath(Name, Context));
                (_) -> ok
            end, Staged),
            {ok, Id};
        {rollback, {error, _} = Error} ->
            Error;
        {rollback, {{error, _} = Error, _StackTrace}} ->
            Error;
        {rollback, Reason} ->
            {error, Reason};
        {error, _} = Error ->
            Error
    end.

%% Non-owned archive files retain their existing digest behavior too. All file
%% hashing happens before the medium transaction.
media_digest(undefined, _Staged, _Context) -> undefined;
media_digest(Filename, Staged, Context) ->
    case lists:keyfind(Filename, 1, Staged) of
        {_, #{hash := Hash}} -> Hash;
        false ->
            case m_cafs:lookup(Filename, Context) of
                {ok, Hash} -> Hash;
                {error, enoent} ->
                    case z_crypto:hex_sha2_file(z_media_archive:abspath(Filename, Context)) of
                        {ok, Hash} -> Hash;
                        {error, _} -> undefined
                    end
            end
    end.

%% Serialize the final processing result with replacements, after expensive file
%% work has finished. Background jobs may only replace their own pending medium.
check_expected_medium(insert_rsc, _Opts, _Context) -> ok;
check_expected_medium(Id, Opts, Context) ->
    z_db:q1("select id from rsc where id=$1 for update", [Id], Context),
    case proplists:get_value(expected_medium, Opts) of
        undefined -> ok;
        {Key, Expected} ->
            case z_db:qmap_props_row("select * from medium where id=$1 for update", [Id], Context) of
                {ok, Medium} ->
                    case maps:get(Key, Medium, undefined) of
                        Expected -> ok;
                        _ -> throw({rollback, {error, stale_medium}})
                    end;
                _ -> throw({rollback, {error, stale_medium}})
            end
    end.

is_deletable_file(undefined, _Context) ->
    false;
is_deletable_file(File, Context) ->
    not z_media_archive:is_archived(File, Context).

%% @doc Replace a resource's medium with the contents found a the URL. After medium update
%% the resource is updated with the RscProps.
-spec replace_url(Url, RscId, RscProps, Context) -> {ok, RscId} | {error, term()} when
    Url :: media_url(),
    RscId :: m_rsc:resource_id(),
    RscProps :: m_rsc:props_all(),
    Context :: z:context().
replace_url(Url, RscId, RscProps, Context) ->
    replace_url(Url, RscId, RscProps, [], Context).

%% @doc Replace a resource's medium with the contents found a the URL. After medium update
%% the resource is updated with the RscProps.
-spec replace_url(Url, RscId, RscProps, Options, Context) -> {ok, RscId} | {error, term()} when
    Url :: media_url(),
    RscId :: m_rsc:resource_id(),
    RscProps :: m_rsc:props_all(),
    Options :: options(),
    Context :: z:context().
replace_url(Url, RscId, RscProps, Options, Context) when is_list(RscProps) ->
    {ok, PropsMap} = z_props:from_list(RscProps),
    replace_url(Url, RscId, PropsMap, Options, Context);
replace_url(Url, RscId, RscProps, Options, Context) ->
    case z_acl:rsc_editable(RscId, Context) of
        true ->
            case download_file(Url, Options, Context) of
                {ok, File, Filename} ->
                    RscProps1 = case maps:get(<<"original_filename">>, RscProps, undefined) of
                        F when is_binary(F), F =/= <<>> ->
                            RscProps;
                        _ ->
                            RscProps#{
                                <<"original_filename">> => Filename
                            }
                    end,
                    Result = replace_file(File, RscId, RscProps1, Options, Context),
                    file:delete(File),
                    Result;
                {error, E} ->
                    {error, E}
            end;
        false ->
            {error, eacces}
    end.

%% @doc Re-upload a file so that identify and previews are regenerated.
-spec reupload( m_rsc:resource_id(), z:context() ) -> {ok, m_rsc:resource_id()} | {error, term()}.
reupload(Id, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            case get(Id, Context) of
                undefined ->
                    {error, enoent};
                Medium ->
                    case maps:get(<<"size">>, Medium, 0) of
                        undefined -> {error, nofile};
                        0 -> {error, nofile};
                        _ -> reupload_1(Id, Medium, Context)
                    end
            end;
        false ->
            {error, eacces}
    end.

reupload_1(Id, #{ <<"filename">> := Filename } = Medium, Context) when is_binary(Filename), Filename =/= <<>> ->
    reupload_2(Id, Medium, Filename, z_file_request:lookup_file(Filename, Context), Context);
reupload_1(_Id, _Medium, _Context) ->
    {error, nofile}.

reupload_2(Id, Medium, Filename, {ok, #z_file_info{} = FInfo}, Context) ->
    case z_file_request:content_file(FInfo, Context) of
        {ok, MediaFile} ->
            TmpFile = z_tempfile:new(),
            % Copy file to temp file
            Result = case file:copy(MediaFile, TmpFile) of
                {ok, _} ->
                    OrgFilename = case maps:get(<<"original_filename">>, Medium, undefined) of
                        undefined -> filename:basename(Filename);
                        <<>> -> filename:basename(Filename);
                        Fn -> Fn
                    end,
                    Upload = #upload{
                        filename = OrgFilename,
                        tmpfile = TmpFile
                    },
                    replace_file(Upload, Id, Context);
                {error, _} = Error ->
                    Error
            end,
            file:delete(TmpFile),
            Result;
        {error, _} = Error ->
            Error
    end;
reupload_2(_Id, _Medium, _Filename, {error, _} = Error, _Context) ->
    Error.


-spec rsc_is_media_cat( m_rsc:resource_id(), z:context() ) -> boolean().
rsc_is_media_cat(Id, Context) ->
    case z_db:q1("select c.name from rsc c join rsc r on r.category_id = c.id where r.id = $1", [Id],
        Context) of
        <<"media">> -> true;
        <<"image">> -> true;
        <<"audio">> -> true;
        <<"video">> -> true;
        <<"document">> -> true;
        _ -> false
    end.

-spec mime_to_category(Mime) -> media | image | video | audio | document when
    Mime ::  string() | binary().
mime_to_category(Mime) ->
    case Mime of
        <<"image/", _/binary>> -> image;
        <<"video/", _/binary>> -> video;
        <<"text/html-video-embed">> -> video;
        <<"audio/", _/binary>> -> audio;
        <<"application/", _/binary>> -> document;
        <<"text/", _/binary>> -> document;

        "image/" ++ _ -> image;
        "video/" ++ _ -> video;
        "text/html-video-embed" -> video;
        "audio/" ++ _ -> audio;
        "application/" ++ _ -> document;
        "text/" ++ _ -> document;

        _ -> media
    end.

mime_to_category(Mime, Opts, Context) ->
    Cat = mime_to_category(Mime),
    case proplists:get_value(preferred_category, Opts) of
        undefined ->
            Cat;
        PrefCat ->
            case m_category:is_a(PrefCat, Cat, Context) of
                true ->
                    PrefCat;
                false ->
                    Cat
            end
    end.

%% @doc Download a file from a http or data url.
download_file(Url, Context) ->
    download_file(Url, [], Context).

download_file(Url, Options, Context) ->
    File = z_tempfile:new(),
    {ok, Device} = file:open(File, [write]),
    FetchOptions = proplists:get_value(fetch_options, Options, []),
    % Backwards compatible: also allow max_length and timeout as direct options.
    OptionsAll = Options ++ FetchOptions,
    MaxLength = proplists:get_value(max_length, OptionsAll, ?MEDIA_MAX_LENGTH_DOWNLOAD),
    Timeout = proplists:get_value(timeout, OptionsAll, ?MEDIA_TIMEOUT_DOWNLOAD),
    FetchOptions1 = [
        {max_length, MaxLength},
        {timeout, Timeout},
        {device, Device}
        | proplists:delete(max_length,
            proplists:delete(timeout,
                proplists:delete(device, FetchOptions)))
    ],
    Url1 = z_sanitize:uri(Url, true),
    case z_fetch:fetch_partial(Url1, FetchOptions1, Context) of
        {ok, {_FinalUrl, Hs, Length, _Data}} when Length < MaxLength ->
            file:close(Device),
            case proplists:get_value("content-length", Hs) of
                undefined ->
                    {ok, File, filename(Url, Hs)};
                "" ->
                    {ok, File, filename(Url, Hs)};
                CL ->
                    ContentLength = z_convert:to_integer(CL),
                    if
                        ContentLength =< Length ->
                            {ok, File, filename(Url, Hs)};
                        true ->
                            ?LOG_ERROR(#{
                                in => zotonic_core,
                                text => <<"File download was incomplete">>,
                                result => error,
                                reason => download_incomplete,
                                content_length => ContentLength,
                                download_length => Length,
                                url => Url
                            }),
                            file:delete(File),
                            {error, download_incomplete}
                    end
            end;
        {ok, {_FinalUrl, _Hs, Length, _Data}} when Length >= MaxLength ->
            file:close(Device),
            file:delete(File),
            ?LOG_ERROR(#{
                in => zotonic_core,
                text => <<"File download was too large">>,
                max_length => MaxLength,
                url => Url
            }),
            {error, file_too_large};
        {ok, Other} ->
            file:close(Device),
            file:delete(File),
            ?LOG_ERROR(#{
                in => zotonic_core,
                text => <<"File download failed">>,
                result => error,
                reason => download_failed,
                returned => Other,
                url => Url
            }),
            {error, download_failed};
        {error, Reason} = Error ->
            file:close(Device),
            file:delete(File),
            ?LOG_ERROR(#{
                in => zotonic_core,
                text => <<"File download failed">>,
                result => error,
                reason => Reason,
                url => Url
            }),
            Error
    end.

filename(Url, Hs) ->
    case z_url_metadata:filename(Url, Hs) of
        undefined ->
            {CT, _CTOpts} = content_type(Hs),
            mime2filename(CT);
        FN ->
            FN
    end.

content_type(Hs) ->
    case proplists:get_value("content-type", Hs) of
        undefined ->
            {<<"application/octet-stream">>, []};
        CT ->
            {Mime, Options} = mochiweb_util:parse_header(CT),
            {z_convert:to_binary(Mime), Options}
    end.

mime2filename(Mime) ->
    iolist_to_binary([filebase(Mime), z_media_identify:extension(Mime)]).

filebase(<<"video/", _/binary>>) -> <<"video">>;
filebase(<<"image/", _/binary>>) -> <<"image">>;
filebase(<<"audio/", _/binary>>) -> <<"audio">>;
filebase(<<"media/", _/binary>>) -> <<"media">>;
filebase(_) -> <<"document">>.

%% @doc Fetch the medium information of the file, if they are not set in the Props
add_medium_info(File, OriginalFilename, MediaProps, Context) ->
    PropsSize = case maps:get(<<"size">>, MediaProps, undefined) of
        undefined ->
            MediaProps#{
                <<"size">> => filelib:file_size(File)
            };
        _ ->
            MediaProps
    end,
    PropsMime = case maps:get(<<"mime">>, PropsSize, undefined) of
        undefined ->
            case z_media_identify:identify_file(File, OriginalFilename, Context) of
                {ok, MediaInfo} ->
                    maps:merge(MediaInfo, PropsSize);
                {error, _Reason} ->
                    PropsSize
            end;
        _ ->
            PropsSize
    end,
    PropsMime.


%% @doc Save a new file from a preview_url as the preview of a medium
save_preview_url(RscId, Url, Context) ->
    DownloadOptions = [
        {fetch_options, [ {max_length, ?MEDIA_MAX_LENGTH_PREVIEW} ]}
    ],
    case download_file(Url, DownloadOptions, Context) of
        {ok, TmpFile, Filename} ->
            case z_media_identify:identify_file(TmpFile, Filename, Context) of
                {ok, #{ <<"mime">> := <<"image/", _/binary>> } = MediaInfo} ->
                    try
                        Mime = maps:get(<<"mime">>, MediaInfo),
                        Width = maps:get(<<"width">>, MediaInfo),
                        Height = maps:get(<<"height">>, MediaInfo),

                        FileUnique = make_preview_unique(RscId, z_media_identify:extension(Mime), Context),
                        FileUniqueAbs = z_media_archive:abspath(FileUnique, Context),
                        ok = z_filelib:ensure_dir(FileUniqueAbs),
                        case file:rename(TmpFile, FileUniqueAbs) of
                            %% cross-fs rename is not supported by erlang, so copy and delete the file
                            {error, exdev} ->
                                {ok, _BytesCopied} = file:copy(TmpFile, FileUniqueAbs),
                                ok = file:delete(TmpFile);
                            ok ->
                                ok
                        end,
                        UpdateProps = #{
                            <<"preview_filename">> => FileUnique,
                            <<"preview_width">> => Width,
                            <<"preview_height">> => Height,
                            <<"is_deletable_preview">> => true
                        },
                        PreProps = m_media:get(RscId, Context),
                        PostProps = maps:merge(PreProps, UpdateProps),
                        {ok, PreviewStage} = m_cafs:stage_file(FileUniqueAbs, Context),
                        {ok, 1} = z_db:transaction(fun(Ctx) ->
                            ok = m_cafs:attach(FileUnique, PreviewStage, Ctx),
                            z_db:update(medium, RscId, UpdateProps, Ctx)
                        end, Context),
                        file:delete(FileUniqueAbs),
                        z_depcache:flush({medium, RscId}, Context),
                        IsA = m_rsc:is_a(RscId, Context),
                        z_notifier:notify(
                            #media_update_done{
                                action = update,
                                id = RscId,
                                pre_is_a = IsA,
                                post_is_a = IsA,
                                pre_props = PreProps,
                                post_props= PostProps
                            }, Context),
                        {ok, FileUnique}
                    catch
                        Type:Error ->
                            ?LOG_WARNING(#{
                                text => <<"Error importing preview">>,
                                in => zotonic_core,
                                result => Type,
                                reason => Error,
                                rsc_id => RscId,
                                url => Url,
                                mediainfo => MediaInfo
                            }),
                            file:delete(TmpFile),
                            {error, Error}
                    end;
                {ok, MediaInfo} ->
                    Mime = maps:get(<<"mime">>, MediaInfo, undefined),
                    ?LOG_WARNING(#{
                        text => <<"Error importing preview">>,
                        in => zotonic_core,
                        result => error,
                        reason => no_image,
                        rsc_id => RscId,
                        url => Url,
                        mime => Mime
                    }),
                    {error, no_image};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Save a preview for a medium record. The data is saved to a file in the archive directory.
-spec save_preview( m_rsc:resource_id(), iodata(), binary()|string(), z:context() ) ->
    {ok, file:filename_all()} | {error, eacces | term()}.
save_preview(RscId, Data, Mime, Context) ->
    case z_acl:rsc_editable(RscId, Context) of
        true ->
            FileUnique = make_preview_unique(RscId, z_media_identify:extension(Mime), Context),
            FileUniqueAbs = z_media_archive:abspath(FileUnique, Context),
            ok = z_filelib:ensure_dir(FileUniqueAbs),
            ok = file:write_file(FileUniqueAbs, Data),

            try
                {ok, MediaInfo} = z_media_identify:identify(FileUniqueAbs, Context),
                Width = maps:get(<<"width">>, MediaInfo),
                Height = maps:get(<<"height">>, MediaInfo),

                UpdateProps = #{
                    <<"preview_filename">> => FileUnique,
                    <<"preview_width">> => Width,
                    <<"preview_height">> => Height,
                    <<"is_deletable_preview">> => true
                },
                PreProps = m_media:get(RscId, Context),
                PostProps = maps:merge(PreProps, UpdateProps),
                {ok, PreviewStage} = m_cafs:stage_file(FileUniqueAbs, Context),
                {ok, 1} = z_db:transaction(fun(Ctx) ->
                    ok = m_cafs:attach(FileUnique, PreviewStage, Ctx),
                    z_db:update(medium, RscId, UpdateProps, Ctx)
                end, Context),
                file:delete(FileUniqueAbs),
                z_depcache:flush({medium, RscId}, Context),
                IsA = m_rsc:is_a(RscId, Context),
                z_notifier:notify(
                    #media_update_done{
                        action = update,
                        id = RscId,
                        pre_is_a = IsA,
                        post_is_a = IsA,
                        pre_props = PreProps,
                        post_props= PostProps
                    }, Context),
                {ok, FileUnique}
            catch
                throw:{error, _} = Error ->
                    file:delete(FileUniqueAbs),
                    Error
            end;
        false ->
            {error, eacces}
    end.

-spec make_preview_unique(RscId, Extension, Context) -> PreviewFilename when
    RscId :: m_rsc:resource_id() | insert_rsc,
    Extension :: binary(),
    Context :: z:context(),
    PreviewFilename :: file:filename_all().
make_preview_unique(RscId, Extension, Context) ->
    Basename = iolist_to_binary([id_to_list(RscId), $-, z_ids:identifier(16), Extension]),
    Filename = filename:join([
            "preview",
            z_ids:identifier(2),
            z_ids:identifier(2),
            Basename]),
    case is_unique_file(Filename, Context) of
        true ->
            Filename;
        false ->
            make_preview_unique(RscId, Extension, Context)
    end.

id_to_list(N) when is_integer(N) -> integer_to_list(N);
id_to_list(insert_rsc) -> "video".

%% @doc Check in the log of all previous files if the given filename is unique.
%% As file paths are cached by clients and the filestore can delay deletes
%% indefinitely, we must prevent clashes of filenames.
-spec is_unique_file(Filename, Context) -> boolean() when
    Filename :: file:filename_all(),
    Context :: z:context().
is_unique_file(Filename, Context) ->
    z_db:q1("select exists(select 1 from medium_log where filename=$1)
        or exists(select 1 from filename_cafs where filename=$1)", [Filename], Context) =:= false.

%% @doc Recover a pre-existing medium record. Update the created timestamp to now.
-spec recover_medium(Medium, Context) -> ok | {error, Reason} when
    Medium :: map(),
    Context :: z:context(),
    Reason :: term().
recover_medium(#{ <<"id">> := RscId } = Medium, Context) ->
    case z_db:q1("select count(*) from medium where id = $1", [ RscId ], Context) of
        1 -> delete(RscId, Context);
        0 -> ok
    end,
    Medium1 = Medium#{
        <<"created">> => calendar:universal_time()
    },
    case maybe_duplicate_file(Medium1, Context) of
        {ok, Medium2} ->
            {ok, Medium3} = maybe_duplicate_preview(Medium2, Context),
            case medium_insert(RscId, Medium3, Context) of
                {ok, _} -> ok;
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

medium_insert(Id, Props, Context) ->
    IsA = m_rsc:is_a(Id, Context),
    Props1 = check_medium_props(Props),
    Props2 = Props1#{
        <<"id">> => Id,
        <<"created">> => calendar:universal_time()
    },
    case z_db:insert(medium, Props2, Context) of
        {ok, _} = OK ->
            z_depcache:flush({medium, Id}, Context),
            PostProps = m_media:get(Id, Context),
            z_notifier:notify(
                #media_update_done{
                    action = insert,
                    id = Id,
                    post_is_a = IsA,
                    pre_is_a = [],
                    pre_props= #{},
                    post_props = PostProps
                }, Context),
            OK;
        {error, _} = Error ->
            Error
    end.

medium_delete(Id, Context) ->
    medium_delete(Id, get(Id, Context), Context).

medium_delete(_Id, undefined, _Context) ->
    {ok, 0};
medium_delete(Id, Props, Context) ->
    IsA = m_rsc:is_a(Id, Context),
    case z_db:delete(medium, Id, Context) of
        {ok, _} = OK ->
            z_depcache:flush({medium, Id}, Context),
            z_notifier:notify(
                #media_update_done{
                    action = delete,
                    id = Id,
                    pre_is_a = IsA,
                    post_is_a = [],
                    pre_props = Props,
                    post_props= #{}
                }, Context),
            OK;
        {error, _} = Error ->
            Error
    end.

check_medium_props(Ps) ->
    maps:fold(
        fun(K, V, Acc) ->
            V1 = check_medium_prop(K, V),
            Acc#{
                K => V1
            }
        end,
        #{},
        Ps).

%% Sanitize and bound the stored original filename at the database boundary.
%% Strip Unix and Windows paths; preserve Unicode letters, marks and numbers.
check_medium_prop(<<"original_filename">>, Filename) when is_binary(Filename); is_list(Filename) ->
    Utf8Filename = z_string:sanitize_utf8(z_convert:to_binary(Filename)),
    Basename = filename:basename(binary:replace(Utf8Filename, <<"\\">>, <<"/">>, [global])),
    OriginalFilename = re:replace(Basename, <<"[^\\p{L}\\p{M}\\p{N} ._()-]">>, <<"_">>,
        [unicode, global, {return, binary}]),
    RootName = filename:rootname(OriginalFilename),
    Extension = filename:extension(OriginalFilename),
    TruncatedRoot = string:slice(RootName, 0, ?MEDIA_MAX_ROOTNAME_LENGTH),
    <<TruncatedRoot/binary, Extension/binary>>;
check_medium_prop(<<"width">>, N) when not is_integer(N) -> 0;
check_medium_prop(<<"height">>, N) when not is_integer(N) -> 0;
check_medium_prop(B, P) when is_binary(B) -> P.


% Return a map with basic (not too sensitive) medium info for MQTT events
-spec mqtt_event_info( map() ) -> map().
mqtt_event_info(Medium) ->
    lists:foldl(
        fun(K, Acc) ->
            Acc#{
                K => maps:get(K, Medium, undefined)
            }
        end,
        #{},
        [
            id, size, width, height,
            orientation, mime, filename
        ]).
