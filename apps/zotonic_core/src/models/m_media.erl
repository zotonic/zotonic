%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Model for medium database
%% @end

%% Copyright 2009-2024 Marc Worrell
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
    replace/3,
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

-export_type([media_url/0]).

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
-spec identify( m_rsc:resource_id(), z:context() ) -> {ok, z_media_identify:media_info()} | {error, term()}.
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


%% @doc Delete the medium at the id.  The file is queued for later deletion.
-spec delete( m_rsc:resource_id(), z:context() ) -> ok  | {error, term()}.
delete(Id, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            Depicts = depicts(Id, Context),
            medium_delete(Id, Context),
            [z_depcache:flush(DepictId, Context) || DepictId <- Depicts],
            m_rsc:touch(Id, Context),
            z_notifier:notify(#media_replace_file{id = Id, medium = []}, Context),
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
-spec replace( m_rsc:resource_id(), map(), z:context() ) -> ok  | {error, term()}.
replace(Id, Props, Context) when is_list(Props) ->
    {ok, Map} = z_props:from_list(Props),
    replace(Id, Map, Context);
replace(Id, Props, Context) ->
    Mime = maps:get(<<"mime">>, Props, undefined),
    Size = maps:get(<<"size">>, Props, 1),
    case z_acl:rsc_editable(Id, Context) andalso
        z_acl:is_allowed(insert, #acl_media{mime = Mime, size = Size}, Context)
    of
        true ->
            Depicts = depicts(Id, Context),
            #media_upload_preprocess{ medium = Props1 } = set_av_flag(
                #media_upload_preprocess{
                    medium = Props,
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
                {rollback, {Error, _Trace}} ->
                    {error, Error}
            end;
        false ->
            {error, eacces}
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
maybe_duplicate_file(#{ <<"filename">> := _, <<"is_deletable_file">> := false } = Ms, _Context) ->
    {ok, Ms};
maybe_duplicate_file(#{ <<"filename">> := Filename, <<"is_deletable_file">> := true } = Ms, Context) ->
    case duplicate_file(archive, Filename, Context) of
        {ok, NewFile} ->
            RootName = z_string:truncatechars(
                filename:rootname(filename:basename(NewFile)),
                ?MEDIA_MAX_ROOTNAME_LENGTH),
            Ms2 = Ms#{
                <<"filename">> => NewFile,
                <<"rootname">> => RootName,
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
maybe_duplicate_preview(#{ <<"preview_filename">> := _, <<"is_deletable_preview">> := false  } = Ms, _Context) ->
    {ok, Ms};
maybe_duplicate_preview(#{ <<"preview_filename">> := Filename, <<"is_deletable_preview">> := true } = Ms, Context) ->
    case duplicate_file(preview, Filename, Context) of
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
            Ms1 = maps:remove(<<"preview_filename">>, Ms),
            Ms2 = maps:remove(<<"is_deletable_preview">>, Ms1),
            {ok, Ms2}
    end.


duplicate_file(Type, Filename, Context) ->
    case z_file_request:lookup_file(Filename, Context) of
        {ok, FileInfo} ->
            {ok, File} = z_file_request:content_file(FileInfo, Context),
            {ok, z_media_archive:archive_copy(Type, File, filename:basename(Filename), Context)};
        {error, _} = Error ->
            Error
    end.

%% @doc Make a new resource for the file, when the file is not in the archive
%% dir then a copy is made in the archive dir
-spec insert_file(file:filename_all() | #upload{}, z:context()) -> {ok, m_rsc:resource_id()} | {error, term()}.
insert_file(File, Context) ->
    insert_file(File, #{}, [], Context).

-spec insert_file(file:filename_all() | #upload{}, m_rsc:props_all(), z:context()) -> {ok, m_rsc:resource_id()} | {error, term()}.
insert_file(File, RscProps, Context) ->
    insert_file(File, RscProps, [], Context).

-spec insert_file(file:filename_all() | #upload{}, m_rsc:props_all(), list(), z:context()) -> {ok, m_rsc:resource_id()} | {error, term()}.
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

insert_file(File, RscProps, MediaProps, Options, Context) ->
    Mime = maps:get(<<"mime">>, MediaProps, undefined),
    MimeCat = mime_to_category(Mime, Options, Context),
    case z_acl:is_allowed(insert, #acl_rsc{category = MimeCat, props = RscProps}, Context) andalso
        z_acl:is_allowed(insert, #acl_media{mime = Mime, size = filelib:file_size(File)}, Context) of
        true ->
            insert_file_mime_ok(File, RscProps, MediaProps, Options, Context);
        false ->
            {error, file_not_allowed}
    end.

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



%% @doc Make a new resource for the file based on a URL.
-spec insert_url(media_url(), z:context()) -> {ok, pos_integer()} | {error, term()}.
insert_url(Url, Context) ->
    insert_url(Url, #{}, [], Context).

-spec insert_url(media_url(), m_rsc:props_all(), z:context()) -> {ok, pos_integer()} | {error, term()}.
insert_url(Url, RscProps, Context) ->
    insert_url(Url, RscProps, [], Context).

-spec insert_url(media_url(), m_rsc:props_all(), list(), z:context()) -> {ok, pos_integer()} | {error, term()}.
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

%% @doc Replaces a medium file, when the file is not in archive then a copy is
%% made in the archive. When the resource is in the media category, then the
%% category is adapted depending on the mime type of the uploaded file.
replace_file(File, RscId, Context) ->
    replace_file(File, RscId, #{}, #{}, [], Context).

replace_file(File, RscId, RscProps, Context) ->
    replace_file(File, RscId, RscProps, #{}, [], Context).

replace_file(File, RscId, RscProps, Opts, Context) ->
    replace_file(File, RscId, RscProps, #{}, Opts, Context).

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

replace_file_mime_check(File, RscId, RscProps, MediaProps, Opts, Context) ->
    Mime = maps:get(<<"mime">>, MediaProps, undefined),
    case z_acl:is_allowed(insert, #acl_media{ mime = Mime, size = filelib:file_size(File) }, Context) of
        true ->
            replace_file_mime_ok(File, RscId, RscProps, MediaProps, Opts, Context);
        false ->
            {error, file_not_allowed}
    end.

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

    F = fun(Ctx) ->
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
        Medium2 = Medium1#{ <<"id">> => Id },
        case medium_insert(Id, Medium2, Ctx) of
            {ok, _MediaId} ->
                {ok, Id};
            Error ->
                % TODO: remove the created
                Error
        end
    end,

    case z_db:transaction(F, Context) of
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
            {ok, Id};
        {rollback, {{error, Reason}, _StackTrace}} ->
            {error, Reason}
    end.

is_deletable_file(undefined, _Context) ->
    false;
is_deletable_file(File, Context) ->
    not z_media_archive:is_archived(File, Context).

replace_url(Url, RscId, RscProps, Context) ->
    replace_url(Url, RscId, RscProps, [], Context).

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

-spec mime_to_category( string() | binary() ) -> image | video | audio | document.
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
    case z_fetch:fetch_partial(Url, FetchOptions1, Context) of
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
                        {ok, 1} = z_db:update(medium, RscId, UpdateProps, Context),
                        z_depcache:flush({medium, RscId}, Context),
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
                {ok, 1} = z_db:update(medium, RscId, UpdateProps, Context),
                z_depcache:flush({medium, RscId}, Context),
                {ok, FileUnique}
            catch
                throw:{error, _} = Error ->
                    file:delete(FileUniqueAbs),
                    Error
            end;
        false ->
            {error, eacces}
    end.

-spec make_preview_unique(integer()|insert_rsc, binary(), z:context()) -> file:filename().
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

is_unique_file(Filename, Context) ->
    z_db:q1("select count(*) from medium_log where filename = $1", [Filename], Context) =:= 0.

medium_insert(Id, Props, Context) ->
    IsA = m_rsc:is_a(Id, Context),
    Props1 = check_medium_props(Props),
    case z_db:insert(medium, Props1#{ <<"id">> => Id }, Context) of
        {ok, _} = OK ->
            z_notifier:notify(#media_update_done{action=insert, id=Id, post_is_a=IsA, pre_is_a=[], pre_props=#{}, post_props=Props1}, Context),
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
            z_notifier:notify(#media_update_done{action=delete, id=Id, pre_is_a=IsA, post_is_a=[], pre_props=Props, post_props=#{}}, Context),
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
