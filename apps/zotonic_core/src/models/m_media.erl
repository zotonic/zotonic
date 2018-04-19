%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2014 Marc Worrell
%% @doc Model for medium database

%% Copyright 2009-2014 Marc Worrell
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
    m_get/2,
    identify/2,
    get/2,
    get_file_data/2,
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
    save_preview_url/3,
    save_preview/4,
    make_preview_unique/3,
    is_unique_file/2,
    download_file/1,
    download_file/2,
    mime_to_category/1
]).

-include_lib("zotonic.hrl").

-define(MEDIA_MAX_LENGTH_PREVIEW, 10 * 1024 * 1024).
-define(MEDIA_MAX_LENGTH_DOWNLOAD, 500 * 1024 * 1024).
-define(MEDIA_TIMEOUT_DOWNLOAD, 60 * 1000).


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([ Id | Rest ], Context) ->
    Media = case z_acl:rsc_visible(Id, Context) of
        true -> get(Id, Context);
        false -> undefined
    end,
    {Media, Rest};
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.


%% @doc Return the identification of a medium. Used by z_media_identify:identify()
%% @spec identify(ImageFilePath, Context) -> {ok, PropList} | {error, Reason}
identify(Id, Context) when is_integer(Id) ->
    case z_db:assoc_row("select id, mime, width, height, orientation from medium where id = $1", [Id],
        Context) of
        undefined ->
            {error, enoent};
        Props ->
            {ok, Props}
    end;
identify(ImageFile, Context) ->
    case z_media_archive:is_archived(ImageFile, Context) of
        true ->
            RelFile = z_media_archive:rel_archive(ImageFile, Context),
            identify_medium_filename(RelFile, Context);
        false ->
            identify_medium_filename(ImageFile, Context)
    end.

identify_medium_filename(MediumFilename, Context) ->
    case z_db:assoc_row("select id, mime, width, height, orientation from medium where filename = $1",
        [MediumFilename], Context) of
        undefined ->
            {error, enoent};
        Props ->
            {ok, Props}
    end.


%% @doc Check if a medium record exists
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
%% @spec get(RscId, Context) -> PropList
get(Id, Context) ->
    F = fun() ->
        z_db:assoc_props_row("select * from medium where id = $1", [Id], Context) end,
    z_depcache:memo(F, {medium, Id}, ?WEEK, [Id], Context).

%% @doc Return the contents of the file belonging to the media resource
get_file_data(Id, Context) ->
    case get(Id, Context) of
        undefined ->
            {error, enoent};
        Media ->
            Filename = proplists:get_value(filename, Media),
            ArchivedFilename = z_media_archive:abspath(Filename, Context),
            case file:read_file(ArchivedFilename) of
                {ok, Data} ->
                    #upload{
                        filename = Filename,
                        data = Data,
                        mime = proplists:get_value(mime, Media)
                    };
                Error ->
                    Error
            end
    end.

%% @doc Fetch a medium by filename
get_by_filename(Filename, Context) ->
    case z_depcache:get({medium, Filename}, Context) of
        undefined ->
            Row = z_db:assoc_props_row("select * from medium where filename = $1", [Filename], Context),
            case Row of
                undefined ->
                    z_depcache:set({medium, Filename}, undefined, ?HOUR, Context);
                _L ->
                    z_depcache:set({medium, Filename}, Row, ?HOUR, [proplists:get_value(id, Row)], Context)
            end,
            Row;
        {ok, Row} ->
            Row
    end.


%% @doc Get the medium record that depicts the resource id. "depiction" Predicates are preferred, when
%% they are missing then the attached medium record itself is returned.  We must be able to generate a preview
%% from the medium.
%% @spec depiction(RscId, Context) -> PropList | undefined
depiction(Id, Context) ->
    try
        z_depcache:memo(
            fun() -> depiction([Id], [], Context) end,
            {depiction, Id},
            ?WEEK, [Id], Context)
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
                        Props when is_list(Props) ->
                            Props
                    end;
                Props when is_list(Props) ->
                    Props
            end
    end.


%% @doc Return the list of resources that is depicted by the medium (excluding the rsc itself)
%% @spec depicts(RscId, Context) -> [Id]
depicts(Id, Context) ->
    m_edge:subjects(Id, depiction, Context).


%% @doc Delete the medium at the id.  The file is queued for later deletion.
%% @spec delete(RscId, Context) -> ok | {error, Reason}
delete(Id, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            Depicts = depicts(Id, Context),
            medium_delete(Id, Context),
            [z_depcache:flush(DepictId, Context) || DepictId <- Depicts],
            z_depcache:flush(Id, Context),
            z_notifier:notify(#media_replace_file{id = Id, medium = []}, Context),
            ok;
        false ->
            {error, eacces}
    end.


%% @doc Replace or insert a medium record for the page.  This is useful for non-file related media.
%% Resets all non mentioned attributes.
%% @spec replace(Id, Props, Context) -> ok | {error, Reason}
replace(Id, Props, Context) ->
    Mime = proplists:get_value(mime, Props),
    Size = proplists:get_value(size, Props, 0),
    case z_acl:rsc_editable(Id, Context) andalso
        z_acl:is_allowed(insert, #acl_media{mime = Mime, size = Size}, Context)
    of
        true ->
            Depicts = depicts(Id, Context),
            F = fun(Ctx) ->
                {ok, _} = medium_delete(Id, Ctx),
                {ok, Id} = medium_insert(Id, [{id, Id} | Props], Ctx)
            end,
            case z_db:transaction(F, Context) of
                {ok, _} ->
                    [z_depcache:flush(DepictId, Context) || DepictId <- Depicts],
                    z_depcache:flush(Id, Context),
                    z_notifier:notify(#media_replace_file{id = Id, medium = get(Id, Context)}, Context),
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
                                [WinnerId, LoserId],
                                Context),
                            [z_depcache:flush(DepictId, Context) || DepictId <- Depicts],
                            z_depcache:flush(LoserId, Context),
                            z_depcache:flush(WinnerId, Context),
                            ok
                    end
            end;
        false ->
            {error, eacces}
    end.

%% @doc Duplicate the media item from the id to the new-id. Called by m_rsc:duplicate/3
-spec duplicate(m_rsc:resource(), m_rsc:resource(), #context{}) -> ok.
duplicate(FromId, ToId, Context) ->
    case z_db:assoc_props_row("select * from medium where id = $1", [m_rsc:rid(FromId, Context)], Context) of
        Ms when is_list(Ms) ->
            {ok, Ms1} = maybe_duplicate_file(Ms, Context),
            {ok, Ms2} = maybe_duplicate_preview(Ms1, Context),
            Ms3 = z_utils:prop_replace(id, ToId, Ms2),
            {ok, _ToId} = medium_insert(ToId, Ms3, Context),
            ok;
        undefined ->
            ok
    end.

maybe_duplicate_file(Ms, Context) ->
    case proplists:get_value(filename, Ms) of
        <<>> ->
            {ok, Ms};
        undefined ->
            {ok, Ms};
        Filename ->
            case proplists:get_value(is_deletable_file, Ms) of
                false ->
                    {ok, Ms};
                true ->
                    {ok, NewFile} = duplicate_file(archive, Filename, Context),
                    RootName = filename:rootname(filename:basename(NewFile)),
                    Ms1 = proplists:delete(filename,
                        proplists:delete(rootname,
                            proplists:delete(is_deletable_file, Ms))),
                    Ms2 = [
                        {filename, NewFile},
                        {rootname, RootName},
                        {is_deletable_file, true}
                        | Ms1
                    ],
                    {ok, Ms2}
            end
    end.

maybe_duplicate_preview(Ms, Context) ->
    case proplists:get_value(preview_filename, Ms) of
        <<>> ->
            {ok, Ms};
        undefined ->
            {ok, Ms};
        Filename ->
            case proplists:get_value(is_deletable_preview, Ms) of
                false ->
                    {ok, Ms};
                true ->
                    case duplicate_file(preview, Filename, Context) of
                        {ok, NewFile} ->
                            Ms1 = proplists:delete(preview_filename,
                                proplists:delete(is_deletable_preview, Ms)),
                            Ms2 = [
                                {preview_filename, NewFile},
                                {is_deletable_preview, true}
                                | Ms1
                            ],
                            {ok, Ms2};
                        {error, _} = Error ->
                            lager:error("Duplicate preview: error ~p for preview file ~p",
                                [Error, Filename]),
                            Ms1 = proplists:delete(preview_filename,
                                proplists:delete(is_deletable_preview, Ms)),
                            {ok, Ms1}
                    end
            end
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
-spec insert_file(term(), z:context()) -> {ok, Id :: m_rsc:resource_id()} | {error, term()}.
insert_file(File, Context) ->
    insert_file(File, [], Context).
insert_file(#upload{filename = OriginalFilename, data = Data, tmpfile = undefined}, Props, Context)
    when Data /= undefined ->
    TmpFile = z_tempfile:new(),
    ok = file:write_file(TmpFile, Data),
    insert_file(#upload{filename = OriginalFilename, tmpfile = TmpFile}, Props, [], Context);
insert_file(File, Props, Context) ->
    insert_file(File, Props, [], Context).


insert_file(#upload{filename = OriginalFilename, tmpfile = TmpFile}, Props, Options, Context) ->
    PropsMedia = add_medium_info(TmpFile, OriginalFilename, [{original_filename, OriginalFilename}], Context),
    insert_file(TmpFile, [{original_filename, OriginalFilename} | Props], PropsMedia, Options, Context);
insert_file(File, Props, Options, Context) ->
    OriginalFilename = proplists:get_value(original_filename, Props, File),
    PropsMedia = add_medium_info(File, OriginalFilename, [{original_filename, OriginalFilename}], Context),
    insert_file(File, Props, PropsMedia, Options, Context).

insert_file(File, Props, PropsMedia, Options, Context) ->
    Mime = proplists:get_value(mime, PropsMedia),
    case z_acl:is_allowed(insert, #acl_rsc{category = mime_to_category(Mime), props = Props}, Context) andalso
        z_acl:is_allowed(insert, #acl_media{mime = Mime, size = filelib:file_size(File)}, Context) of
        true ->
            insert_file_mime_ok(File, Props, PropsMedia, Options, Context);
        false ->
            {error, file_not_allowed}
    end.

%% @doc Insert a medium, together with rsc props and an optional preview_url. This is used for importing media
insert_medium(Medium, RscProps, Options, Context) ->
    update_medium_1(insert_rsc, Medium, RscProps, Options, Context).

replace_medium(Medium, RscId, RscProps, Options, Context) ->
    case z_acl:rsc_editable(RscId, Context) of
        true -> update_medium_1(RscId, Medium, RscProps, Options, Context);
        false -> {error, eacces}
    end.

update_medium_1(RscId, Medium, RscProps, Options, Context) ->
    {mime, Mime} = proplists:lookup(mime, Medium),
    Category = case proplists:get_value(category, RscProps) of
                    undefined -> tl(m_rsc:is_a(RscId, Context));
                    Cat -> Cat
               end,
    case z_acl:is_allowed(insert, #acl_rsc{category = Category, props = RscProps}, Context) andalso
         z_acl:is_allowed(insert, #acl_media{mime=Mime, size=0}, Context) of
        true ->
            case replace_file_acl_ok(undefined, RscId, RscProps, Medium, Options, Context) of
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


%% @doc Make a new resource for the file based on a URL.
%% @spec insert_url(File, Context) -> {ok, Id} | {error, Reason}
-spec insert_url(file:filename(), #context{}) -> {ok, pos_integer()} | {error, term()}.
insert_url(Url, Context) ->
    insert_url(Url, [], Context).

insert_url(Url, Props, Context) ->
    insert_url(Url, Props, [], Context).

insert_url(Url, Props, Options, Context) ->
    case download_file(Url) of
        {ok, TmpFile, Filename} ->
            Result = insert_file(TmpFile, [{original_filename, Filename} | Props], Options, Context),
            file:delete(TmpFile),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

%% Perform the resource management around inserting a file. The ACL is already checked for the mime type.
%% Runs the final insert inside a transaction so that we can rollback.
insert_file_mime_ok(File, Props1, PropsMedia, Options, Context) ->
    Props2 = case proplists:get_value(is_published, Props1) of
        undefined -> [{is_published, true} | Props1];
        _ -> Props1
    end,
    Props3 = case z_utils:is_empty(proplists:get_value(title, Props2)) of
        true ->
            [{title, filename_basename(proplists:get_value(original_filename, PropsMedia))} | Props2];
        false -> Props2
    end,
    replace_file_mime_ok(File, insert_rsc, Props3, PropsMedia, Options, Context).

filename_basename(undefined) -> <<>>;
filename_basename(Filename) ->
    F1 = z_convert:to_binary(Filename),
    F2 = lists:last(binary:split(F1, <<"/">>, [global])),
    lists:last(binary:split(F2, <<"\\">>, [global])).

%% @doc Replaces a medium file, when the file is not in archive then a copy is
%% made in the archive. When the resource is in the media category, then the
%% category is adapted depending on the mime type of the uploaded file.
%% @spec replace_file(File, RscId, Context) -> {ok, Id} | {error, Reason}
replace_file(File, RscId, Context) ->
    replace_file(File, RscId, [], [], Context).

replace_file(File, RscId, Props, Context) ->
    replace_file(File, RscId, Props, [], Context).

replace_file(File, RscId, Props, Opts, Context) ->
    replace_file(File, RscId, Props, [], Opts, Context).

replace_file(#upload{filename = OriginalFilename, data = Data, tmpfile = undefined},
    RscId, Props, MInfo, Opts, Context) when Data /= undefined ->
    TmpFile = z_tempfile:new(),
    ok = file:write_file(TmpFile, Data),
    replace_file(#upload{filename = OriginalFilename, tmpfile = TmpFile}, RscId, Props, MInfo, Opts, Context);
replace_file(#upload{filename = OriginalFilename, tmpfile = TmpFile}, RscId, Props, MInfo, Opts, Context) ->
    PropsMedia = add_medium_info(TmpFile, OriginalFilename, [{original_filename, OriginalFilename} | MInfo],
        Context),
    replace_file_mime_check(TmpFile, RscId, [{original_filename, OriginalFilename} | Props], PropsMedia, Opts,
        Context);
replace_file(File, RscId, Props, MInfo, Opts, Context) ->
    OriginalFilename = proplists:get_value(original_filename, Props, File),
    PropsMedia = add_medium_info(File, OriginalFilename, [{original_filename, OriginalFilename} | MInfo],
        Context),
    replace_file_mime_check(File, RscId, Props, PropsMedia, Opts, Context).

replace_file_mime_check(File, RscId, Props, PropsMedia, Opts, Context) ->
    Mime = proplists:get_value(mime, PropsMedia),
    case z_acl:is_allowed(insert, #acl_rsc{category = mime_to_category(Mime), props = Props}, Context) andalso
        z_acl:is_allowed(insert, #acl_media{mime = Mime, size = filelib:file_size(File)}, Context) of
        true ->
            replace_file_mime_ok(File, RscId, Props, PropsMedia, Opts, Context);
        false ->
            {error, file_not_allowed}
    end.

%% @doc Replace the file, no mime check needed.
replace_file_mime_ok(File, RscId, Props, PropsMedia, Opts, Context) ->
    case RscId == insert_rsc
        orelse z_acl:rsc_editable(RscId, Context)
        orelse not(m_rsc:p(RscId, is_authoritative, Context)) of
        true ->
            replace_file_acl_ok(File, RscId, Props, PropsMedia, Opts, Context);
        false ->
            {error, eacces}
    end.

replace_file_acl_ok(File, RscId, Props, Medium, Opts, Context) ->
    Mime = z_convert:to_binary(proplists:get_value(mime, Medium)),
    PreProc = #media_upload_preprocess{
        id = RscId,
        mime = Mime,
        file = File,
        original_filename = proplists:get_value(original_filename, Props,
            proplists:get_value(original_filename, Medium, File)),
        medium = Medium
    },
    NewPreProc = case z_notifier:first(PreProc, Context) of
                    undefined -> PreProc;
                    #media_upload_preprocess{} = Mapped -> Mapped
                 end,
    NewPreProc2 = z_media_sanitize:sanitize(NewPreProc, Context),
    replace_file_db(RscId, NewPreProc2, Props, Opts, Context).

replace_file_db(RscId, PreProc, Props, Opts, Context) ->
    SafeRootName = z_string:to_rootname(PreProc#media_upload_preprocess.original_filename),
    PreferExtension = z_convert:to_binary(
        filename:extension(PreProc#media_upload_preprocess.original_filename)),
    Mime = z_convert:to_binary(PreProc#media_upload_preprocess.mime),
    SafeFilename = iolist_to_binary(
        [SafeRootName, z_media_identify:extension(Mime, PreferExtension, Context)]),
    ArchiveFile = case PreProc#media_upload_preprocess.file of
                      undefined -> undefined;
                    UploadFile -> z_media_archive:archive_copy_opt(UploadFile, SafeFilename, Context)
                  end,
    RootName = case ArchiveFile of
                    undefined -> undefined;
                    _ -> filename:rootname(filename:basename(ArchiveFile))
               end,
    Medium0 = [
        {mime, Mime},
        {filename, ArchiveFile},
        {rootname, RootName},
        {is_deletable_file, is_deletable_file(PreProc#media_upload_preprocess.file, Context)}
    ],
    Medium = z_utils:props_merge(PreProc#media_upload_preprocess.medium, Medium0),
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

    IsImport = proplists:is_defined(is_import, Opts),
    NoTouch = proplists:is_defined(no_touch, Opts),

    F = fun(Ctx) ->
        %% If the resource is in the media category, then move it to the correct sub-category depending
        %% on the mime type of the uploaded file.
        Props1 = case proplists:is_defined(category, PropsM)
            orelse proplists:is_defined(category_id, PropsM)
        of
            true -> PropsM;
            false -> [{category, mime_to_category(Mime)} | PropsM]
        end,
        {ok, Id} = case RscId of
            insert_rsc ->
                m_rsc_update:insert(Props1, Opts, Ctx);
            _ ->
                case rsc_is_media_cat(RscId, Context) of
                    true ->
                        {ok, RscId} = m_rsc_update:update(RscId, Props1, Opts, Ctx);
                    false ->
                        case IsImport orelse NoTouch of
                            true -> nop;
                            false -> {ok, RscId} = m_rsc:touch(RscId, Ctx)
                        end
                end,
                medium_delete(RscId, Ctx),
                {ok, RscId}
        end,
        case medium_insert(Id, [{id, Id} | Medium1], Ctx) of
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
            [z_depcache:flush(Cat, Context) || Cat <- CatList],

            m_rsc:get(Id, Context), %% Prevent side effect that empty things are cached?

            % Run possible post insertion function.
            case PreProc#media_upload_preprocess.post_insert_fun of
                undefined -> ok;
                PostFun when is_function(PostFun, 3) ->
                    PostFun(Id, Medium1, Context)
            end,

            %% Pass the medium record along in the notification; this also fills the depcache (side effect).
            z_notifier:notify(#media_replace_file{id = Id, medium = get(Id, Context)}, Context),
            {ok, Id};
        {rollback, {{error, not_allowed}, _StackTrace}} ->
            {error, not_allowed}
    end.

is_deletable_file(undefined, _Context) ->
    false;
is_deletable_file(File, Context) ->
    not z_media_archive:is_archived(File, Context).

replace_url(Url, RscId, Props, Context) ->
    replace_url(Url, RscId, Props, [], Context).

replace_url(Url, RscId, Props, Options, Context) ->
    case z_acl:rsc_editable(RscId, Context) orelse not(m_rsc:p(RscId, is_authoritative, Context)) of
        true ->
            case download_file(Url) of
                {ok, File, Filename} ->
                    Result = replace_file(File, RscId, [{original_filename, Filename} | Props], Options,
                        Context),
                    file:delete(File),
                    Result;
                {error, E} ->
                    {error, E}
            end;
        false ->
            {error, eacces}
    end.


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


%% @doc Download a file from a http or data url.
download_file(Url) ->
    download_file(Url, []).

download_file(Url, Options) ->
    File = z_tempfile:new(),
    {ok, Device} = file:open(File, [write]),
    MaxLength = proplists:get_value(max_length, Options, ?MEDIA_MAX_LENGTH_DOWNLOAD),
    Timeout = proplists:get_value(timeout, Options, ?MEDIA_TIMEOUT_DOWNLOAD),
    FetchOptions = [
        {max_length, MaxLength},
        {timeout, Timeout},
        {device, Device}
    ],
    case z_url_fetch:fetch_partial(Url, FetchOptions) of
        {ok, {_FinalUrl, Hs, Length, _Data}} when Length < MaxLength ->
            file:close(Device),
            {ok, File, filename(Url, Hs)};
        {ok, {_FinalUrl, _Hs, Length, _Data}} when Length >= MaxLength ->
            file:close(Device),
            file:delete(File),
            {error, file_too_large};
        {ok, _Other} ->
            file:close(Device),
            file:delete(File),
            {error, download_failed};
        {error, _} = Error ->
            file:close(Device),
            file:delete(File),
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
add_medium_info(File, OriginalFilename, Props, Context) ->
    PropsSize = case proplists:get_value(size, Props) of
        undefined ->
            [{size, filelib:file_size(File)} | Props];
        _ ->
            Props
    end,
    PropsMime = case proplists:get_value(mime, PropsSize) of
        undefined ->
            case z_media_identify:identify_file(File, OriginalFilename, Context) of
                {ok, MediaInfo} -> MediaInfo ++ PropsSize;
                {error, _Reason} -> PropsSize
            end;
        _ ->
            PropsSize
    end,
    PropsMime.


%% @doc Save a new file from a preview_url as the preview of a medium
save_preview_url(RscId, Url, Context) ->
    case download_file(Url, [{max_length, ?MEDIA_MAX_LENGTH_PREVIEW}]) of
        {ok, TmpFile, Filename} ->
            case z_media_identify:identify_file(TmpFile, Filename, Context) of
                {ok, MediaInfo} ->
                    try
                        {mime, Mime} = proplists:lookup(mime, MediaInfo),
                        {width, Width} = proplists:lookup(width, MediaInfo),
                        {height, Height} = proplists:lookup(height, MediaInfo),

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
                        UpdateProps = [
                            {preview_filename, FileUnique},
                            {preview_width, Width},
                            {preview_height, Height},
                            {is_deletable_preview, true}
                        ],
                        {ok, 1} = z_db:update(medium, RscId, UpdateProps, Context),
                        z_depcache:flush({medium, RscId}, Context),
                        {ok, FileUnique}
                    catch
                        _:Error ->
                            lager:warning("Error importing preview for ~p, url ~p, mediainfo ~p",
                                [RscId, Url, MediaInfo]),
                            file:delete(TmpFile),
                            {error, Error}
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Save a preview for a medium record. The data is saved to a file in the archive directory.
save_preview(RscId, Data, Mime, Context) ->
    case z_acl:rsc_editable(RscId, Context) of
        true ->
            FileUnique = make_preview_unique(RscId, z_media_identify:extension(Mime), Context),
            FileUniqueAbs = z_media_archive:abspath(FileUnique, Context),
            ok = z_filelib:ensure_dir(FileUniqueAbs),
            ok = file:write_file(FileUniqueAbs, Data),

            try
                {ok, MediaInfo} = z_media_identify:identify(FileUniqueAbs, Context),
                {width, Width} = proplists:lookup(width, MediaInfo),
                {height, Height} = proplists:lookup(height, MediaInfo),

                UpdateProps = [
                    {preview_filename, FileUnique},
                    {preview_width, Width},
                    {preview_height, Height},
                    {is_deletable_preview, true}
                ],
                {ok, 1} = z_db:update(medium, RscId, UpdateProps, Context),
                z_depcache:flush({medium, RscId}, Context),
                {ok, FileUnique}
            catch
                Error ->
                    file:delete(FileUniqueAbs),
                    Error
            end;
        false ->
            {error, eacces}
    end.

-spec make_preview_unique(integer()|insert_rsc, string(), #context{}) -> file:filename().
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
    z_notifier:notify(
        #media_update_done{
            action = insert,
            id = Id,
            post_is_a = IsA,
            post_props = Props1
        },
        Context),
    z_db:insert(medium, Props1, Context).

medium_delete(Id, Context) ->
    medium_delete(Id, get(Id, Context), Context).

medium_delete(_Id, undefined, _Context) ->
    {ok, 0};
medium_delete(Id, Props, Context) ->
    IsA = m_rsc:is_a(Id, Context),
    z_notifier:notify(
        #media_update_done{
            action = delete,
            id = Id,
            pre_is_a = IsA,
            pre_props = Props
        },
        Context),
    z_db:delete(medium, Id, Context).

check_medium_props(Ps) ->
    [check_medium_prop(P) || P <- Ps].

check_medium_prop({width, N}) when not is_integer(N) -> {width, 0};
check_medium_prop({height, N}) when not is_integer(N) -> {height, 0};
check_medium_prop(P) -> P.
