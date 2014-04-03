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

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
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
    insert_file/2,
    insert_file/3,
    insert_file/4,
    replace_file/3,
    replace_file/4,
    replace_file/5,
    replace_file/6,
    insert_url/2,
    insert_url/3,
    insert_url/4,
    replace_url/4,
    replace_url/5,
	save_preview/4,
    make_preview_unique/2,
    is_unique_file/2
]).

-include_lib("zotonic.hrl").



%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Id, #m{value=undefined}, Context) ->
    get(Id, Context);
m_find_value(_Key, #m{}, _Context) ->
    undefined.

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{}, _Context) ->
    [].
    
%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{}, _Context) ->
    undefined.


%% @doc Return the identification of a medium. Used by z_media_identify:identify()
%% @spec identify(ImageFilePath, Context) -> {ok, PropList} | {error, Reason}
identify(Id, Context) when is_integer(Id) ->
    case z_db:assoc_row("select id, mime, width, height, orientation from medium where id = $1", [Id], Context) of
        undefined ->
            {error, enoent};
        Props ->
            {ok, Props}
    end;
identify(ImageFile, Context) ->
    case z_media_archive:is_archived(ImageFile, Context) of
        true ->
            RelFile = z_media_archive:rel_archive(ImageFile, Context),
            case z_db:assoc_row("select id, mime, width, height, orientation from medium where filename = $1", [RelFile], Context) of
                undefined ->
                    {error, enoent};
                Props ->
                    {ok, Props}
            end;
        false ->
            {error, enoent}
    end.


%% @doc Check if a medium record exists
exists(undefined, _Context) ->
    false;
exists([C|_] = Name, Context) when is_integer(C) ->
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
    F = fun() -> z_db:assoc_props_row("select * from medium where id = $1", [Id], Context) end,
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
                        filename=Filename,
                        data=Data,
                        mime=proplists:get_value(mime, Media)
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
depiction(Id, Context) when is_integer(Id) ->
    F = fun() ->
        find_previewable(m_edge:objects(Id, depiction, Context) ++ [Id], Context)
    end,
    z_depcache:memo(F, {depiction, Id}, ?WEEK, [Id], Context).

    %% @doc Find the first image in the the list of depictions that can be used to generate a preview.
    find_previewable([], _Context) ->
        undefined;
    find_previewable([Id|Rest], Context) ->
        case get(Id, Context) of
            undefined ->
                find_previewable(Rest, Context);
            Props ->
                case z_media_preview:can_generate_preview(proplists:get_value(mime, Props)) of
                    true -> Props;
                    false -> find_previewable(Rest, Context)
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
            [ z_depcache:flush(DepictId, Context) || DepictId <- Depicts ],
            z_depcache:flush(Id, Context),
            ok;
        false ->
            {error, eacces}
    end.


%% @doc Replace or insert a medium record for the page.  This is useful for non-file related media.
%% Resets all non mentioned attributes.
%% @spec replace(Id, Props, Context) -> ok | {error, Reason}
replace(Id, Props, Context) ->
    Depicts = depicts(Id, Context),
    F = fun(Ctx) ->
        {ok, _}  = medium_delete(Id, Ctx),
        {ok, Id} = medium_insert(Id, [{id, Id} | Props], Ctx)
    end,
    
    case z_db:transaction(F, Context) of
        {ok, _} -> 
            [ z_depcache:flush(DepictId, Context) || DepictId <- Depicts ],
            z_depcache:flush(Id, Context),
            ok;
        {rollback, {Error, _Trace}} ->
             {error, Error}
    end.


%% @doc Duplicate the media item from the id to the new-id. Called by m_rsc:duplicate/3 
duplicate(FromId, ToId, Context) ->
    case z_db:assoc_props_row("select * from medium where id = $1", [FromId], Context) of
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
                    {ok, NewFile} = duplicate_file(Filename, Context),
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
                    {ok, NewFile} = duplicate_file(Filename, Context),
                    Ms1 = proplists:delete(preview_filename, 
                            proplists:delete(is_deletable_preview, Ms)), 
                    Ms2 = [
                        {preview_filename, NewFile}, 
                        {is_deletable_preview, true}
                        | Ms1
                    ],
                    {ok, Ms2}
            end
    end.


duplicate_file(Filename, Context) ->
    case z_file_request:lookup_file(Filename, Context) of
        {ok, FileInfo} ->
            {ok, File} = z_file_request:content_file(FileInfo, Context),
            ArchiveFile = z_media_archive:archive_copy(File, filename:basename(Filename), Context),
            {ok, ArchiveFile};
        {error, _} = Error ->
            Error
    end.


%% @doc Make a new resource for the file, when the file is not in the archive dir then a copy is made in the archive dir
%% @spec insert_file(File, Context) -> {ok, Id} | {error, Reason}
insert_file(File, Context) ->
    insert_file(File, [], Context).
insert_file(#upload{filename=OriginalFilename, data=Data, tmpfile=undefined}, Props, Context) when Data /= undefined ->
    TmpFile = z_tempfile:new(),
    ok = file:write_file(TmpFile, Data),
    insert_file(#upload{filename=OriginalFilename, tmpfile=TmpFile}, Props, [], Context);
insert_file(File, Props, Context) ->
    insert_file(File, Props, [], Context).


insert_file(#upload{filename=OriginalFilename, tmpfile=TmpFile}, Props, Options, Context) ->
    PropsMedia = add_medium_info(TmpFile, OriginalFilename, [{original_filename, OriginalFilename}], Context),
    insert_file(TmpFile, [{original_filename, OriginalFilename}|Props], PropsMedia, Options, Context);
insert_file(File, Props, Options, Context) ->
    OriginalFilename = proplists:get_value(original_filename, Props, File),
    PropsMedia = add_medium_info(File, OriginalFilename, [{original_filename, OriginalFilename}], Context),
    insert_file(File, Props, PropsMedia, Options, Context).

insert_file(File, Props, PropsMedia, Options, Context) ->
    Mime = proplists:get_value(mime, PropsMedia),
    case z_acl:is_allowed(insert, #acl_rsc{category=mime_to_category(Mime)}, Context) andalso
         z_acl:is_allowed(insert, #acl_media{mime=Mime, size=filelib:file_size(File)}, Context) of
        true ->
            insert_file_mime_ok(File, Props, PropsMedia, Options, Context);
        false ->
            {error, file_not_allowed}
    end.


%% @doc Make a new resource for the file based on a URL.
%% @spec insert_url(File, Context) -> {ok, Id} | {error, Reason}
insert_url(Url, Context) ->
    insert_url(Url, [], Context).

insert_url(Url, Props, Context) ->
    insert_url(Url, Props, [], Context).

insert_url(Url, Props, Options, Context) ->
    case download_file(Url) of
        {ok, TmpFile, Filename} ->
            Result = insert_file(TmpFile, [{original_filename, Filename}|Props], Options, Context),
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
    Props3 = case proplists:get_value(title, Props2) of
        undefined -> [{title, proplists:get_value(original_filename, Props2)}|Props2];
        _ -> Props2
    end,
    replace_file_mime_ok(File, insert_rsc, Props3, PropsMedia, Options, Context).


%% @doc Replaces a medium file, when the file is not in archive then a copy is made in the archive.
%% When the resource is in the media category, then the category is adapted depending on the mime type of the uploaded file.
%% @spec replace_file(File, RscId, Context) -> {ok, Id} | {error, Reason}
replace_file(File, RscId, Context) ->
    replace_file(File, RscId, [], [], Context).

replace_file(File, RscId, Props, Context) ->
    replace_file(File, RscId, Props, [], Context).

replace_file(File, RscId, Props, Opts, Context) ->
    replace_file(File, RscId, Props, [], Opts, Context).

replace_file(#upload{filename=OriginalFilename, data=Data, tmpfile=undefined}, RscId, Props, MInfo, Opts, Context) when Data /= undefined ->
    TmpFile = z_tempfile:new(),
    ok = file:write_file(TmpFile, Data),
    replace_file(#upload{filename=OriginalFilename, tmpfile=TmpFile}, RscId, Props, MInfo, Opts, Context);
replace_file(#upload{filename=OriginalFilename, tmpfile=TmpFile}, RscId, Props, MInfo, Opts, Context) ->
    PropsMedia = add_medium_info(TmpFile, OriginalFilename, [{original_filename, OriginalFilename}|MInfo], Context),
    replace_file_mime_check(TmpFile, RscId, [{original_filename, OriginalFilename}|Props], PropsMedia, Opts, Context);
replace_file(File, RscId, Props, MInfo, Opts, Context) ->
    OriginalFilename = proplists:get_value(original_filename, Props, File),
    PropsMedia = add_medium_info(File, OriginalFilename, [{original_filename, OriginalFilename}|MInfo], Context),
    replace_file_mime_check(File, RscId, Props, PropsMedia, Opts, Context).

replace_file_mime_check(File, RscId, Props, PropsMedia, Opts, Context) ->
    Mime = proplists:get_value(mime, PropsMedia),
    case z_acl:is_allowed(insert, #acl_rsc{category=mime_to_category(Mime)}, Context) andalso
         z_acl:is_allowed(insert, #acl_media{mime=Mime, size=filelib:file_size(File)}, Context) of
        true ->
            replace_file_mime_ok(File, RscId, Props, PropsMedia, Opts, Context);
        false ->
            {error, file_not_allowed}
    end.

%% @doc Replace the file, no mime check needed.
replace_file_mime_ok(File, RscId, Props, PropsMedia, Opts, Context) ->
    case RscId == insert_rsc orelse z_acl:rsc_editable(RscId, Context) orelse not(m_rsc:p(RscId, is_authoritative, Context)) of
        true ->
            replace_file_acl_ok(File, RscId, Props, PropsMedia, Opts, Context);
        false ->
            {error, eacces}
    end.

replace_file_acl_ok(File, RscId, Props, Medium, Opts, Context) ->
    Mime = proplists:get_value(mime, Medium),
    PreProc = #media_upload_preprocess{
                    id=RscId, 
                    mime=Mime,
                    file=File,
                    original_filename=proplists:get_value(original_filename, Props, File),
                    medium=Medium
                },
    NewPreProc = case z_notifier:first(PreProc, Context) of
                    undefined -> PreProc;
                    #media_upload_preprocess{} = Mapped -> Mapped
                 end,
    replace_file_db(RscId, NewPreProc, Props, Opts, Context).

replace_file_db(RscId, PreProc, Props, Opts, Context) ->
    SafeRootName = z_string:to_rootname(PreProc#media_upload_preprocess.original_filename),
    PreferExtension = z_convert:to_binary(filename:extension(PreProc#media_upload_preprocess.original_filename)),
    Mime = PreProc#media_upload_preprocess.mime,
    SafeFilename = SafeRootName ++ z_media_identify:extension(Mime, PreferExtension),
    ArchiveFile = case PreProc#media_upload_preprocess.file of
                    undefined -> undefined;
                    UploadFile -> z_media_archive:archive_copy_opt(UploadFile, SafeFilename, Context)
                  end,
    RootName = case ArchiveFile of
                    undefined -> undefined;
                    _ -> filename:rootname(filename:basename(ArchiveFile))
               end,
    Medium0 = [
        {mime, PreProc#media_upload_preprocess.mime},
        {filename, ArchiveFile}, 
        {rootname, RootName}, 
        {is_deletable_file, is_deletable_file(PreProc#media_upload_preprocess.file, Context)}
    ],
    Medium = z_utils:props_merge(PreProc#media_upload_preprocess.medium, Medium0),
    Medium1 = z_notifier:foldl(
                            #media_upload_props{
                                    id=RscId,
                                    mime=Mime, 
                                    archive_file=ArchiveFile,
                                    options=Opts
                            },
                            Medium,
                            Context),

    IsImport = proplists:is_defined(is_import, Opts),
    NoTouch = proplists:is_defined(no_touch, Opts),
    
    F = fun(Ctx) ->
                %% If the resource is in the media category, then move it to the correct sub-category depending
                %% on the mime type of the uploaded file.
                Props1 = case      proplists:is_defined(category, Props)
                            orelse proplists:is_defined(category_id, Props)
                         of 
                            true -> Props;
                            false -> [{category, mime_to_category(Mime)} | Props]
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
    
    {ok, Id} = z_db:transaction(F, Context),
    Depicts = depicts(Id, Context),
    [ z_depcache:flush(DepictId, Context) || DepictId <- Depicts ],
    z_depcache:flush(Id, Context),

    %% Flush categories
    CatList = m_rsc:is_a(Id, Context),
    [ z_depcache:flush(Cat, Context) || Cat <- CatList ],
    
    m_rsc:get(Id, Context), %% Prevent side effect that empty things are cached?

    % Run possible post insertion function.
    case PreProc#media_upload_preprocess.post_insert_fun of
        undefined -> ok;
        PostFun when is_function(PostFun, 3) -> PostFun(Id, Medium1, Context)
    end, 

    %% Pass the medium record along in the notification; this also fills the depcache (side effect).
    z_notifier:notify(#media_replace_file{id=Id, medium=get(Id, Context)}, Context),
    {ok, Id}.

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
                    Result = replace_file(File, RscId, [{original_filename, Filename}|Props], Options, Context),
                    file:delete(File),
                    Result;
                {error, E} ->
                    {error, E}
            end;
        false ->
            {error, eacces}
    end.


rsc_is_media_cat(Id, Context) ->
    case z_db:q1("select c.name from rsc c join rsc r on r.category_id = c.id where r.id = $1", [Id], Context) of
        <<"media">> -> true;
        <<"image">> -> true;
        <<"audio">> -> true;
        <<"video">> -> true;
        <<"document">> -> true;
        _ -> false
    end.

mime_to_category(Mime) ->
    case Mime of
        "image/" ++ _ -> image;
        "video/" ++ _ -> video;
        "audio/" ++ _ -> audio;
        "application/" ++ _ -> document;
        _ -> media
    end.


%% @doc Download a file from a http or data url.
download_file("data:" ++ _ = DataUrl) ->
    download_file(z_convert:to_binary(DataUrl));
download_file(<<"data:", _/binary>> = DataUrl) ->
    case z_url:decode_data_url(DataUrl) of
        {ok, Mime, _Charset, Bytes} ->
            Filename = z_tempfile:new(),
            ok = file:write_file(Filename, Bytes),
            {ok, Filename, mime2filename(Mime)};
        {error, _} = Error ->
            Error
    end;
download_file(Url) ->
    File = z_tempfile:new(),
    case httpc:request(get, 
                      {z_convert:to_list(Url), []},
                      [],
                      [{stream, File}]) of
        {ok, saved_to_file} ->
            {ok, File, filename:basename(Url)};
        {ok, _Other} ->
            {error, download_failed};
        {error, E} ->
            file:delete(File),
            {error, E}
    end.


mime2filename(<<"image/jpeg">>) ->
    <<"image.jpg">>;
mime2filename(<<"application/pdf">>) ->
    <<"document.pdf">>;
mime2filename(<<"text/", _/binary>>) ->
    <<"document.txt">>;
mime2filename(<<"application/", _/binary>> = Mime) ->
    case mimetypes:mime_to_exts(Mime) of
        undefined -> "file";
        [Ext|_] -> iolist_to_binary([<<"document.">>, Ext]) 
    end;
mime2filename(Mime) ->
    [Base|_] = binary:split(Mime, <<"/">>),
    case mimetypes:mime_to_exts(Mime) of
        undefined -> Base;
        [Ext|_] -> iolist_to_binary([Base, $., Ext]) 
    end.


%% @doc Fetch the medium information of the file, if they are not set in the Props
add_medium_info(File, OriginalFilename, Props, Context) ->
    PropsSize = case proplists:get_value(size, Props) of
        undefined ->
            [{size, filelib:file_size(File)}|Props];
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


%% @doc Save a preview for a medium record. The data is saved to a file in the archive directory.
save_preview(RscId, Data, Mime, Context) ->
	case z_acl:rsc_editable(RscId, Context) of
    	true ->
			Filename = data2filepath(RscId, Data, z_media_identify:extension(Mime)),
			OldPreviewFilename = z_db:q1("select preview_filename from medium where id = $1", [RscId], Context),
			case z_convert:to_list(OldPreviewFilename) of
				Filename -> 
					ok;
				_OldFile ->
					FileUnique = make_preview_unique(Filename, Context),
					FileUniqueAbs = z_media_archive:abspath(FileUnique, Context),
					ok = filelib:ensure_dir(FileUniqueAbs),
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
						{ok,1} = z_db:update(medium, RscId, UpdateProps, Context),
						z_depcache:flush({medium, RscId}, Context),
						{ok, FileUnique}
					catch 
						Error -> 
							file:delete(FileUniqueAbs), 
							Error
					end
			end;
		false ->
			{error, eacces}
	end.

data2filepath(RscId, Data, Extension) ->
	<<A:8, B:8, Rest/binary>> = crypto:sha(Data),
	filename:join([ "preview", mochihex:to_hex(A), mochihex:to_hex(B), 
					integer_to_list(RscId) ++ [$-|mochihex:to_hex(Rest)] ++ Extension ]).

make_preview_unique(Filename, Context) ->
    case is_unique_file(Filename, Context) of
        true ->
            Filename;
        false ->
			Dirname = filename:dirname(Filename),
			Basename = filename:basename(Filename),
			Rootname = filename:rootname(Basename),
			Rootname1 = Rootname ++ [$-|integer_to_list(z_ids:number())],
			Filename1 = filename:join([Dirname, Rootname1 ++ filename:extension(Basename)]),
			make_preview_unique(Filename1, Context)
	end.

is_unique_file(Filename, Context) ->
    z_db:q1("select count(*) from medium_log where filename = $1", [Filename], Context) =:= 0.

medium_insert(Id, Props, Context) ->
    IsA = m_rsc:is_a(Id, Context),
    z_notifier:notify(#media_update_done{action=insert, id=Id, post_is_a=IsA, pre_is_a=[], pre_props=[], post_props=Props}, Context),
    z_db:insert(medium, Props, Context).
 
medium_delete(Id, Context) ->
    medium_delete(Id, get(Id, Context), Context).

medium_delete(_Id, undefined, _Context) ->
    {ok, 0};
medium_delete(Id, Props, Context) ->
    IsA = m_rsc:is_a(Id, Context),
    z_notifier:notify(#media_update_done{action=delete, id=Id, pre_is_a=IsA, post_is_a=[], pre_props=Props, post_props=[]}, Context),
    z_db:delete(medium, Id, Context).
