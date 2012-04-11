%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2011 Arjan Scherpenisse
%% @doc Email image embedding

%% Copyright 2010-2011 Arjan Scherpenisse <arjan@scherpenisse.net>
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

-module(z_email_embed).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

%% interface functions
-export([embed_images/2]).

-include_lib("zotonic.hrl").

%% @doc Embed images mentioned in the HTML parts.
embed_images(Parts, Context) ->
    [ embed_images_part(P, Context) || P <- Parts ].


embed_images_part({<<"text">>, <<"html">>, Hs, Ps, Html} = HtmlPart, Context) ->
    case embed_images_html(Html, Context) of
        {[], _Html} ->
            HtmlPart;
        {ImageParts, Html1} ->
            Html1,
            {   <<"multipart">>, <<"related">>,
                [], 
                [], 
                [ 
                    {<<"text">>, <<"html">>, Hs, Ps, z_convert:to_binary(Html1)} | ImageParts
                ]
            }
    end;
embed_images_part({<<"multipart">>, SubType, Hs, Ps, Parts}, Context) ->
    Parts1 = [ embed_images_part(P, Context) || P <- Parts ],
    {<<"multipart">>, SubType, Hs, Ps, Parts1};
embed_images_part(Part, _Context) ->
    Part.



%% Given a HTML message, extract images (starting with /lib/ or /image/) and
embed_images_html(Html, Context) ->
    {P1, Html1} = embed_lib_images(Html, Context),
    {P2, Html2} = embed_generated_images(Html1, Context),
    {P1 ++ P2, Html2}.


embed_lib_images(Html, Context) ->
    Re = "src=[\"']/lib/(.*?)[\"']",
    case re:run(Html, Re, [{capture, all_but_first, list}, global, caseless]) of
        nomatch -> 
            {[], Html};
        {match, Matches} ->
            UniqueMatches = sets:to_list(sets:from_list(Matches)),
            {P, H, _C} = lists:foldl(fun embed_lib_image_match/2, {[], Html, Context}, UniqueMatches),
            {P, H}
    end.

embed_lib_image_match([Match], {Parts, Html, Context}) ->
    case z_module_indexer:find(lib, Match, Context) of
        {ok, #module_index{filepath=File}} ->
            case filelib:is_file(File) of
                true ->
                    ContentType = z_media_identify:guess_mime(File),
                    {Part, Cid} = create_attachment(File, ContentType, undefined),
                    Html1 = re:replace(Html, "/lib/" ++ Match, "cid:" ++ Cid, [global, {return, list}]),
                    {[Part|Parts], Html1, Context};
                _ ->
                    {Parts, Html, Context}
            end;
        {error, _} ->
            {Parts, Html, Context}
    end.


embed_generated_images(Html, Context) ->
    Re = "src=[\"']/image/(.*?)[\"']",
    case re:run(Html, Re, [{capture, all_but_first, list}, global, caseless]) of
        nomatch -> 
            {[], Html};
        {match, Matches} ->
            UniqueMatches = sets:to_list(sets:from_list(Matches)),
            {P, H, _C} = lists:foldl(fun embed_generated_image_match/2, {[], Html, Context}, UniqueMatches),
            {P, H}
    end.


embed_generated_image_match([Match], {Parts, Html, Context}) ->
    case ensure_image_file(Match, Context) of
        {true, BaseFile, Context1} ->
            File = z_context:get(fullpath, Context1),
            ContentType = z_context:get(mime, Context1),
            BaseName = filename:rootname(filename:basename(BaseFile)) ++ z_media_identify:extension(ContentType),
            {Part, Cid} = create_attachment(File, ContentType, BaseName),
            Html1 = re:replace(Html, "/image/" ++ Match, "cid:" ++ Cid, [global, {return, list}]),
            {[Part|Parts], Html1, Context};
        {false, _} ->
            {Parts, Html, Context};
        {error, _} ->
            {Parts, Html, Context}
    end.

ensure_image_file(RawPath, Context) ->
    FilePath = mochiweb_util:safe_relative_path(mochiweb_util:unquote(RawPath)),
    {Path, BaseFile, ContextPath} = rsc_media_check(FilePath, Context),
    ContextMime = case z_context:get(mime, ContextPath) of
                      undefined -> z_context:set(mime, z_media_identify:guess_mime(Path), ContextPath);
                      _Mime -> ContextPath
                  end,
    case file_exists(Path, ContextMime) of 
        {true, FullPath} ->
            {true, BaseFile, z_context:set([ {path, Path}, {fullpath, FullPath} ], ContextMime)};
        _ -> 
            %% We might be able to generate a new preview
            case z_context:get(is_media_preview, ContextMime, false) of
                true ->
                    % Generate a preview, recurse on success
                    ensure_preview(Path, ContextMime);
                false ->
                    {false, ContextMime}
            end
    end.



rsc_media_check(undefined, Context) ->
    {undefined, undefined, Context};
rsc_media_check(File, Context) ->
    {BaseFile, IsResized, Context1} = case lists:member($(, File) of
                            true ->
                                {File1, Proplists, Check, Prop} = z_media_tag:url2props(File, Context),
                                {File1, true, z_context:set(media_tag_url2props, {File1, Proplists, Check, Prop}, Context)};
                            false ->
                                {File, false, Context}
                          end,
    case m_media:get_by_filename(BaseFile, Context1) of
        undefined ->
            {File, BaseFile, Context1};
        Media ->
            MimeOriginal = z_convert:to_list(proplists:get_value(mime, Media)),
            Props = [
                {id, proplists:get_value(id, Media)},
                {mime_original, MimeOriginal},
                {is_media_preview, IsResized}
            ],
            Props1 = case IsResized of 
                        true -> [ {mime, z_media_identify:guess_mime(File)} | Props ];
                        false -> [ {mime, MimeOriginal} | Props ]
                     end,
            {File, BaseFile, z_context:set(Props1, Context1)}
    end.


file_exists(undefined, _Context) ->
    false;
file_exists([], _Context) ->
    false;
file_exists(Name, Context) ->
    RelName = case hd(Name) of
                  $/ -> tl(Name);
                  _ -> Name
              end,
    case mochiweb_util:safe_relative_path(RelName) of
        undefined ->
            false;
        SafePath ->
            RelName = case hd(SafePath) of
                          "/" -> tl(SafePath);
                          _ -> SafePath
                      end,
            NamePath = filename:join([z_path:media_preview(Context),RelName]),
            case filelib:is_regular(NamePath) of 
                true -> {true, NamePath};
                false -> false
            end
    end.



%% @spec ensure_preview(Path, Context) -> {Boolean, NewContext}
%% @doc Generate the file on the path from an archived media file.
%% The path is like: 2007/03/31/wedding.jpg(300x300)(crop-center)(709a-a3ab6605e5c8ce801ac77eb76289ac12).jpg
%% The original media should be in State#media_path (or z_path:media_archive)
%% The generated image should be created in State#root (or z_path:media_preview)
ensure_preview(Path, Context) ->
    {Filepath, PreviewPropList, _Checksum, _ChecksumBaseString} = 
                    case z_context:get(media_tag_url2props,Context) of
                        undefined -> z_media_tag:url2props(Path, Context);
                        MediaInfo -> MediaInfo
                    end,
    case mochiweb_util:safe_relative_path(Filepath) of
        undefined ->
            {false, Context};
        Safepath  ->
            MediaPath = case z_context:get(media_path, Context) of
                            undefined -> z_path:media_archive(Context);
                            ConfMediaPath -> ConfMediaPath
                        end,

            MediaFile = case Safepath of 
                            "lib/" ++ LibPath ->  
                                case z_module_indexer:find(lib, LibPath, Context) of 
                                    {ok, #module_index{filepath=ModuleFilename}} -> ModuleFilename; 
                                    {error, _} -> filename:join(MediaPath, Safepath) 
                                end; 
                            _ -> 
                                filename:join(MediaPath, Safepath) 
                        end,
            case filelib:is_regular(MediaFile) of
                true ->
                    % Media file exists, perform the resize
                    PreviewFile = filename:join(z_path:media_preview(Context), Path),
                    case z_media_preview:convert(MediaFile, PreviewFile, PreviewPropList, Context) of
                        ok -> {true, MediaFile, z_context:set(fullpath, PreviewFile, Context)};
                        {error, _Reason} = Error -> Error
                    end;
                false ->
                    {false, Context}
            end
    end.



create_attachment(Filename, ContentType, Name) ->
    ContentId = z_ids:id(30),
    Headers = [ 
        {<<"Content-ID">>, iolist_to_binary(["<" ++ ContentId ++ ">"])},
        {<<"X-Attachment-Id">>, z_convert:to_binary(ContentId)} 
    ],
    {create_attachment_part(Filename, ContentType, Name, Headers), ContentId}.

create_attachment_part(Filename, ContentType, Name, Headers) ->
    [Type, Subtype] = string:tokens(ContentType, "/"),
    {ok, Data} = file:read_file(Filename),
    { z_convert:to_binary(Type), z_convert:to_binary(Subtype),
      Headers,
      [
        {<<"transfer-encoding">>, <<"base64">>},
        {<<"disposition">>, <<"inline">>}, 
        {<<"disposition-params">>, [{<<"filename">>, z_convert:to_binary(Name)}]}
      ],
      Data
    }.



