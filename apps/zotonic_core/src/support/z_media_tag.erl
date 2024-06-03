%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell, David de Boer
%% @doc Generate media urls and html for viewing media, based on the filename, size and optional filters.
%% Does not generate media previews itself, this is done when fetching the image.
%%
%% Typical urls are like:
%% /image/2007/03/31/wedding.jpg(300x300)(crop-center)(a3ab6605e5c8ce801ac77eb76289ac12).jpg
%% /media/inline/2007/03/31/wedding.jpg
%% /media/attachment/2007/03/31/wedding.jpg
%% @end

%% Copyright 2009-2023 Marc Worrell, David de Boer
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

-module(z_media_tag).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    scomp_viewer/3,
    scomp_tag/3,
    scomp_url/3,
    scomp_data_url/3,

    viewer/3,
    tag/3,
    attributes/3,
    url/3,
    url2props/2,

    % Export for tests
    props2url/2
]).

-include_lib("zotonic.hrl").

-type mediaref() :: undefined
                  | m_rsc:resource_id()
                  | z_media_identify:media_info()
                  | #rsc_list{}
                  | proplists:proplist()
                  | file:filename_all().

-type fileref() :: file:filename_all()
                 | {filepath, file:filename_all(), file:filename_all()}.

-type viewer_options() :: proplists:proplist().

-export_type([
    mediaref/0,
    fileref/0,
    viewer_options/0
    ]).

% Max number of pixels we allow a (animated) GIF image resized output.
% Larger will be served without resizing.
-define(MAX_GIF_PIXELS, 500).
-define(MAX_GIF_FRAMES, 50).


%% @doc Called from template, render the media viewer for some resource/medium
scomp_viewer(undefined, _Options, _Context) ->
    <<>>;
scomp_viewer(IdOrName, Options, Context) ->
    {ok, Rendered} = viewer(IdOrName, Options, Context),
    Rendered.

%% @doc Called from template, render the media tag for some resource/medium
scomp_tag(undefined, _Options, _Context) ->
    <<>>;
scomp_tag(IdOrName, Options, Context) ->
    {ok, Rendered} = tag(IdOrName, Options, Context),
    Rendered.

%% @doc Called from template, render the media url for some resource/medium
scomp_url(undefined, _Options, _Context) ->
    <<>>;
scomp_url(IdOrName, Options, Context) ->
    case url(IdOrName, Options, Context) of
        {ok, Rendered} ->
            Rendered;
        {error, enoent} ->
            <<>>
    end.

%% @doc Generate a 'data:' url for the given image.
scomp_data_url(undefined, _Options, _Context) ->
    <<>>;
scomp_data_url(IdOrName, Options, Context) ->
    % Generate the Url
    case scomp_url(IdOrName, [ is_data_url | Options ], Context) of
        <<>> ->
            <<>>;
        Url ->
            % Fetch data from the url
            case z_media_data:file_data_url(Url, Context) of
                {ok, DataUrl} -> DataUrl;
                {error, _} -> <<>>
            end
    end.


%% @doc Generate a html fragment for displaying a medium.  This can generate audio or video player html.
-spec viewer(MediaReference, Options, Context) -> {ok, HTML} when
    MediaReference :: mediaref(),
    Options :: viewer_options(),
    Context :: z:context(),
    HTML :: iodata().
viewer(undefined, _Options, _Context) ->
    {ok, <<>>};
viewer([], _Options, _Context) ->
    {ok, <<>>};
viewer(#rsc_list{list=[]}, _Options, _Context) ->
    {ok, <<>>};
viewer(#rsc_list{list=[Id|_]}, Options, Context) ->
    viewer(Id, Options, Context);
viewer(Name, Options, Context) when is_atom(Name) ->
    case m_rsc:name_to_id(Name, Context) of
        {ok, Id} -> viewer(Id, Options, Context);
        _ -> {ok, <<>>}
    end;
viewer([ Id | _ ], Options, Context) when is_integer(Id) ->
    viewer(Id, Options, Context);
viewer(Id, Options, Context) when is_integer(Id) ->
    case m_media:get(Id, Context) of
        MediaProps when is_map(MediaProps) -> viewer(MediaProps, Options, Context);
        undefined -> viewer1(Id, #{}, undefined, Options, Context)
    end;
viewer(Props, Options, Context) when is_map(Props) ->
    Id = maps:get(<<"id">>, Props, undefined),
    case maps:get(<<"filename">>, Props, undefined) of
        None when None =:= <<>>; None =:= undefined ->
            viewer1(Id, Props, undefined, Options, Context);
        Filename ->
            FilePath = filename_to_filepath(Filename, Context),
            viewer1(Id, Props, FilePath, Options, Context)
    end;
viewer(<<"/", Filename/binary>>, Options, Context) ->
    viewer(Filename, Options, Context);
viewer(Filename, Options, Context) when is_binary(Filename) ->
    FilePath = filename_to_filepath(Filename, Context),
    case z_media_identify:identify(FilePath, Context) of
        {ok, Props} ->
            viewer1(undefined, Props, FilePath, Options, Context);
        {error, _} ->
            % Unknown content type, we just can't display it.
            {ok, <<>>}
    end.


%% @doc Try to generate Html for the media reference.  First check if a module can do this, then
%% check the normal image tag.
viewer1(Id, Props, FilePath, Options, Context) ->
    Options1 = drop_undefined(Options),
    case z_notifier:first(#media_viewer{
            id = Id,
            props = Props,
            filename = FilePath,
            options = Options1
        }, Context)
    of
        {ok, Html} -> {ok, Html};
        undefined -> tag(Props, Options1, Context)
    end.


%% @doc Generate a HTML image tag for the image with the filename and options. The medium _must_ be in
%% a format for which we can generate a preview.  Note that this will never generate video or audio.
-spec tag(MediaReference, Options, Context) -> {ok, ImgTag} when
    MediaReference :: mediaref(),
    Options :: viewer_options(),
    Context :: z:context(),
    ImgTag :: binary().
tag(undefined, _Options, _Context) ->
    {ok, <<>>};
tag([], _Options, _Context) ->
    {ok, <<>>};
tag([ Id | _ ], Options, Context) when is_integer(Id) ->
    tag(Id, Options, Context);
tag(#rsc_list{list=[]}, _Options, _Context) ->
    {ok, <<>>};
tag(#rsc_list{list=[Id|_]}, Options, Context) ->
    tag(Id, Options, Context);
tag(Name, Options, Context) when is_atom(Name) ->
    case m_rsc:name_to_id(Name, Context) of
        {ok, Id} -> tag(Id, Options, Context);
        _ -> {ok, <<>>}
    end;
tag(Id, Options, Context) when is_integer(Id) ->
    tag(m_media:depiction(Id, Context), Options, Context);
tag(Props, Options, Context) when is_map(Props) ->
    Id = maps:get(<<"id">>, Props, undefined),
    case mediaprops_filename(Id, Props, Context) of
        None when None =:= []; None =:= <<>>; None =:= undefined ->
            {ok, <<>>};
        Filename ->
            Options1 = extra_image_options(Id, Props, Options, Context),
            tag1(Props, Filename, Options1, Context)
    end;
tag(<<"/", Filename/binary>>, Options, Context) ->
    tag(Filename, Options, Context);
tag(Filename, Options, Context) when is_binary(Filename) ->
    FilePath = filename_to_filepath(Filename, Context),
    tag1(FilePath, Filename, Options, Context);
tag({filepath, Filename, FilePath}, Options, Context) ->
    tag1(FilePath, Filename, Options, Context).


tag1(MediaRef, Filename, Options, Context) ->
    {ok, {Attrs, TagOpts}} = attributes1(MediaRef, Filename, Options, Context),
    case proplists:get_value(link, TagOpts) of
        None when None =:= []; None =:= <<>>; None =:= undefined ->
            {ok, iolist_to_binary(z_tags:render_tag("img", Attrs))};
        Link ->
            HRef = iolist_to_binary(get_link(MediaRef, Link, Context)),
            Tag = z_tags:render_tag("img", Attrs),
            {ok, iolist_to_binary(z_tags:render_tag("a", [{href,HRef}], Tag))}
    end.


%% @doc Generate a HTML image tag attributes for the image with the filename and options. The medium _must_ be in
%% a format for which we can generate a preview.  Note that this will never generate video or audio.
-spec attributes(MediaReference, Options, Context) -> {ok, Attrs} | {error, Reason} when
    MediaReference :: mediaref(),
    Options :: viewer_options(),
    Context :: z:context(),
    Attrs :: list( {atom(), iodata()} ),
    Reason :: enoent.
attributes(undefined, _Options, _Context) ->
    {error, enoent};
attributes([], _Options, _Context) ->
    {error, enoent};
attributes([ Id | _ ], Options, Context) when is_integer(Id) ->
    attributes(Id, Options, Context);
attributes(#rsc_list{list=[]}, _Options, _Context) ->
    {error, enoent};
attributes(#rsc_list{list=[Id|_]}, Options, Context) ->
    attributes(Id, Options, Context);
attributes(Name, Options, Context) when is_atom(Name) ->
    case m_rsc:name_to_id(Name, Context) of
        {ok, Id} -> attributes(Id, Options, Context);
        _ -> {error, enoent}
    end;
attributes(Id, Options, Context) when is_integer(Id) ->
    attributes(m_media:depiction(Id, Context), Options, Context);
attributes(Props, Options, Context) when is_map(Props) ->
    Id = maps:get(<<"id">>, Props, undefined),
    case mediaprops_filename(Id, Props, Context) of
        None when None =:= []; None =:= <<>>; None =:= undefined ->
            {error, enoent};
        Filename ->
            Options1 = extra_image_options(Id, Props, Options, Context),
            {ok, {Attrs, _}} = attributes1(Props, Filename, Options1, Context),
            {ok, Attrs}
    end;
attributes(<<"/", Filename/binary>>, Options, Context) ->
    attributes(Filename, Options, Context);
attributes(Filename, Options, Context) when is_binary(Filename) ->
    FilePath = filename_to_filepath(Filename, Context),
    {ok, {Attrs, _}} = attributes1(FilePath, Filename, Options, Context),
    {ok, Attrs};
attributes({filepath, Filename, FilePath}, Options, Context) ->
    {ok, {Attrs, _}} = attributes1(FilePath, Filename, Options, Context),
    {ok, Attrs}.


attributes1(_MediaRef, {filepath, Filename, FilePath}, Options, Context) ->
    attributes1(FilePath, Filename, Options, Context);
attributes1(MediaRef, Filename, Options, Context) ->
    Options1 = drop_undefined(Options),
    {url, Url, TagOpts, _ImageOpts} = url1(Filename, Options1, Context),
    % Expand the mediaclass for the correct size options
    {ok, SizeOptions} = z_mediaclass:expand_mediaclass(Options1, Context),
    TagOpts1 = case z_convert:to_bool( proplists:get_value(nowh, SizeOptions, false) ) of
        true ->
            TagOpts;
        false ->
            % Calculate the default width/height
            case z_media_preview:size(MediaRef, SizeOptions, Context) of
                {size, Width, Height, _Mime} ->
                    [
                        {width,Width},
                        {height,Height}
                        | TagOpts
                    ];
                _ ->
                    TagOpts
            end
    end,
    % Add the mediaclass to the tag's class attribute
    TagOpts2 = case proplists:get_value(mediaclass, Options1) of
        undefined ->
            TagOpts1;
        MC ->
            MC1 = <<"mediaclass-", (z_convert:to_binary(MC))/binary>>,
            case proplists:get_value(class, TagOpts1) of
                undefined ->
                    [
                        {class, MC1}
                        | TagOpts1
                    ];
                Class ->
                    [
                        {class, <<MC1/binary, " ", (z_convert:to_binary(Class))/binary>>}
                        | proplists:delete(class, TagOpts1)
                    ]
            end
    end,
    % Make sure the required alt tag is present
    TagOpts3 =  case proplists:get_value(alt, TagOpts2) of
        undefined ->
            [
                {alt,<<>>}
                | TagOpts2
            ];
        _ ->
            TagOpts2
    end,
    % Add default decoding async
    TagOpts4 =  case proplists:get_value(decoding, TagOpts3) of
        undefined ->
            [
                {decoding,<<"async">>}
                | TagOpts3
            ];
        _ ->
            TagOpts3
    end,
    % Add the optional srcset
    TagOpts5 = with_srcset(TagOpts4, Filename, Options, Context),
    % Filter some opts
    TagOpts6 = proplists:delete(link, TagOpts5),
    {Url1, TagOpts7} = case maybe_replace_gif_url(MediaRef, Url, TagOpts6, Context) of
        Url ->
            {Url, TagOpts6};
        NewUrl ->
            {NewUrl, proplists:delete(srcset, TagOpts6)}
    end,
    {ok, {[{src,Url1}|TagOpts7], TagOpts}}.


%% @doc If resizing from animated GIF to a GIF, then use the original
%% file for large animated GIFs.
maybe_replace_gif_url(#{
        <<"filename">> := Filename,
        <<"mime">> := <<"image/gif">>,
        <<"frame_count">> := FrameCount,
        <<"width">> := OrgWidth,
        <<"height">> := OrgHeight
    },
    Url, TagOpts, Context)
    when is_integer(FrameCount), FrameCount > 1, is_binary(Filename) ->
    case filename:extension(Url) of
        <<".gif">> ->
            UrlWidth = proplists:get_value(width, TagOpts),
            UrlHeight = proplists:get_value(height, TagOpts),
            case is_large_gif(OrgWidth, OrgHeight, FrameCount)
                orelse is_large_gif(UrlWidth, UrlHeight, FrameCount)
            of
                true ->
                    InlineMediaUrl = filename_to_urlpath(Filename, Context),
                    case Url of
                        <<"https:", _/binary>> ->
                            z_context:abs_url(InlineMediaUrl, Context);
                        _ ->
                            InlineMediaUrl
                    end;
                false ->
                    Url
            end;
        _ ->
            Url
    end;
maybe_replace_gif_url(_MediaRef, Url, _TagOpts, _Context) ->
    Url.

% Arbitrary cut off at 500px x 500px x 30 frames.
% More frames allows smaller image, less frames allows larger image.
is_large_gif(Width, Height, FrameCount)
    when is_integer(Width), is_integer(Height), is_integer(FrameCount) ->
    Width * Height * FrameCount > (?MAX_GIF_PIXELS * ?MAX_GIF_PIXELS * ?MAX_GIF_FRAMES);
is_large_gif(_Width, _Height, _FrameCount) ->
    false.

with_srcset(TagOptions, Filename, Options, Context) ->
    case proplists:get_value(mediaclass, Options) of
        undefined ->
            TagOptions;
        MediaClass ->
            {ok, MediaClassProps, _Hash} = z_mediaclass:get(MediaClass, Context),
            case proplists:get_value(srcset, MediaClassProps) of
                undefined ->
                    TagOptions;
                SrcSet ->
                    Result = lists:map(
                        fun({Descriptor, _Props} = SrcCandidate) ->
                            %% Inherit props from the original image
                            SrcOptions = with_srcset_props(SrcCandidate, Options, MediaClassProps),
                            {url, SrcUrl, _TagOpts, _ImageOpts} = url1(Filename, SrcOptions, Context),
                            binary_to_list(SrcUrl) ++ " " ++ Descriptor
                        end,
                        SrcSet
                    ),

                    %% Build up syntax: srcset="medium.jpg 150w, large.jpg 500w"
                    TagOptions2 = [{srcset, string:join(Result, ", ")} | TagOptions],

                    %% Optionally add sizes attribute
                    case proplists:get_value(sizes, MediaClassProps) of
                        undefined ->
                            TagOptions2;
                        Sizes ->
                            [{sizes, Sizes} | TagOptions2]
                    end
            end
    end.

%% @doc Return image properties
with_srcset_props({Descriptor, Props}, OriginalOptions, OriginalMediaClass) ->
    Width = case proplists:get_value(width, Props) of
        undefined ->
            parse_srcset_descriptor(lists:reverse(Descriptor), OriginalMediaClass);
        CandidateWidth ->
            %% Width specified in srcset props, so use that
            CandidateWidth
    end,

    OriginalMCWidth = proplists:get_value(width, OriginalMediaClass),
    OriginalMCHeight = proplists:get_value(height, OriginalMediaClass),

    Height = case proplists:get_value(height, Props) of
        undefined when is_integer(OriginalMCWidth), is_integer(OriginalMCHeight) ->
            (z_convert:to_integer(Width) / OriginalMCWidth) * OriginalMCHeight;
        undefined ->
            undefined;
        CandidateHeight ->
            CandidateHeight
    end,

    Options1 = proplists:delete(width,
            proplists:delete(height, OriginalOptions) ),
    case {Width, Height} of
        {undefined, undefined} ->
            Options1;
        {_, undefined} ->
            [ {width, Width} | Options1 ];
        {undefined, _} ->
            [ {height, Height} | Options1 ];
        {_, _} ->
            [ {width, Width}, {height, Height} | Options1 ]
    end.

%% @doc Derive width from either width or density descriptor
parse_srcset_descriptor("w" ++ Width, _Original) ->
    lists:reverse(Width);
parse_srcset_descriptor("x" ++ Density, Original) ->
    case proplists:get_value(width, Original) of
        undefined -> undefined;
        Width -> z_convert:to_integer(lists:reverse(Density)) * Width
    end.

drop_undefined(L) when is_list(L) ->
    lists:filter(
        fun
            ({_K, undefined}) -> false;
            (undefined) -> false;
            (_) -> true
        end,
        L).

get_link(Media, true, Context) ->
    Id = media_id(Media),
    case m_rsc:p(Id, website, Context) of
        None when None =:= []; None =:= <<>>; None =:= undefined ->
            m_rsc:p(Id, page_url, Context);
        Website ->
            Website
    end;
get_link(_Media, Id, Context) when is_integer(Id) ->
    m_rsc:p(Id, page_url, Context);
get_link(_Media, HRef, _Context) when is_binary(HRef); is_list(HRef) ->
    HRef.

media_id([{_,_}|_] = List) ->
    proplists:get_value(id, List);
media_id(#{ <<"id">> := Id }) ->
    Id;
media_id(_) ->
    undefined.

%% @doc Give the filepath for the filename being served.
filename_to_filepath(Filename, Context) ->
    case binary:match(Filename, [ <<"../">>, <<"\\">> ]) of
        nomatch -> filename_to_filepath_1(Filename, Context);
        _ -> <<"error">>
    end.

filename_to_filepath_1(<<"/", Filename/binary>>, Context) ->
    filename_to_filepath_1(Filename, Context);
filename_to_filepath_1(<<"lib/", RelFilename/binary>> = Filename, Context) ->
    case z_module_indexer:find(lib, RelFilename, Context) of
        {ok, #module_index{filepath=Libfile}} -> Libfile;
        _ -> Filename
    end;
filename_to_filepath_1(Filename, Context) ->
    filename:join([z_path:media_archive(Context), Filename]).


%% @doc Give the base url for the filename being served using the 'image' dispatch rule
filename_to_urlpath(Filename, Context) ->
    z_dispatcher:url_for(image, [{star, Filename}], z_context:set_language(undefined, Context)).


%% @doc Generate the url for the image with the filename and options
-spec url(MediaRef, Options, Context) -> {ok, Url} | {error, Reason} when
    MediaRef :: mediaref(),
    Options :: viewer_options(),
    Context :: z:context(),
    Url :: binary(),
    Reason :: enoent.
url(undefined, _Options, _Context) ->
    {error, enoent};
url(Name, Options, Context) when is_atom(Name) ->
    case m_rsc:name_to_id(Name, Context) of
        {ok, Id} -> url(Id, Options, Context);
        _ -> {ok, <<>>}
    end;
url(Id, Options, Context) when is_integer(Id) ->
    url(m_media:depiction(Id, Context), Options, Context);
url(Props, Options, Context) when is_map(Props) ->
    Id = maps:get(<<"id">>, Props, undefined),
    case mediaprops_filename(Id, Props, Context) of
        None when None =:= []; None =:= <<>>; None =:= undefined ->
            {ok, <<>>};
        Filename ->
            Options1 = extra_image_options(Id, Props, Options, Context),
            {url, Url, TagOptions, _ImageOptions} = url1(Filename, Options1, Context),
            Url1 = maybe_replace_gif_url(Props, Url, TagOptions, Context),
            {ok, Url1}
    end;
url(Filename, Options, Context) when is_list(Filename) ->
    url(list_to_binary(Filename), Options, Context);
url(<<"/", Filename/binary>>, Options, Context) ->
    url(Filename, Options, Context);
url(Filename, Options, Context) when is_binary(Filename) ->
    {url, Url, _TagOptions, _ImageOptions} = url1(Filename, Options, Context),
    {ok, Url}.

%% @doc Creates an url for the given filename and filters. This does not check the filename
%% or if it is convertible.
-spec url1(Filename, Options, Context) -> {url, Url, TagOptions, ImageOptions} when
    Filename :: binary() | string(),
    Options :: viewer_options(),
    Context :: z:context(),
    Url :: binary(),
    TagOptions :: proplists:proplist(),
    ImageOptions :: proplists:proplist().
url1(Filename, Options, Context) ->
    UrlAndOpts = url2(Filename, Options, Context),
    case use_absolute_url(Options, Context) of
        true ->
            {url, Url, TagOpts, ImageOpts} = UrlAndOpts,
            {url, z_dispatcher:abs_url(Url, Context), TagOpts, ImageOpts};
        false ->
            UrlAndOpts
    end.

% Given the media properties of an id, find the depicting file
% mediaprops_filename(Id, undefined, Context) ->
%     case z_notifier:first({media_stillimage, Id, []}, Context) of
%         {ok, Filename} -> Filename;
%         undefined -> undefined
%     end;
mediaprops_filename(undefined, Props, _Context) ->
    case z_convert:to_binary(maps:get(<<"preview_filename">>, Props, undefined)) of
        <<>> -> z_convert:to_binary(maps:get(<<"filename">>, Props, undefined));
        Filename -> Filename
    end;
mediaprops_filename(Id, Props, Context) ->
    case z_notifier:first({media_stillimage, Id, Props}, Context) of
        {ok, Filename} ->
            Filename;
        _ ->
            case maps:get(<<"preview_filename">>, Props, undefined) of
                undefined -> maps:get(<<"filename">>, Props, undefined);
                <<>> -> maps:get(<<"filename">>, Props, undefined);
                Filename -> Filename
            end
    end.

use_absolute_url(Options, Context) ->
    case proplists:get_value(is_data_url, Options) of
        true ->
            false;
        _ ->
            case use_absolute(proplists:get_value(absolute_url, Options)) of
                false -> false;
                true -> true;
                undefined -> z_convert:to_bool(z_context:get(absolute_url, Context))
            end
    end.

use_absolute(undefined) -> undefined;
use_absolute(<<>>) -> undefined;
use_absolute([]) -> undefined;
use_absolute(true) -> true;
use_absolute(false) -> false;
use_absolute(A) -> z_convert:to_bool(A).


url2(Filename, Options, Context) ->
    case lists:partition(fun is_tagopt/1, filter_options(Options)) of
        {TagOpts, []} ->
            {url, filename_to_urlpath(Filename, Context), TagOpts, []};
        {TagOpts, ImageOpts} ->
            % Map all ImageOpts to an opt string
            MimeFile = z_media_identify:guess_mime(Filename),
            {_Mime, Extension} = z_media_preview:out_mime(MimeFile, ImageOpts, Context),

            case props2url(ImageOpts, Context) of
                {no_checksum, UrlProps} ->
                    PropsQuoted = z_url:url_encode(UrlProps),
                    {url, filename_to_urlpath(iolist_to_binary([Filename,PropsQuoted,Extension]), Context),
                          TagOpts,
                          ImageOpts};
                {checksum, UrlProps} ->
                    Checksum = z_crypto:checksum([Filename,UrlProps,Extension], Context),
                    PropCheck = z_url:url_encode(iolist_to_binary([UrlProps,$(,Checksum,$)])),
                    {url, filename_to_urlpath(iolist_to_binary([Filename,PropCheck,Extension]), Context),
                          TagOpts,
                          ImageOpts}
            end
   end.

filter_options(Options) ->
    lists:filter(
        fun
            (is_data_url) -> false;
            (_) -> true
        end,
        Options).

is_tagopt({link,  _}) -> true;
is_tagopt({alt,   _}) -> true;
is_tagopt({title, _}) -> true;
is_tagopt({class, _}) -> true;
is_tagopt({style, _}) -> true;
is_tagopt({loading, _}) -> true;
is_tagopt({decoding, _}) -> true;
is_tagopt({align, _}) -> true;  % HTML 1.0 for e-mails

% Some preview args we definitely know exist (just an optimization)
is_tagopt({width, _}) -> false;
is_tagopt({height, _}) -> false;
is_tagopt({crop, _}) -> false;
is_tagopt({cropp, _}) -> false;
is_tagopt({grey, _}) -> false;
is_tagopt({gray, _}) -> false;
is_tagopt({mono, _}) -> false;
is_tagopt({extent, _}) -> false;
is_tagopt({upscale, _}) -> false;
is_tagopt({blur, _}) -> false;
is_tagopt({quality, _}) -> false;
is_tagopt({background, _}) -> false;
is_tagopt({lossless, _}) -> false;
is_tagopt({removebg, _}) -> false;
is_tagopt({mediaclass, _}) -> false;
is_tagopt({rotate, _}) -> false;
is_tagopt({rotate3d, _}) -> false;
is_tagopt({brightness, _}) -> false;
is_tagopt({contrast, _}) -> false;
is_tagopt({original, _}) -> false;
is_tagopt({nowh, _}) -> false;

% And be sure to keep the data-xxxx args in the tag
is_tagopt({Prop, _}) ->
    case z_convert:to_list(Prop) of
        "data_"++_ -> true;
        _ -> false
    end.


props2url([{mediaclass, _}] = Props, Context) ->
    {_Width, _Height, Acc} = props2url(Props, undefined, undefined, [], Context),
    {no_checksum, iolist_to_binary([$(,Acc,$)])};
props2url(Props, Context) ->
    {Width, Height, Acc} = props2url(Props, undefined, undefined, [], Context),
    case {Width,Height} of
        {undefined,undefined} ->
            case Acc of
                [[<<"mediaclass-">>|_]] ->
                    {no_checksum, iolist_to_binary([$(,Acc,$)])};
                _ ->
                    with_checksum([], Acc)
            end;
        {_W,undefined} ->
            with_checksum([integer_to_list(Width)] ++ "x", Acc);
        {undefined,_H} ->
            with_checksum([$x|integer_to_list(Height)], Acc);
        {_W,_H} ->
            with_checksum(integer_to_list(Width) ++ [$x|integer_to_list(Height)], Acc)
    end.

with_checksum(Size, Acc) ->
    {checksum, iolist_to_binary([$(, lists:join(")(", [Size|lists:reverse(Acc)]), $)])}.

props2url([], Width, Height, Acc, _Context) ->
    {Width, Height, Acc};
props2url([{width,Width}|Rest], _Width, Height, Acc, Context) ->
    props2url(Rest, z_convert:to_integer(Width), Height, Acc, Context);
props2url([{height,Height}|Rest], Width, _Height, Acc, Context) ->
    props2url(Rest, Width, z_convert:to_integer(Height), Acc, Context);
props2url([{absolute_url,_}|Rest], Width, Height, Acc, Context) ->
    props2url(Rest, Width, Height, Acc, Context);
props2url([{nowh, _}|Rest], Width, Height, Acc, Context) ->
    props2url(Rest, Width, Height, Acc, Context);
props2url([{crop,None}|Rest], Width, Height, Acc, Context)
    when None =:= false; None =:= undefined; None =:= <<>>; None =:= [] ->
    props2url(Rest, Width, Height, Acc, Context);
props2url([{crop,[ X, Y ]}|Rest], Width, Height, Acc, Context) ->
    R = [
        "crop-+", integer_to_list(X), "+", integer_to_list(Y)
    ],
    props2url(Rest, Width, Height, [R|Acc], Context);
props2url([{mediaclass,Class}|Rest], Width, Height, Acc, Context) ->
    case z_mediaclass:get(Class, Context) of
        {ok, [], <<>>} ->
            ?LOG_WARNING(#{
                text => <<"Ignoring unknown mediaclass">>,
                in => zotonic_core,
                mediaclass => Class
            }),
            props2url(Rest, Width, Height, Acc, Context);
        {ok, _Props, Checksum} ->
            MC = [
                <<"mediaclass-">>,
                Class,
                $.,
                Checksum
            ],
            props2url(Rest, Width, Height, [MC|Acc], Context)
    end;
props2url([{rotate3d, Args}|Rest], Width, Height, Acc, Context) ->
    R = [
        "rotate3d-",
        lists:join($,, [ z_convert:to_list(N) || N <- Args ])
    ],
    props2url(Rest, Width, Height, [R|Acc], Context);
props2url([{cropp, Args}|Rest], Width, Height, Acc, Context) ->
    R = [
        "cropp-",
        lists:join($,, [ z_convert:to_list(N) || N <- Args ])
    ],
    props2url(Rest, Width, Height, [R|Acc], Context);
props2url([{Prop}|Rest], Width, Height, Acc, Context) ->
    props2url(Rest, Width, Height, [atom_to_list(Prop)|Acc], Context);
props2url([{_Prop,undefined}|Rest], Width, Height, Acc, Context) ->
    props2url(Rest, Width, Height, Acc, Context);
props2url([{Prop,true}|Rest], Width, Height, Acc, Context) ->
    props2url(Rest, Width, Height, [atom_to_list(Prop)|Acc], Context);
props2url([{Prop,Value}|Rest], Width, Height, Acc, Context) ->
    props2url(Rest, Width, Height, [[atom_to_list(Prop),$-,z_convert:to_list(Value)]|Acc], Context).


%% @doc Translate an url of the format "image.jpg(300x300)(crop-center)(checksum).jpg" to parts
%% @todo Map the extension to the format of the preview (.jpg or .png)
-spec url2props( binary() | string(), z:context() ) ->
              {ok, {FilePath :: string(), Props :: list(), Checksum :: string(), ChecksumBaseString :: string()}}
            | {error, no_lparen | checksum_invalid | extension_invalid | badarg}.
url2props(Url, Context) when is_binary(Url) ->
    url2props(erlang:binary_to_list(Url), Context);
url2props(Url, Context) ->
    {Filepath,Rest} = lists:splitwith(fun(C) -> C =/= $( end, Url),
    PropsRoot = filename:rootname(Rest),
    % Take the checksum from the string
    FileMime = z_media_identify:guess_mime(Rest),
    case is_valid_resize_mime(FileMime) of
        true ->
            case string:rchr(PropsRoot, $() of
                0 ->
                    {error, no_lparen};
                LastParen ->
                    case lists:last(PropsRoot) of
                        $) ->
                            {Props,[$(|Check]} = lists:split(LastParen-1, PropsRoot),
                            Check1 = string:strip(Check, right, $)),
                            PropList = case Props of
                                           "()" ++ _ -> [""|string:tokens(Props, ")(")];
                                           _ -> string:tokens(Props, ")(")
                                       end,
                            {_Mime, Extension} = z_media_preview:out_mime(FileMime, map_mime_props(PropList), Context),
                            case {Check1,PropList} of
                                {"mediaclass-"++_, []} ->
                                    % shorthand with only the mediaclass
                                    {ok, {Filepath,url2props1([Check1], []),none,none}};
                                _ ->
                                    % multiple args, also needs a checksum
                                    try
                                        z_crypto:checksum_assert([Filepath,Props,Extension], Check1, Context),
                                        PropList1 = case PropList of
                                            [] ->
                                                [];
                                            [Size|RestProps]->
                                                {W,XH} = lists:splitwith(fun(C) -> C >= $0 andalso C =< $9 end, Size),
                                                SizeProps = case {W,XH} of
                                                    {"", "x"}            -> [];
                                                    {"", ""}             -> [];
                                                    {Width, ""}          -> [ {width, list_to_integer(Width)} ];
                                                    {Width, "x"}         -> [ {width, list_to_integer(Width)} ];
                                                    {"", [$x|Height]}    -> [ {height, list_to_integer(Height)} ];
                                                    {Width, [$x|Height]} ->
                                                        [
                                                            {width,list_to_integer(Width)},
                                                            {height,list_to_integer(Height)}
                                                        ]
                                                end,
                                                SizeProps ++ url2props1(RestProps, [])
                                        end,
                                        {ok, {Filepath,PropList1,Check1,Props}}
                                    catch
                                        error:checksum_invalid ->
                                            {error, checksum_invalid};
                                        error:badarg ->
                                            {error, badarg}
                                    end
                            end;
                        _ ->
                            {error, badarg}
                    end
            end;
        false ->
            {error, extension_invalid}
    end.

is_valid_resize_mime(<<"image/jpeg">>) -> true;
is_valid_resize_mime(<<"image/gif">>) -> true;
is_valid_resize_mime(<<"image/png">>) -> true;
is_valid_resize_mime(<<"image/webp">>) -> true;
is_valid_resize_mime(_) -> false.

map_mime_props(Props) ->
    [ map_mime_prop(P) || P <- Props ].

map_mime_prop("lossless") ->
    lossless;
map_mime_prop("mediaclass-"++Rest) ->
    {mediaclass, lists:takewhile(fun(C) -> C =/= $. end, Rest)};
map_mime_prop(X) ->
    X.


url2props1([], Acc) ->
    lists:reverse(Acc);
url2props1([P|Rest], Acc) ->
    {Prop,Arg} = lists:splitwith(fun(C) -> C =/= $- end, P),
    Arg1 =  case Arg of
                [$-|A] -> A;
                _ -> Arg
            end,
    case z_media_preview:string2filter(Prop, Arg1) of
        {ok, Filter} ->
            url2props1(Rest, [Filter|Acc]);
        {error, _} ->
            url2props1(Rest, Acc)
    end.

%% @doc Let moduls modify the list of preview options, unless the 'original' option is passed.
extra_image_options(Id, Props, Options, Context) ->
    case proplists:get_value(original, Options) of
        true ->
            proplists:delete(original, Options);
        _ ->
            Options1 = proplists:delete(original, Options),
            Width = maps:get(<<"width">>, Props),
            Height = maps:get(<<"height">>, Props),
            z_notifier:foldl(
                #media_preview_options{
                    id = Id,
                    width = Width,
                    height = Height,
                    options = Options
                },
                Options1,
                Context)
    end.

