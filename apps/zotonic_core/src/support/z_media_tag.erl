%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2016 Marc Worrell
%% @doc Generate media urls and html for viewing media, based on the filename, size and optional filters.
%% Does not generate media previews itself, this is done when fetching the image.
%%
%% Typical urls are like:
%% /image/2007/03/31/wedding.jpg(300x300)(crop-center)(a3ab6605e5c8ce801ac77eb76289ac12).jpg
%% /media/inline/2007/03/31/wedding.jpg
%% /media/attachment/2007/03/31/wedding.jpg

%% Copyright 2009-2016 Marc Worrell
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

    viewer/3,
    tag/3,
    url/3,
    url2props/2,

    % Export for tests
    props2url/2
]).

-include_lib("zotonic.hrl").

-compile([{parse_transform, lager_transform}]).

%% @doc Called from template, render the media viewer for some resource/medium
scomp_viewer(undefined, _Options, _Context) ->
    <<>>;
scomp_viewer(IdOrName, Options, Context) ->
    case viewer(IdOrName, Options, Context) of
        {ok, Rendered} ->
            Rendered;
        {error, Reason} ->
            lager:debug("[~p] Could not render media-viewer for ~p (~p), error ~p",
                        [z_context:site(Context), IdOrName, Options, Reason]),
            <<>>
    end.

%% @doc Called from template, render the media tag for some resource/medium
scomp_tag(undefined, _Options, _Context) ->
    <<>>;
scomp_tag(IdOrName, Options, Context) ->
    case tag(IdOrName, Options, Context) of
        {ok, Rendered} ->
            Rendered;
        {error, Reason} ->
            lager:debug("[~p] Could not render media-tag for ~p (~p), error ~p",
                        [z_context:site(Context), IdOrName, Options, Reason]),
            <<>>
    end.

%% @doc Called from template, render the media url for some resource/medium
scomp_url(undefined, _Options, _Context) ->
    <<>>;
scomp_url(IdOrName, Options, Context) ->
    case url(IdOrName, Options, Context) of
        {ok, Rendered} ->
            Rendered;
        {error, Reason} ->
            lager:debug("[~p] Could not render media-url for ~p (~p), error ~p",
                        [z_context:site(Context), IdOrName, Options, Reason]),
            <<>>
    end.


%% @spec viewer(MediaReference, Options, Context) -> {ok, HtmlFragMent} | {error, Reason}
%%   MediaReference = Filename | RscId | MediaPropList
%% @doc Generate a html fragment for displaying a medium.  This can generate audio or video player html.
viewer(undefined, _Options, _Context) ->
    {ok, []};
viewer([], _Options, _Context) ->
    {ok, []};
viewer(#rsc_list{list=[]}, _Options, _Context) ->
    {ok, []};
viewer(#rsc_list{list=[Id|_]}, Options, Context) ->
    viewer(Id, Options, Context);
viewer(Name, Options, Context) when is_atom(Name) ->
    case m_rsc:name_to_id(Name, Context) of
        {ok, Id} -> viewer(Id, Options, Context);
        _ -> {ok, []}
    end;
viewer(Id, Options, Context) when is_integer(Id) ->
    case m_media:get(Id, Context) of
        MediaProps when is_list(MediaProps) -> viewer(MediaProps, Options, Context);
        undefined -> viewer1(Id, [], undefined, Options, Context)
    end;
viewer([{_Prop, _Value}|_] = Props, Options, Context) ->
    Id = proplists:get_value(id, Props),
    case z_convert:to_list(proplists:get_value(filename, Props)) of
        None when None == []; None == undefined ->
            viewer1(Id, Props, undefined, Options, Context);
        Filename ->
            FilePath = filename_to_filepath(Filename, Context),
            viewer1(Id, Props, FilePath, Options, Context)
    end;
viewer(Filename, Options, Context) when is_binary(Filename) ->
    viewer(binary_to_list(Filename), Options, Context);
viewer(Filename, Options, Context) ->
    FilePath = filename_to_filepath(Filename, Context),
    case z_media_identify:identify(FilePath, Context) of
        {ok, Props} ->
            viewer1(undefined, Props, FilePath, Options, Context);
        {error, _} ->
            % Unknown content type, we just can't display it.
            {ok, []}
    end.


    %% @doc Try to generate Html for the media reference.  First check if a module can do this, then
    %% check the normal image tag.
    viewer1(Id, Props, FilePath, Options, Context) ->
        case z_notifier:first(#media_viewer{id=Id, props=Props, filename=FilePath, options=Options}, Context) of
            {ok, Html} -> {ok, Html};
            undefined -> tag(Props, Options, Context)
        end.


%% @spec tag(MediaReference, Options, Context) -> {ok, TagString} | {error, Reason}
%%   MediaReference = Filename | RscId | MediaPropList
%% @doc Generate a HTML image tag for the image with the filename and options. The medium _must_ be in
%% a format for which we can generate a preview.  Note that this will never generate video or audio.
tag(undefined, _Options, _Context) ->
    {ok, []};
tag([], _Options, _Context) ->
    {ok, []};
tag(#rsc_list{list=[]}, _Options, _Context) ->
    {ok, []};
tag(#rsc_list{list=[Id|_]}, Options, Context) ->
    tag(Id, Options, Context);
tag(Name, Options, Context) when is_atom(Name) ->
    case m_rsc:name_to_id(Name, Context) of
        {ok, Id} -> tag(Id, Options, Context);
        _ -> {ok, []}
    end;
tag(Id, Options, Context) when is_integer(Id) ->
    tag(m_media:depiction(Id, Context), Options, Context);
tag([{_Prop, _Value}|_] = Props, Options, Context) ->
    case mediaprops_filename(proplists:get_value(id, Props), Props, Context) of
        None when None =:= []; None =:= <<>>; None =:= undefined ->
            {ok, []};
        Filename ->
            Options1 = opt_crop_center(proplists:get_value(id, Props), Options, Context),
            tag1(Props, Filename, Options1, Context)
    end;
tag(Filename, Options, Context) when is_binary(Filename) ->
    tag(binary_to_list(Filename), Options, Context);
tag(Filename, Options, Context) when is_list(Filename) ->
    FilePath = filename_to_filepath(Filename, Context),
    tag1(FilePath, Filename, Options, Context);
tag({filepath, Filename, FilePath}, Options, Context) ->
    tag1(FilePath, Filename, Options, Context).


    tag1(_MediaRef, {filepath, Filename, FilePath}, Options, Context) ->
        tag1(FilePath, Filename, Options, Context);
    tag1(MediaRef, Filename, Options, Context) ->
        {url, Url, TagOpts, ImageOpts} = url1(Filename, Options, Context),
        TagOpts1 =
            case proplists:get_value(mediaclass, Options) of
                undefined ->
                    % Calculate the real size of the image using the options
                    case z_media_preview:size(MediaRef, ImageOpts, Context) of
                        {size, Width, Height, _Mime} ->
                            [{width,Width},{height,Height}|TagOpts];
                        _ ->
                            TagOpts
                    end;
                MC ->
                    % Add the mediaclass to the tag's class attribute
                    case proplists:get_value(class, TagOpts) of
                        undefined -> [{class, MC} | TagOpts];
                        Class -> [{class, iolist_to_binary([MC, 32, Class])} | proplists:delete(class, TagOpts)]
                    end
            end,
        % Make sure the required alt tag is present
        TagOpts2 =  case proplists:get_value(alt, TagOpts1) of
                        undefined -> [{alt,""}|TagOpts1];
                        _ -> TagOpts1
                    end,
        % Filter some opts
        case proplists:get_value(link, TagOpts) of
            None when None =:= []; None =:= <<>>; None =:= undefined ->
                {ok, iolist_to_binary(z_tags:render_tag("img", [{src,Url}|TagOpts2]))};
            Link ->
                HRef = iolist_to_binary(get_link(MediaRef, Link, Context)),
                Tag = z_tags:render_tag("img", [{src,Url}|proplists:delete(link, TagOpts2)]),
                {ok, iolist_to_binary(z_tags:render_tag("a", [{href,HRef}], Tag))}
        end.

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
    proplists:get_value(id, List).

%% @doc Give the filepath for the filename being served.
%% @todo Ensure the file is really in the given directory (ie. no ..'s)
filename_to_filepath(Filename, #context{} = Context) ->
    case Filename of
        "/" ++ _ ->
            Filename;
        "lib/" ++ RelFilename ->
            case z_module_indexer:find(lib, RelFilename, Context) of
                {ok, #module_index{filepath=Libfile}} -> Libfile;
                _ -> Filename
            end;
        _ ->
            filename:join([z_path:media_archive(Context), Filename])
    end.


%% @doc Give the base url for the filename being served using the 'image' dispatch rule
filename_to_urlpath(Filename, Context) ->
    z_dispatcher:url_for(image, [{star, Filename}], z_context:set_language(undefined, Context)).


%% @spec url(MediaRef, Options, Context) -> {ok, Url::binary()} | {error, Reason}
%% @doc Generate the url for the image with the filename and options
url(undefined, _Options, _Context) ->
    {error, enoent};
url(Name, Options, Context) when is_atom(Name) ->
    case m_rsc:name_to_id(Name, Context) of
        {ok, Id} -> url(Id, Options, Context);
        _ -> {ok, []}
    end;
url(Id, Options, Context) when is_integer(Id) ->
    url(m_media:depiction(Id, Context), Options, Context);
url([{_Prop, _Value}|_] = Props, Options, Context) ->
    Id = proplists:get_value(id, Props),
    case mediaprops_filename(Id, Props, Context) of
        None when None =:= []; None =:= <<>>; None =:= undefined ->
            {ok, []};
        Filename ->
            Options1 = opt_crop_center(Id, Options, Context),
            {url, Url, _TagOptions, _ImageOptions} = url1(Filename, Options1, Context),
            {ok, Url}
    end;
url(Filename, Options, Context) ->
    {url, Url, _TagOptions, _ImageOptions} = url1(Filename, Options, Context),
    {ok, Url}.


%% @spec url1(Filename, Options, Context) -> {url, Url::binary(), TagOptions, ImageOpts} | {error, Reason}
%% @doc Creates an url for the given filename and filters.  This does not check the filename or if it is convertible.
url1(File, Options, Context) ->
    UrlAndOpts = url2(File, Options, Context),
    case use_absolute_url(Options, Context) of
        true ->
            {url, Url, TagOpts, ImageOpts} = UrlAndOpts,
            {url, z_dispatcher:abs_url(Url, Context), TagOpts, ImageOpts};
        false ->
            UrlAndOpts
    end.

% Given the media properties of an id, find the depicting file
mediaprops_filename(Id, undefined, Context) ->
    case z_notifier:first({media_stillimage, Id, []}, Context) of
        {ok, Filename} -> Filename;
        undefined -> undefined
    end;
mediaprops_filename(undefined, Props, _Context) ->
    case z_convert:to_list(proplists:get_value(preview_filename, Props)) of
        [] -> z_convert:to_list(proplists:get_value(filename, Props));
        Filename -> Filename
    end;
mediaprops_filename(Id, Props, Context) ->
    case z_notifier:first({media_stillimage, Id, Props}, Context) of
        {ok, Filename} -> Filename;
        _ -> case z_convert:to_list(proplists:get_value(preview_filename, Props)) of
                 [] -> z_convert:to_list(proplists:get_value(filename, Props));
                 Filename -> Filename
             end
    end.

use_absolute_url(Options, Context) ->
    case use_absolute(proplists:get_value(absolute_url, Options)) of
        false -> false;
        true -> true;
        undefined -> z_convert:to_bool(z_context:get(absolute_url, Context))
    end.

use_absolute(undefined) -> undefined;
use_absolute(<<>>) -> undefined;
use_absolute([]) -> undefined;
use_absolute(true) -> true;
use_absolute(false) -> false;
use_absolute(A) -> z_convert:to_bool(A).


url2(File, Options, Context) ->
    Filename = z_convert:to_list(File),
    {TagOpts, ImageOpts} = lists:partition(fun is_tagopt/1, Options),
    % Map all ImageOpts to an opt string
    MimeFile = z_media_identify:guess_mime(Filename),
    {_Mime,Extension} = z_media_preview:out_mime(MimeFile, ImageOpts, Context),
    case props2url(ImageOpts, Context) of
        {no_checksum, UrlProps} ->
            PropsQuoted = mochiweb_util:quote_plus(UrlProps),
            {url, filename_to_urlpath(lists:flatten([Filename,PropsQuoted,Extension]), Context),
                  TagOpts,
                  ImageOpts};
        {checksum, UrlProps} ->
            Checksum = z_utils:checksum([Filename,UrlProps,Extension], Context),
            PropCheck = mochiweb_util:quote_plus(iolist_to_binary([UrlProps,$(,Checksum,$)])),
            {url, filename_to_urlpath(lists:flatten([Filename,PropCheck,Extension]), Context),
                  TagOpts,
                  ImageOpts}
    end.


is_tagopt({link,  _}) -> true;
is_tagopt({alt,   _}) -> true;
is_tagopt({title, _}) -> true;
is_tagopt({class, _}) -> true;
is_tagopt({style, _}) -> true;
is_tagopt({align, _}) -> true;  % HTML 1.0 for e-mails

% Some preview args we definitely know exist (just an optimization)
is_tagopt({width, _}) -> false;
is_tagopt({height, _}) -> false;
is_tagopt({crop, _}) -> false;
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
    {checksum, iolist_to_binary([$(, z_utils:combine(")(", [Size|lists:reverse(Acc)]), $)])}.

props2url([], Width, Height, Acc, _Context) ->
    {Width, Height, Acc};
props2url([{crop,None}|Rest], Width, Height, Acc, Context) when None == false; None == undefined; None == <<>>; None == [] ->
    props2url(Rest, Width, Height, Acc, Context);
props2url([{width,Width}|Rest], _Width, Height, Acc, Context) ->
    props2url(Rest, z_convert:to_integer(Width), Height, Acc, Context);
props2url([{height,Height}|Rest], Width, _Height, Acc, Context) ->
    props2url(Rest, Width, z_convert:to_integer(Height), Acc, Context);
props2url([{absolute_url,_}|Rest], Width, Height, Acc, Context) ->
    props2url(Rest, Width, Height, Acc, Context);
props2url([{mediaclass,Class}|Rest], Width, Height, Acc, Context) ->
    case z_mediaclass:get(Class, Context) of
        {ok, [], <<>>} ->
            lager:warning("unknown mediaclass ~p", [Class]),
            props2url(Rest, Width, Height, Acc, Context);
        {ok, _Props, Checksum} ->
            MC = [
                <<"mediaclass-">>,
                Class,
                $.,
                Checksum
            ],
            props2url(Rest, Width, Height, [MC|Acc], Context);
        {error, _Reason} = Error ->
            lager:info("error looking up mediaclass ~p: ~p", [Class, Error]),
            props2url(Rest, Width, Height, Acc, Context)
    end;
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
-spec url2props(binary()|string(), #context{}) ->
            {ok, {FilePath :: string(), Props :: list(), Checksum :: string(), ChecksumBaseString :: string()}} |
            {error, no_lparen|checksum_invalid|badarg}.
url2props(Url, Context) when is_binary(Url) ->
    url2props(erlang:binary_to_list(Url), Context);
url2props(Url, Context) ->
    {Filepath,Rest} = lists:splitwith(fun(C) -> C =/= $( end, Url),
    PropsRoot = filename:rootname(Rest),
    % Take the checksum from the string
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
                    FileMime = z_media_identify:guess_mime(Rest),
                    {_Mime, Extension} = z_media_preview:out_mime(FileMime, map_mime_props(PropList), Context),
                    case {Check1,PropList} of
                        {"mediaclass-"++_, []} ->
                            % shorthand with only the mediaclass
                            {ok, {Filepath,url2props1([Check1], []),none,none}};
                        _ ->
                            % multiple args, also needs a checksum
                            try
                                z_utils:checksum_assert([Filepath,Props,Extension], Check1, Context),
                                PropList1 = case PropList of
                                                [] ->
                                                    [];
                                                [Size|RestProps]->
                                                    {W,XH} = lists:splitwith(fun(C) -> C >= $0 andalso C =< $9 end, Size),
                                                    SizeProps = case {W,XH} of
                                                                    {"", "x"}            -> [];
                                                                    {"", ""}             -> [];
                                                                    {Width, ""}          -> [{width,list_to_integer(Width)}];
                                                                    {Width, "x"}         -> [{width,list_to_integer(Width)}];
                                                                    {"", [$x|Height]}    -> [{height,list_to_integer(Height)}];
                                                                    {Width, [$x|Height]} -> [{width,list_to_integer(Width)},{height,list_to_integer(Height)}]
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
    end.

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
    Filter = z_media_preview:string2filter(Prop, Arg1),
    url2props1(Rest, [Filter|Acc]).



opt_crop_center(Id, Options, Context) ->
    Crop = proplists:get_value(crop, Options),
    case is_crop_center(Crop) of
        true -> maybe_add_crop_center(Id, Options, Context);
        false when Crop =:= undefined ->
            opt_crop_center_mediaclass(proplists:get_value(mediaclass, Options), Id, Options, Context);
        false -> Options
    end.

opt_crop_center_mediaclass(undefined, _Id, Options, _Context) ->
    Options;
opt_crop_center_mediaclass(Mediaclass, Id, Options, Context) ->
    {ok, Props, _Hash} = z_mediaclass:get(Mediaclass, Context),
    case is_crop_center(proplists:get_value(crop, Props)) of
        true -> maybe_add_crop_center(Id, Options, Context);
        false -> Options
    end.

maybe_add_crop_center(Id, Options, Context) ->
    case m_rsc:p_no_acl(Id, crop_center, Context) of
        <<"+", _/binary>> = Center ->
            z_utils:prop_replace(crop, Center, Options);
        _ ->
            Options
    end.

is_crop_center(true) -> true;
is_crop_center(center) -> true;
is_crop_center(<<"center">>) -> true;
is_crop_center("center") -> true;
is_crop_center(_) -> false.

