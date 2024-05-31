%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Make still previews of media, using image manipulation functions.  Resize, crop, grey, etc.
%% This uses the command line imagemagick tools for all image manipulation.
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

-module(z_media_preview).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    convert/4,
    convert/5,
    size/3,
    can_generate_preview/1,
    out_mime/3,
    out_mime/2,
    string2filter/2,
    cmd_args/3,
    calc_size/1
]).

% Max pixels of the target image
% Larger will be truncated to this size.
-define(MAX_PIXSIZE,  20000).

% Low and max image size (in total pixels) for quality 99 and 55.
% A small thumbnail needs less compression to keep image quality.
-define(PIX_Q99, 1000).
-define(PIX_Q50, 250000).

-include_lib("zotonic.hrl").


%% @doc Convert the Infile to an outfile with a still image using the filters.
-spec convert(file:filename_all(), file:filename_all(), list(), #context{}) -> ok | {error, term()}.
convert(InFile, InFile, _, _Context) ->
    ?LOG_ERROR(#{
        text => <<"Image convert will overwrite input file">>,
        in => zotonic_core,
        result => error,
        reason => will_overwrite_infile,
        file => InFile
    }),
    {error, will_overwrite_infile};
convert(InFile, OutFile, Filters, Context) ->
    convert(InFile, InFile, OutFile, Filters, Context).


convert(InFile, MediumFilename, OutFile, Filters, Context) ->
    case z_media_identify:identify(InFile, MediumFilename, MediumFilename, Context) of
        {ok, #{ <<"mime">> := Mime } = FileProps} ->
            case can_generate_preview(Mime) of
                true ->
                    case z_mediaclass:expand_mediaclass_checksum(Filters) of
                        {ok, FiltersExpanded} ->
                            SiteDir = z_path:site_dir(Context),
                            convert_1(os:find_executable("convert"), InFile, OutFile, Mime, FileProps, FiltersExpanded, SiteDir);
                        {error, Reason} = Error ->
                            ?LOG_WARNING(#{
                                text => <<"Cannot expand mediaclass">>,
                                in => zotonic_core,
                                result => error,
                                reason => Reason,
                                filters => Filters
                            }),
                            Error
                    end;
                false ->
                    ?LOG_NOTICE(#{
                        text => <<"cannot convert mime type">>,
                        in => zotonic_core,
                        mime => Mime,
                        file => unicode:characters_to_binary(InFile)
                    }),
                    {error, mime_type}
            end;
        {ok, _} ->
            {error, mime_type};
        {error, Reason} ->
            {error, Reason}
    end.

convert_1(false, _InFile, _OutFile, _InMime, _FileProps, _Filters, _SiteDir) ->
    ?LOG_ERROR(#{
        text => <<"Install ImageMagick 'convert' to generate previews of images.">>,
        in => zotonic_core
    }),
    {error, convert_missing};
convert_1(ConvertCmd, InFile, OutFile, InMime, FileProps, Filters, SiteDir) ->
    OutMime = z_media_identify:guess_mime(OutFile),
    case cmd_args(FileProps, Filters, OutMime) of
        {ok, {EndWidth, EndHeight, _CmdArgs}} when EndWidth > ?MAX_PIXSIZE; EndHeight > ?MAX_PIXSIZE ->
            {error, image_too_big};
        {ok, {_, _, CmdArgs}} ->
            convert_2(CmdArgs, ConvertCmd, InFile, OutFile, InMime, FileProps, SiteDir);
        {error, _} = Error ->
            Error
    end.

convert_2(CmdArgs, ConvertCmd, InFile, OutFile, InMime, FileProps, SiteDir) ->
    file:delete(OutFile),
    ok = z_filelib:ensure_dir(OutFile),
    Cmd = lists:flatten([
        "cd ", z_filelib:os_filename(SiteDir), "; ",
        z_filelib:os_filename(ConvertCmd), " ",
        opt_density(FileProps),
        z_filelib:os_filename( unicode:characters_to_list(InFile) ++ infile_suffix(InMime) ), " ",
        lists:flatten(lists:join(32, CmdArgs)), " ",
        z_filelib:os_filename(OutFile)
    ]),
    case run_cmd(Cmd, OutFile) of
        ok ->
            case filelib:is_regular(OutFile) of
                true ->
                    ok;
                false ->
                    case filelib:is_regular(InFile) of
                        false -> {error, enoent};
                        true -> {error, convert_error}
                    end
            end;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                text => <<"convert cmd failed">>,
                in => zotonic_core,
                command => unicode:characters_to_binary(Cmd),
                reason => Reason
            }),
            Error
    end.

% We need to set a higher density for PDF rendering, otherwise the resulting
% image is too small.
opt_density(#{ <<"mime">> := <<"application/pdf">> }) -> " -density 150x150 ";
opt_density(_) -> "".

run_cmd(Cmd, OutFile) ->
    jobs:run(media_preview_jobs,
            fun() ->
                case filelib:is_regular(OutFile) of
                    true -> ok;
                    false -> once(Cmd, OutFile)
                end
            end).


once(Cmd, OutFile) ->
    Key = {n,l,Cmd},
    CmdBin = unicode:characters_to_binary(Cmd),
    case gproc:reg_or_locate(Key) of
        {Pid, _} when Pid =:= self() ->
            ?LOG_DEBUG(#{
                in => zotonic_core,
                text => <<"ImageMagick convert command started">>,
                command => CmdBin
            }),
            Result = z_exec:run(CmdBin),
            gproc:unreg(Key),
            case Result of
                {ok, StdOut} ->
                    case filelib:is_regular(OutFile) of
                        true ->
                            ok;
                        false ->
                            ?LOG_ERROR(#{
                                in => zotonic_core,
                                text => <<"ImageMagick convert command failed - no output file">>,
                                result => error,
                                reason => enoent,
                                command => CmdBin,
                                stdout => StdOut
                            }),
                            {error, convert_error}
                    end;
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        in => zotonic_core,
                        text => <<"ImageMagick convert command failed">>,
                        result => error,
                        reason => Reason,
                        command => CmdBin
                    }),
                    {error, convert_error}
            end;
        {_OtherPid, _} ->
            ?LOG_DEBUG(#{
                text => "Waiting for parallel resizer",
                in => zotonic_core,
                command => CmdBin
            }),
            Ref = gproc:monitor(Key),
            receive
                {gproc, unreg, Ref, Key} ->
                    ok
            end
    end.


%% Return the ImageMagick input-file suffix.
%% @spec infile_suffix(Mime) -> Suffix::string()
infile_suffix(<<"image/gif">>) -> [];
infile_suffix(_) -> "[0]".


%% @spec size(MediaRef, Filters, Context) -> {size, Width, Height, ResizedMime} | {error, Reason}
%%   MediaRef = Filename | MediaProps
%% @doc Calculate the size of the resulting image.
size(Props, Filters, _Context) when is_map(Props) ->
    size_props(Props, Filters);
size(InFile, Filters, Context) when is_binary(InFile); is_integer(InFile) ->
    case z_media_identify:identify(InFile, Context) of
        {ok, FileProps} ->
            size_props(FileProps, Filters);
        {error, Reason} ->
            {error, Reason}
    end.


size_props(#{ <<"mime">> := Mime, <<"width">> := ImageWidth, <<"height">> := ImageHeight } = FileProps, Filters) ->
    case can_generate_preview(Mime) of
        true ->
            Orientation = maps:get(<<"orientation">>, FileProps, 1),
            ReqWidth = z_convert:to_integer(proplists:get_value(width, Filters)),
            ReqHeight = z_convert:to_integer(proplists:get_value(height, Filters)),
            {CropPar,_Filters1} = fetch_crop(Filters),
            {ResizeWidth,ResizeHeight,CropArgs} = calc_size(
                #{
                    req_width => ReqWidth,
                    req_height => ReqHeight,
                    image_width => ImageWidth,
                    image_height => ImageHeight,
                    crop => CropPar,
                    orientation => Orientation,
                    filters => Filters,
                    is_upscale => is_enabled(upscale, Filters)
                }),
            case CropArgs of
                none ->
                    case is_enabled(extent, Filters) of
                        true when is_integer(ReqWidth) andalso is_integer(ReqHeight) ->
                            {size, ReqWidth, ReqHeight, <<"image/jpeg">>};
                        _ ->
                            {size, ResizeWidth, ResizeHeight, <<"image/jpeg">>}
                    end;
                {_CropL, _CropT, CropWidth, CropHeight} ->
                    {size, CropWidth, CropHeight, <<"image/jpeg">>}
            end;
        false ->
            {error, {no_preview_for_mimetype, Mime}}
    end;
size_props(Props, _Filters) ->
    {error, {no_preview_for_mimetype, maps:get(<<"mime">>, Props, undefined)}}.


%% @doc Check if we can generate a preview image of the given mime type
-spec can_generate_preview( binary() | string() ) -> boolean().
can_generate_preview(<<"image/", _/binary>>) -> true;
can_generate_preview(<<"application/pdf">>) -> true;
can_generate_preview(<<"application/postscript">>) -> true;
can_generate_preview(B) when is_list(B) -> can_generate_preview(list_to_binary(B));
can_generate_preview(_Mime) -> false.


%% @doc Map filters to commandline options
cmd_args(#{ <<"mime">> := Mime, <<"width">> := ImageWidth, <<"height">> := ImageHeight } = FileProps, Filters, OutMime) ->
    Orientation = maps:get(<<"orientation">>, FileProps, 1),
    ReqWidth   = proplists:get_value(width, Filters),
    ReqHeight  = proplists:get_value(height, Filters),
    {CropPar,Filters1} = fetch_crop(Filters),

    {ResizeWidth,ResizeHeight,CropArgs} = calc_size(
                #{
                    req_width => ReqWidth,
                    req_height => ReqHeight,
                    image_width => ImageWidth,
                    image_height => ImageHeight,
                    crop => CropPar,
                    orientation => Orientation,
                    filters => Filters,
                    is_upscale => is_enabled(upscale, Filters)
                }),
    {FiltersPreResize, FiltersPostResize} = lists:partition(
        fun
            ({rotate3d, _}) -> true;
            ({rotate, _}) -> true;
            ({cropp, _}) -> true;
            (_) -> false
        end,
        Filters1),
    Filters2   = [  {make_image, Mime},
                    {correct_orientation, Orientation}
                ] ++ FiltersPreResize ++ [
                    {resize, ResizeWidth, ResizeHeight, is_enabled(upscale, Filters)},
                    {colorspace, z_config:get(default_colorspace, "sRGB")},
                    {crop, CropArgs},
                    {density, 72}
                ] ++ FiltersPostResize,
    Filters3 = case {CropArgs,is_enabled(extent, Filters)} of
                    {none,true} -> Filters2 ++ [{extent, ReqWidth, ReqHeight}];
                    _ -> Filters2
                end,
    Filters4 = case is_blurred(Filters3) of
                    true ->  Filters3;
                    false -> case is_lossless(Mime) of
                                true -> Filters3;
                                false -> Filters3 ++ [sharpen_small]
                             end
               end,
    Filters5 = case proplists:get_value(background, Filters4) of
                    undefined -> default_background(OutMime) ++ Filters4;
                    _ -> Filters4
               end,
    Filters6 = add_optional_quality(Filters5, is_lossless(OutMime), ResizeWidth, ResizeHeight),
    Filters7 = add_optional_interlace(Filters6, OutMime),
    Filters8 = move_pre_post_filters(Filters7),
    Filters9 = case lists:member(coalesce, Filters8) of
        true ->
            Filters8 ++ [deconstruct];
        false ->
            Filters8
    end,
    {EndWidth,EndHeight,Args} = lists:foldl(fun (Filter, {W,H,Acc}) ->
                                                {NewW,NewH,Arg} = filter2arg(Filter, W, H, Filters7),
                                                {NewW,NewH,[Arg|Acc]}
                                            end,
                                            {ImageWidth,ImageHeight,[]},
                                            Filters9),
    {ok, {EndWidth, EndHeight, ["-strip" | lists:reverse(Args) ]}};
cmd_args(_, _Filters, _OutMime) ->
    {error, no_size}.

default_background(<<"image/gif">>) -> [coalesce];
default_background(<<"image/png">>) -> [coalesce];
default_background(<<"image/webp">>) -> [coalesce];
default_background(_) -> [{background,"white"}, {layers,"flatten"}].

%% @doc Check if there is a blurring filter that prevents us from sharpening the resulting image
is_blurred([]) -> false;
is_blurred([blur|_]) -> true;
is_blurred([{blur, _}|_]) -> true;
is_blurred([_|Rest]) -> is_blurred(Rest).

is_lossless(<<"image/gif">>) -> true;
is_lossless(<<"image/png">>) -> true;
is_lossless(_) -> false.

is_enabled(_F, []) -> false;
is_enabled(F, [F|_]) -> true;
is_enabled(F, [{F, Val}|_]) -> z_convert:to_bool(Val);
is_enabled(F, [_|R]) -> is_enabled(F, R).

move_pre_post_filters(Fs) ->
    lists:filter(fun is_pre_filter/1, Fs)
        ++ lists:filter(fun(F) -> not is_pre_filter(F) andalso not is_post_filter(F) end, Fs)
        ++ lists:filter(fun is_post_filter/1, Fs).

is_pre_filter({pre_magick, _}) -> true;
is_pre_filter(_) -> false.

is_post_filter({post_magick, _}) -> true;
is_post_filter(_) -> false.

add_optional_quality(Fs, true, _W, _H) ->
    Fs;
add_optional_quality(Fs, false, W, H) ->
    case proplists:is_defined(quality, Fs) of
        true -> Fs;
        false -> add_optional_quality_1(Fs, W*H)
    end.

add_optional_quality_1(Fs, Pixels) when Pixels =< ?PIX_Q99 ->
    Fs ++ [{quality, 99}];
add_optional_quality_1(Fs, Pixels) when Pixels >= ?PIX_Q50 ->
    Fs ++ [{quality, 50}];
add_optional_quality_1(Fs, Pixels) ->
    % Calculate the quality on a lineair scale between PIX_Q50 and PIX_Q99
    Q = 99 - round(50 * (Pixels - ?PIX_Q99) / (?PIX_Q50 - ?PIX_Q99)),
    Fs ++ [{quality, Q}].

%% @doc Make JPEG images interlaced for nicer incremental loading
add_optional_interlace(Fs, <<"image/jpeg">>) ->
    case proplists:is_defined(interlace, Fs) of
        false ->
            Fs ++ [ {interlace, "plane"} ];
        true ->
            Fs
    end;
add_optional_interlace(Fs, _) ->
    Fs.

%% @doc Determine the output mime type, after expanding optional mediaclass arguments.
out_mime(Mime, Options, Context) ->
    {ok, Options1} = z_mediaclass:expand_mediaclass(Options, Context),
    out_mime(Mime, Options1).

%% @doc Return the preferred mime type of the image generated by resizing an image of a certain type and size.
-spec out_mime( InMime :: binary(), list() ) -> { OutMime :: binary(), Extension :: string()}.
out_mime(Mime, Options) ->
    Mime1 = case proplists:get_value(format, Options) of
                webp ->
                    <<"image/webp">>;
                undefined -> 
                    Mime
            end,
    out_mime1(get_lossless_value(Options), Mime1).


out_mime1(_,     <<"image/gif">>)  -> {<<"image/gif">>,  ".gif"};
out_mime1(_,     <<"image/webp">>) -> {<<"image/webp">>, ".webp"};
out_mime1(false, _Mime)            -> {<<"image/jpeg">>, ".jpg"};
out_mime1(true,  _Mime)            -> {<<"image/png">>,  ".png"};
out_mime1(auto,  <<"image/png">>)  -> {<<"image/png">>,  ".png"};
out_mime1(auto,  _)                -> {<<"image/jpeg">>, ".jpg"}.


get_lossless_value(Options) ->
    ValueOpt = lists:foldl(fun({K, V}, Acc) ->
                                   case lists:member(K, Options) of true -> V; false -> Acc end
                           end,
                           undefined,
                           [{"lossless", true}, {"lossless-true", true}, {"lossless-auto", auto},
                            {"lossless-false", false}]),
    case ValueOpt of
        undefined -> z_convert:to_atom(proplists:get_value(lossless, Options, false));
        V -> V
    end.

%% @doc Map filters to an ImageMagick argument
filter2arg({make_image, <<"application/pdf">>}, Width, Height, _AllFilters) ->
    RArg = ["-resize ", integer_to_list(Width),$x,integer_to_list(Height)],
    {Width, Height, RArg};
filter2arg(coalesce, Width, Height, _AllFilters) ->
    {Width, Height, "-coalesce"};
filter2arg(deconstruct, Width, Height, _AllFilters) ->
    {Width, Height, "-deconstruct"};
filter2arg({make_image, Mime}, Width, Height, _AllFilters) when is_binary(Mime) ->
    {Width, Height, []};
filter2arg({correct_orientation, Orientation}, Width, Height, _AllFilters) ->
    case Orientation of
        2 -> {Width, Height, "-flip"};
        3 -> {Width, Height, "-rotate 180"};
        4 -> {Width, Height, "-flop"};
        5 -> {Height, Width, "-transpose"};
        6 -> {Height, Width, "-rotate 90"};
        7 -> {Height, Width, "-transverse"};
        8 -> {Height, Width, "-rotate 270"};
        _ -> {Width, Height, []}
    end;
filter2arg({rotate, Rotation}, Width, Height, _AllFilters) ->
    case Rotation of
        -90 -> {Height, Width, "-rotate 270"};
        -180 -> {Width, Height, "-rotate 180"};
        -270 -> {Height, Width, "-rotate 90"};
        90 -> {Height, Width, "-rotate 90"};
        180 -> {Width, Height, "-rotate 180"};
        270 -> {Height, Width, "-rotate 270"};
        _ -> {Width, Height, ""}
    end;
filter2arg({background, Color}, Width, Height, _AllFilters) ->
    {Width, Height, ["-background ", $", z_filelib:os_escape(Color), $"]};
filter2arg({layers, Method}, Width, Height, _AllFilters) ->
    {Width, Height, ["-layers ", $", z_filelib:os_escape(Method), $"]};
filter2arg({colorspace, Colorspace}, Width, Height, _AllFilters) ->
    {Width, Height, ["-colorspace ", $", z_filelib:os_escape(Colorspace), $"]};
filter2arg({density, DPI}, Width, Height, _AllFilters) when is_integer(DPI) ->
    {Width, Height, ["-set units PixelsPerInch -density ", integer_to_list(DPI)]};
filter2arg({interlace, Interlace}, Width, Height, _AllFilters) ->
    {Width, Height, ["-interlace ", $", z_filelib:os_escape(Interlace), $" ]};
filter2arg({width, _}, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({height, _}, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({resize, Width, Height, _}, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({resize, EndWidth, EndHeight, false}, Width, Height, _AllFilters)
  when Width =< EndWidth andalso Height =< EndHeight ->
    %% No scaling up, keep original image dimensions
    {Width, Height, []};
filter2arg({resize, EndWidth, EndHeight, true}, Width, Height, _AllFilters)
  when Width < EndWidth andalso Height < EndHeight ->
    % Scale up
    EArg = ["-resize ", integer_to_list(EndWidth),$x,integer_to_list(EndHeight)],
    RArg = ["-thumbnail ", z_filelib:os_escape([integer_to_list(EndWidth),$x,integer_to_list(EndHeight),$!])],
    {EndWidth, EndHeight, [EArg, 32, RArg]};
filter2arg({extent, EndWidth, EndHeight}, Width, Height, _AllFilters) when EndWidth == undefined orelse EndHeight == undefined ->
    {Width, Height, []};
filter2arg({extent, EndWidth, EndHeight}, Width, Height, _AllFilters) when Width /= EndWidth orelse Height /= EndHeight ->
    GArg = "-gravity Center",
    EArg = ["-extent ", integer_to_list(EndWidth),$x,integer_to_list(EndHeight)],
    {EndWidth, EndHeight, [GArg, 32, EArg]};
filter2arg({resize, EndWidth, EndHeight, _}, _Width, _Height, _AllFilters) ->
    GArg = "-gravity NorthWest",
    RArg = ["-thumbnail ", z_filelib:os_escape([integer_to_list(EndWidth),$x,integer_to_list(EndHeight),$!])],
    {EndWidth, EndHeight, [GArg, 32, RArg]};
filter2arg({crop, none}, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({crop, {CropL, CropT, CropWidth, CropHeight}}, _Width, _Height, _AllFilters) ->
    GArg = "-gravity NorthWest",
    CArg = ["-crop ",   integer_to_list(CropWidth),$x,integer_to_list(CropHeight),
                        $+,integer_to_list(CropL),$+,integer_to_list(CropT)],
    EArg = ["-extent ",   integer_to_list(CropWidth),$x,integer_to_list(CropHeight)],
    RArg = "+repage",
    {CropWidth, CropHeight, [GArg,32,CArg,32,EArg,32,RArg]};
filter2arg(grey, Width, Height, _AllFilters) ->
    {Width, Height, "-colorspace Gray"};
filter2arg(mono, Width, Height, _AllFilters) ->
    {Width, Height, "-monochrome"};
filter2arg(flip, Width, Height, _AllFilters) ->
    {Width, Height, "-flip"};
filter2arg(flop, Width, Height, _AllFilters) ->
    {Width, Height, "-flop"};
filter2arg(blur, Width, Height, _AllFilters) ->
    filter2arg({blur, 10}, Width, Height, _AllFilters);
filter2arg({blur, Blur}, Width, Height, _AllFilters) when is_integer(Blur) ->
    {Width, Height, ["-blur ", integer_to_list(Blur)]};
filter2arg({blur, Blur}, Width, Height, _AllFilters) when is_list(Blur) ->
    case string:tokens(Blur, "x") of
        [A,B] -> {Width, Height, ["-blur ", ensure_integer(A), $x, ensure_integer(B)]};
        [A] ->   {Width, Height, ["-blur ", ensure_integer(A)]}
    end;
filter2arg(sharpen_small, Width, Height, _AllFilters) when Width < 400 andalso Height < 400 ->
    {Width, Height, "-unsharp 0.3x0.7 "}; % 6x3+1+0
filter2arg(sharpen_small, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({lossless, _}, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg(lossless, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({quality, Q}, Width, Height, _AllFilters) ->
    {Width,Height, ["-quality ",integer_to_list(Q)]};
filter2arg({removebg, MatteFuzz}, Width, Height, AllFilters) ->
    % Check if this is also contains a matter color.
    {Matte, Fuzz} = case binary:split(z_convert:to_binary(MatteFuzz), <<",">>) of
        [ M, F ] ->
            {"-mattecolor "++z_filelib:os_escape(z_convert:to_list(M)), z_convert:to_integer(F)};
        F ->
            {"-matte", z_convert:to_integer(F)}
    end,
    Filter = case lists:member(lossless, AllFilters) of
                 true ->
                     %% PNG images get the alpha channel flood-filled to remove the background.
                     [Matte ++ " -fill none -fuzz ", integer_to_list(Fuzz), "% ",
                      "-draw 'matte 0,0 floodfill' ",
                      "-draw 'matte 0,", integer_to_list(Height-1), " floodfill' ",
                      "-draw 'matte ", integer_to_list(Width-1), ",0 floodfill' ",
                      "-draw 'matte ", integer_to_list(Width-1), ",", integer_to_list(Height-1), " floodfill' "
                     ];
                 false ->
                     %% JPEG images get flood-filled with white to remove the background.
                     [Matte ++ "-fill white -fuzz ", integer_to_list(Fuzz), "% ",
                      "-draw 'color 0,0 floodfill' ",
                      "-draw 'color 0,", integer_to_list(Height-1), " floodfill' ",
                      "-draw 'color ", integer_to_list(Width-1), ",0 floodfill' ",
                      "-draw 'color ", integer_to_list(Width-1), ",", integer_to_list(Height-1), " floodfill' "
                     ]
             end,
    {Width, Height, Filter};
% Custom ImageMagick command line arguments -- only available from a mediaclass file
filter2arg({magick, Arg}, Width, Height, _AllFilters) ->
    {Width, Height, z_convert:to_list(Arg)};
filter2arg({pre_magick, Arg}, Width, Height, _AllFilters) ->
    {Width, Height, z_convert:to_list(Arg)};
filter2arg({post_magick, Arg}, Width, Height, _AllFilters) ->
    {Width, Height, z_convert:to_list(Arg)};
% Ignore these (are already handled as other filter args)
filter2arg(extent, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({extent, _}, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({extent, _, _}, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg(upscale, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg(nowh, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({srcset, _Arg}, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({sizes, _Arg}, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({rotate3d, Args}, Width, Height, _AllFilters) ->
    [R, T, P] = case Args of
        [ Roll ] -> [ Roll, 0, 0 ];
        [ Roll, Tilt ] -> [ Roll, Tilt, 0 ];
        [ Roll, Tilt, Angle ] -> [ Roll, Tilt, Angle ]
    end,
    case z_media_rotate3d:rotate3d(Width, Height, R, T, P) of
        {ok, Distort} ->
            {Width, Height, Distort};
        {error, _} ->
            {Width, Height, []}
    end;
filter2arg({cropp, Args}, Width, Height, _AllFilters) ->
    {W, H, _Left, _Top, Command} = cropp(Width, Height, Args),
    {W, H, Command};
filter2arg({brightness, Arg}, Width, Height, AllFilters) ->
    Contrast = proplists:get_value(contrast, AllFilters, 0),
    {Width, Height, brightness_contrast(Arg, Contrast)};
filter2arg({contrast, Arg}, Width, Height, AllFilters) ->
    case proplists:get_value(brightness, AllFilters) of
        undefined ->
            {Width, Height, brightness_contrast(0, Arg)};
        _ ->
            {Width, Height, ""}
    end;
filter2arg({format, webp}, Width, Height, AllFilters) ->
    case get_lossless_value(AllFilters) of
        true ->
            {Width, Height, "-define webp:lossless=true"};
        false ->
            {Width, Height, []};
        auto ->
            {make_image, InMime} = proplists:lookup(make_image, AllFilters),
            case is_lossless(InMime) of
                true ->
                    {Width, Height, "-define webp:lossless=true"};
                false ->
                    {Width, Height, []}
            end
    end.
    

% Map css3 brightness/contrast to ImageMagick options.
%
% From: https://epsiloncool.ru/programmirovanie/php/yarkost-i-kontrastnost-css3-na-storone-servera
% // Example CSS3 values of brightness and contrast
% $b = 1.5;
% $c = 1.2;
%
% // Calculate level values
% $z1 = ($c - 1) / (2 * $b * $c);
% $z2 = ($c + 1) / (2 * $b * $c);
%
% // Doing a conversion using ImageMagick
% exec('convert src.jpg -level '.($z1 * 100).'%,'.($z2 * 100).'% dest.jpg');
brightness_contrast(0, 0) ->
    "";
brightness_contrast(B, C) when B >= -100, C >= -100 ->
    B1 = (B + 100.0) / 100.0 + 0.01,
    C1 = (C + 100.0) / 100.0 + 0.01,
    Z1 = (C1 - 1.0) / (2.0 * B1 * C1),
    Z2 = (C1 + 1.0) / (2.0 * B1 * C1),
    "-level "
        ++ float_to_list(Z1 * 100.0) ++ "%,"
        ++ float_to_list(Z2 * 100.0) ++ "%".


%% @doc Map the percentage-crop to ImageMagick commands and calculate the new width/height
cropp(Width, Height, [ ]) ->
    {Width, Height, []};
cropp(Width, Height, [ CL ]) ->
    cropp(Width, Height, [ CL, 0.0 ]);
cropp(Width, Height, [ CL, CR ]) ->
    cropp(Width, Height, [ CL, CR, 0.0 ]);
cropp(Width, Height, [ CL, CR, CT ]) ->
    cropp(Width, Height, [ CL, CR, CT, 0.0 ]);
cropp(Width, Height, [ CL, CR, CT, CB ]) ->
    CL1 = erlang:max(0.0, z_convert:to_float(CL)),
    CR1 = erlang:max(0.0, z_convert:to_float(CR)),
    CT1 = erlang:max(0.0, z_convert:to_float(CT)),
    CB1 = erlang:max(0.0, z_convert:to_float(CB)),
    CropH = erlang:max(0.0, 100.0 - (CL1 + CR1)),
    CropV = erlang:max(0.0, 100.0 - (CT1 + CB1)),
    W1 = round( (CropH / 100.0) * Width ),
    H1 = round( (CropV / 100.0) * Height ),
    Left = erlang:trunc(Width * CL1 / 100.0),
    Top = erlang:trunc(Height * CT1 / 100.0),
    Crop = lists:flatten([
        io_lib:format("+repage -crop ~px~p+~p+~p +repage",
            [ W1, H1, Left, Top ])
    ]),
    {W1, H1, Left, Top, Crop}.


%% @doc Split the filters into (automatic) crop and image manipulation filters.
-spec fetch_crop( proplists:proplist() ) -> { CropFs :: proplists:proplist(), OtherFs :: proplists:proplist() }.
fetch_crop(Filters) ->
    {Crop,OtherFilters} = lists:partition(
                                fun (crop) -> true;
                                    ({crop,_}) -> true;
                                    (_) -> false
                                end, Filters),
    CropPar = case Crop of
        [ {crop, false} | _ ] -> none;
        [ {crop, undefined} | _ ] -> none;
        [ {crop, <<>>} | _ ] -> none;
        [ {crop, ""} | _ ] -> none;
        [ {crop, none} | _ ] -> none;
        [ {crop, true} | _ ] -> center;        % default crop = center
        [ {crop, Gravity} | _ ] -> Gravity;    % center or one of the wind directions
        [ crop | _ ] -> center;
        _ -> none
    end,
    {CropPar, OtherFilters}.


calc_size(#{ req_width := 0 } = S) ->
    calc_size(S#{ req_width => 1 });
calc_size(#{ req_height := 0 } = S) ->
    calc_size(S#{ req_height => 1 });
calc_size(#{ image_height := 0 } = S) ->
    calc_size(S#{ image_height => 1 });
calc_size(#{ image_width := 0 } = S) ->
    calc_size(S#{ image_width => 1 });

calc_size(#{ image_height := H, image_width := W, orientation := Orientation } = S) when Orientation >= 5 ->
    calc_size(S#{ orientation => 1, image_height => W, image_width => H });

calc_size(#{ filters := [ {rotate, Rotation} | Fs ], image_width := W, image_height := H, crop := Crop } = S)
    when Rotation =:= 90;
         Rotation =:= 270;
         Rotation =:= -90;
         Rotation =:= -270 ->
    Crop1 = rotate_crop(W, H, Rotation, Crop),
    calc_size(S#{ image_width => H, image_height => W, filters => Fs, crop => Crop1 });
calc_size(#{ filters := [ {rotate, Rotation} | Fs ], image_width := W, image_height := H, crop := Crop } = S) ->
    Crop1 = rotate_crop(W, H, Rotation, Crop),
    calc_size(S#{ filters => Fs, crop => Crop1 });

calc_size(#{ filters := [ {cropp, Cropp} | Fs ], image_width := IW, image_height := IH, crop := Crop } = S) ->
    {W1, H1, Left, Top, _} = cropp(IW, IH, Cropp),
    Crop1 = crop_crop(Left, Top, Crop),
    calc_size(S#{ image_width => W1, image_height => H1, filters => Fs, crop => Crop1 });
calc_size(#{ filters := [ _ | Fs ] } = S) ->
    calc_size(S#{ filters => Fs });

calc_size(#{ req_height := undefined, req_width := undefined } = S) ->
    {maps:get(image_width, S), maps:get(image_height, S), none};

calc_size(#{ crop := Crop, req_height := undefined, req_width := W } = S) when Crop =/= none ->
    calc_size(S#{ req_height => W });
calc_size(#{ crop := Crop, req_height := H, req_width := undefined } = S) when Crop =/= none ->
    calc_size(S#{ req_width => H });

calc_size(#{ req_width := undefined, req_height := H, image_height := IH, crop := none, orientation := 1, is_upscale := false } = S)
    when IH < H ->
    % Image will be extented
    {maps:get(image_width, S), H, none};

calc_size(#{ req_width := undefined, req_height := H, image_width := IW, image_height := IH } = S) ->
    W = round((IW / IH) * H),
    calc_size(S#{ req_width := W });

calc_size(#{ req_height := undefined, req_width := W, image_width := IW, orientation := 1, is_upscale := false } = S)
    when IW < W ->
    % Image will be extented
    {W, maps:get(image_height, S), none};

calc_size(#{ req_height := undefined, req_width := W, image_width := IW, image_height := IH } = S) ->
    H = round((IH / IW) * W),
    calc_size(S#{ req_height := H });

calc_size(#{ req_width := W, req_height := H, image_width := IW, image_height := IH, crop := Crop, is_upscale := false })
    when Crop =/= none, W > IW, H > IH ->
    {W, H, none};

calc_size(#{
        req_width := Width,
        req_height := Height,
        image_width := ImageWidth,
        image_height := ImageHeight,
        crop := CropPar
    }) ->
    ImageAspect = ImageWidth / ImageHeight,
    Aspect      = Width / Height,
    case CropPar of
        none ->
            case Aspect > ImageAspect of
                true  -> {s_ceil(ImageAspect * Height), Height, none};
                false -> {Width, s_ceil(Width / ImageAspect), none}
            end;
        _ ->
            %% If we are doing a crop then we have to calculate the
            %% maximum inner bounding box, and not the maximum outer
            %% bounding box for the image
            {W,H} = case Aspect > ImageAspect of
                % width is the larger one
                true  -> {Width, Width / ImageAspect};

                % height is the larger one
                false -> {ImageAspect * Height, Height}
            end,

            Scale = ImageWidth / W,

            CropL = case CropPar of
                X when X == north_west; X == west; X == south_west -> 0;
                X when X == north_east; X == east; X == south_east -> s_ceil(W - Width);
                [X,_] when is_integer(X) -> s_ceil(erlang:max(0, erlang:min(W-Width, X / Scale - Width/2)));
                _ -> s_ceil((W - Width) / 2)
            end,

            CropT = case CropPar of
                Y when Y == north_west; Y == north; Y == north_east -> 0;
                Y when Y == south_west; Y == south; Y == south_east -> s_ceil(H - Height);
                [_,Y] when is_integer(Y) -> s_ceil(erlang:max(0, erlang:min(H-Height, Y / Scale - Height/2)));
                _ -> s_ceil((H - Height) / 2)
            end,

            %% @todo Prevent scaleup of the image, but preserve the result size
            %% The crop is relative to the original image
            {s_ceil(W), s_ceil(H), {CropL, CropT, Width, Height}}
    end.


rotate_crop(_W, _H, A, [ X, Y ]) when A =:= 90; A =:= -270 ->
    [ Y, X ];
rotate_crop(W, _H, A, [ X, Y ]) when A =:= -90; A =:= 270 ->
    [ Y, W - X ];
rotate_crop(W, H, A, [ X, Y ]) when A =:= 180; A =:= -180 ->
    [ H - Y, W - X ];
rotate_crop(_W, _H, _A, Crop) ->
    Crop.

crop_crop(Left, Top, [ X, Y ]) ->
    [ X - Left, Y - Top ];
crop_crop(_Left, _Top, Crop) ->
    Crop.


%% @doc Map the list of known filters and known args to atoms.  Used when mapping preview urls back to filter args.
-spec string2filter( string(), string() ) -> {ok, tuple()|atom()} | {error, term()}.
string2filter("crop", "none") ->
    {ok, {crop,none}};
string2filter("crop", []) ->
    {ok, {crop,center}};
string2filter("crop", Where) ->
    Dir = case Where of
            "north"      -> north;
            "north_east" -> north_east;
            "east"       -> east;
            "south_east" -> south_east;
            "south"      -> south;
            "south_west" -> south_west;
            "west"       -> west;
            "north_west" -> north_west;
            "center"     -> center;
            [C|_] = CropLT when C =:= $+ orelse C =:= $- ->
                {match, [[CropL], [CropT]]} = re:run(CropLT, "[+-][0-9]+", [global, {capture, first, list}]),
                [list_to_integer(CropL), list_to_integer(CropT)]
          end,
    {ok, {crop,Dir}};
string2filter("grey",[]) ->
    {ok, grey};
string2filter("gray",[]) ->
    {ok, grey};
string2filter("mono",[]) ->
    {ok, mono};
string2filter("flip",[]) ->
    {ok, flip};
string2filter("flop",[]) ->
    {ok, flop};
string2filter("extent",[]) ->
    {ok, extent};
string2filter("upscale",[]) ->
    {ok, upscale};
string2filter("blur",[]) ->
    {ok, blur};
string2filter("blur",Arg) ->
    {ok, {blur,Arg}};
string2filter("quality", Arg) ->
    {ok, {quality, list_to_integer(Arg)}};
string2filter("background", Arg) ->
    {ok, {background,Arg}};
string2filter("lossless", _) ->
    {ok, lossless};
string2filter("removebg", []) ->
    {ok, {removebg, 5}};
string2filter("removebg", Arg) ->
    {ok, {removebg, list_to_integer(Arg)}};
string2filter("mediaclass", Arg) ->
    [MediaClass|Checksum] = string:tokens(Arg, "."),
    {ok, {mediaclass, {MediaClass, iolist_to_binary(Checksum)}}};
string2filter("srcset", Arg) ->
    {ok, {srcset, Arg}};
string2filter("rotate3d", Arg) ->
    [ Roll, Tilt, Pan ] = case binary:split(z_convert:to_binary(Arg), <<",">>, [ global ]) of
        [ R, T, P ] -> [ binary_to_integer(R), binary_to_integer(T), binary_to_integer(P) ];
        [ R, T ] -> [ binary_to_integer(R), binary_to_integer(T), 0 ];
        [ R ] -> [ binary_to_integer(R), 0, 0 ]
    end,
    {ok, {rotate3d, [ Roll, Tilt, Pan ]}};
string2filter("cropp", Arg) ->
    L = binary:split(z_convert:to_binary(Arg), <<",">>, [ global ]),
    L1 = [ z_convert:to_float(V) || V <- L ],
    {ok, {cropp, L1}};
string2filter("brightness", Arg) ->
    {ok, {brightness, list_to_integer(Arg)}};
string2filter("contrast", Arg) ->
    {ok, {contrast, list_to_integer(Arg)}};
string2filter("rotate", Arg) ->
    {ok, {rotate, list_to_integer(Arg)}};
string2filter(_Filter, _Arg) ->
    {error, unknown_filter}.

% simple ceil for positive numbers
s_ceil(A)  -> round(A + 0.499999).
%floor(A) -> round(A - 0.499999).

ensure_integer(A) ->
    integer_to_list(list_to_integer(A)).
