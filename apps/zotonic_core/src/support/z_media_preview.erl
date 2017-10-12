%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%% @doc Make still previews of media, using image manipulation functions.  Resize, crop, grey, etc.
%% This uses the command line imagemagick tools for all image manipulation.
%% This code is adapted from PHP GD2 code, so the resize/crop could've been done more efficiently, but it works :-)

%% Copyright 2009-2017 Marc Worrell
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

-compile([{parse_transform, lager_transform}]).

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
    calc_size/7
]).

-define(MAX_PIXSIZE,  20000).

% Low and max image size (in total pixels) for quality 99 and 55.
% A small thumbnail needs less compression to keep image quality.
-define(PIX_Q99, 1000).
-define(PIX_Q50, 250000).

-include_lib("zotonic.hrl").


%% @doc Convert the Infile to an outfile with a still image using the filters.
-spec convert(file:filename_all(), file:filename_all(), list(), #context{}) -> ok | {error, term()}.
convert(InFile, InFile, _, _Context) ->
    lager:error("Image convert will overwrite input file ~p", [InFile]),
    {error, will_overwrite_infile};
convert(InFile, OutFile, Filters, Context) ->
    convert(InFile, InFile, OutFile, Filters, Context).


convert(InFile, MediumFilename, OutFile, Filters, Context) ->
    case z_media_identify:identify(InFile, MediumFilename, MediumFilename, Context) of
        {ok, FileProps} ->
            {mime, Mime} = proplists:lookup(mime, FileProps),
            case can_generate_preview(Mime) of
                true ->
                    case z_mediaclass:expand_mediaclass_checksum(Filters) of
                        {ok, FiltersExpanded} ->
                            convert_1(os:find_executable("convert"), InFile, OutFile, Mime, FileProps, FiltersExpanded);
                        {error, _} = Error ->
                            lager:warning("cannot expand mediaclass for ~p (~p)", [Filters, Error]),
                            Error
                    end;
                false ->
                    lager:info("cannot convert a ~p (~p)", [Mime, InFile]),
                    {error, mime_type}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

convert_1(false, _InFile, _OutFile, _Mime, _FileProps, _Filters) ->
    lager:error("Install ImageMagick 'convert' to generate previews of images."),
    {error, convert_missing};
convert_1(ConvertCmd, InFile, OutFile, Mime, FileProps, Filters) ->
    OutMime = z_media_identify:guess_mime(OutFile),
    case cmd_args(FileProps, Filters, OutMime) of
        {EndWidth, EndHeight, _CmdArgs} when EndWidth > ?MAX_PIXSIZE; EndHeight > ?MAX_PIXSIZE ->
            {error, image_too_big};
        {_, _, CmdArgs} ->
            convert_2(CmdArgs, ConvertCmd, InFile, OutFile, Mime, FileProps)
    end.

convert_2(CmdArgs, ConvertCmd, InFile, OutFile, Mime, FileProps) ->
    file:delete(OutFile),
    ok = z_filelib:ensure_dir(OutFile),
    Cmd = lists:flatten([
        z_utils:os_filename(ConvertCmd), " ",
        opt_density(FileProps),
        z_utils:os_filename(InFile++infile_suffix(Mime)), " ",
        lists:flatten(z_utils:combine(32, CmdArgs)), " ",
        z_utils:os_filename(OutFile)
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
        {error, _} = Error ->
            lager:error("convert cmd ~p failed, result ~p", [Cmd, Error]),
            Error
    end.

% We need to set a bigger density for PDF rendering, otherwise the resulting
% image is too small.
opt_density(Props) ->
    case proplists:get_value(mime, Props) of
        <<"application/pdf">> -> " -density 150x150 ";
        "application/pdf" -> " -density 150x150 ";
        _Mime -> ""
    end.

run_cmd(Cmd, OutFile) ->
    jobs:run(media_preview_jobs,
            fun() ->
                case filelib:is_regular(OutFile) of
                    true -> ok;
                    false -> once(Cmd, OutFile)
                end
            end).


once(Cmd, OutFile) ->
    MyPid = self(),
    Key = {n,l,Cmd},
    case gproc:reg_or_locate(Key) of
        {MyPid, _} ->
            lager:debug("Convert: ~p", [Cmd]),
            Result = os:cmd(Cmd),
            gproc:unreg(Key),
            case filelib:is_regular(OutFile) of
                true ->
                    ok;
                false ->
                    lager:error("convert cmd ~p failed, result ~p", [Cmd, Result]),
                    {error, convert_error}
            end;
        {_OtherPid, _} ->
            lager:debug("Waiting for parallel: ~p", [Cmd]),
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
size([{_Prop, _Value}|_] = Props, Filters, _Context) ->
    size_props(Props, Filters);
size(InFile, Filters, Context) ->
    case z_media_identify:identify(InFile, Context) of
        {ok, FileProps} ->
            size_props(FileProps, Filters);
        {error, Reason} ->
            {error, Reason}
    end.


    size_props(FileProps, Filters) ->
        {mime, Mime} = proplists:lookup(mime, FileProps),
        case can_generate_preview(Mime) of
            true ->
                {width, ImageWidth}   = proplists:lookup(width, FileProps),
                {height, ImageHeight} = proplists:lookup(height, FileProps),
                Orientation = proplists:get_value(orientation, FileProps, 1),

                ReqWidth   = z_convert:to_integer(proplists:get_value(width, Filters)),
                ReqHeight  = z_convert:to_integer(proplists:get_value(height, Filters)),
                {CropPar,_Filters1} = fetch_crop(Filters),
                {ResizeWidth,ResizeHeight,CropArgs} = calc_size(ReqWidth, ReqHeight, ImageWidth, ImageHeight, CropPar, Orientation, is_enabled(upscale, Filters)),
                case CropArgs of
                    none ->
                        case is_enabled(extent, Filters) of
                            true when is_integer(ReqWidth) andalso is_integer(ReqHeight) ->
                                {size, ReqWidth, ReqHeight, "image/jpeg"};
                            _ ->
                                {size, ResizeWidth, ResizeHeight, "image/jpeg"}
                        end;
                    {_CropL, _CropT, CropWidth, CropHeight} -> {size, CropWidth, CropHeight, "image/jpeg"}
                end;
            false ->
                {error, {no_preview_for_mimetype, Mime}}
        end.


%% @spec can_generate_preview(Mime) -> true | false
%% @doc Check if we can generate a preview image of the given mime type
can_generate_preview(B) when is_binary(B) -> can_generate_preview(binary_to_list(B));
can_generate_preview("image/" ++ _) -> true;
can_generate_preview("application/pdf") -> true;
can_generate_preview("application/postscript") -> true;
can_generate_preview(_Mime) -> false.


%% @doc Map filters to commandline options
cmd_args(FileProps, Filters, OutMime) ->
    {width, ImageWidth}   = proplists:lookup(width, FileProps),
    {height, ImageHeight} = proplists:lookup(height, FileProps),
    {mime, Mime0} = proplists:lookup(mime, FileProps),
    Mime = z_convert:to_list(Mime0),
    Orientation = proplists:get_value(orientation, FileProps, 1),
    ReqWidth   = proplists:get_value(width, Filters),
    ReqHeight  = proplists:get_value(height, Filters),
    {CropPar,Filters1} = fetch_crop(Filters),
    {ResizeWidth,ResizeHeight,CropArgs} = calc_size(ReqWidth, ReqHeight, ImageWidth, ImageHeight, CropPar, Orientation, is_enabled(upscale, Filters)),
    Filters2   = [  {make_image, Mime},
                    {correct_orientation, Orientation},
                    {resize, ResizeWidth, ResizeHeight, is_enabled(upscale, Filters)},
                    {crop, CropArgs},
                    {colorspace, z_config:get(default_colorspace, "sRGB")},
                    {density, 72} | Filters1],
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
    Filters7 = move_pre_post_filters(Filters6),
    {EndWidth,EndHeight,Args} = lists:foldl(fun (Filter, {W,H,Acc}) ->
                                                {NewW,NewH,Arg} = filter2arg(Filter, W, H, Filters6),
                                                {NewW,NewH,[Arg|Acc]}
                                            end,
                                            {ImageWidth,ImageHeight,[]},
                                            Filters7),
    {EndWidth, EndHeight, lists:reverse(Args)}.


default_background("image/gif") -> [coalesce];
default_background("image/png") -> [coalesce];
default_background(_) -> [{background,"white"}, {layers,"flatten"}].

%% @doc Check if there is a blurring filter that prevents us from sharpening the resulting image
is_blurred([]) -> false;
is_blurred([blur|_]) -> true;
is_blurred([{blur, _}|_]) -> true;
is_blurred([_|Rest]) -> is_blurred(Rest).

is_lossless("image/gif") -> true;
is_lossless("image/png") -> true;
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


%% @doc Determine the output mime type, after expanding optional mediaclass arguments.
out_mime(Mime, Options, Context) ->
    {ok, Options1} = z_mediaclass:expand_mediaclass(Options, Context),
    out_mime(Mime, Options1).

%% @spec out_mime(Mime, Options) -> {Mime, Extension}
%% @doc Return the preferred mime type of the image generated by resizing an image of a certain type and size.
out_mime(Mime, Options) ->
    out_mime1(get_lossless_value(Options), Mime).

out_mime1(_, "image/gif") -> {"image/gif", ".gif"};
out_mime1(false, _Mime) -> {"image/jpeg", ".jpg"};
out_mime1(true, _Mime) -> {"image/png", ".png"};
out_mime1(auto, "image/png") -> {"image/png", ".png"};
out_mime1(auto, _) -> {"image/jpeg", ".jpg"}.


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

%% @spec filter2arg(Filter, Width, Height, AllFilters) -> {NewWidth, NewHeight, Filter::string}
%% @doc Map filters to an ImageMagick argument
filter2arg({make_image, "application/pdf"}, Width, Height, _AllFilters) ->
    RArg = ["-resize ", integer_to_list(Width),$x,integer_to_list(Height)],
    {Width, Height, RArg};
filter2arg(coalesce, Width, Height, _AllFilters) ->
    {Width, Height, "-coalesce"};
filter2arg({make_image, _Mime}, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({correct_orientation, Orientation}, Width, Height, _AllFilters) ->
    case Orientation of
        2 -> {Width, Height, "-flip"};
        3 -> {Width, Height, "-rotate 180"};
        4 -> {Width, Height, "-flop"};
        5 -> {Width, Height, "-transpose"};
        6 -> {Width, Height, "-rotate 90"};
        7 -> {Width, Height, "-transverse"};
        8 -> {Width, Height, "-rotate 270"};
        _ -> {Width, Height, []}
    end;
filter2arg({background, Color}, Width, Height, _AllFilters) ->
    {Width, Height, ["-background ", $", z_utils:os_escape(Color), $"]};
filter2arg({layers, Method}, Width, Height, _AllFilters) ->
    {Width, Height, ["-layers ", $", z_utils:os_escape(Method), $"]};
filter2arg({colorspace, Colorspace}, Width, Height, _AllFilters) ->
    {Width, Height, ["-colorspace ", $", z_utils:os_escape(Colorspace), $"]};
filter2arg({density, DPI}, Width, Height, _AllFilters) when is_integer(DPI) ->
    {Width, Height, ["-set units PixelsPerInch -density ", integer_to_list(DPI)]};
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
    RArg = ["-thumbnail ", z_utils:os_escape([integer_to_list(EndWidth),$x,integer_to_list(EndHeight),$!])],
    {EndWidth, EndHeight, [EArg, 32, RArg]};
filter2arg({extent, EndWidth, EndHeight}, Width, Height, _AllFilters) when EndWidth == undefined orelse EndHeight == undefined ->
    {Width, Height, []};
filter2arg({extent, EndWidth, EndHeight}, Width, Height, _AllFilters) when Width /= EndWidth orelse Height /= EndHeight ->
    GArg = "-gravity Center",
    EArg = ["-extent ", integer_to_list(EndWidth),$x,integer_to_list(EndHeight)],
    {EndWidth, EndHeight, [GArg, 32, EArg]};
filter2arg({resize, EndWidth, EndHeight, _}, _Width, _Height, _AllFilters) ->
    GArg = "-gravity NorthWest",
    RArg = ["-thumbnail ", z_utils:os_escape([integer_to_list(EndWidth),$x,integer_to_list(EndHeight),$!])],
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
filter2arg({removebg, Fuzz}, Width, Height, AllFilters) ->
    Filter = case lists:member(lossless, AllFilters) of
                 true ->
                     %% PNG images get the alpha channel flood-filled to remove the background.
                     ["-matte -fill none -fuzz ", integer_to_list(Fuzz), "% ",
                      "-draw 'matte 0,0 floodfill' ",
                      "-draw 'matte 0,", integer_to_list(Height-1), " floodfill' ",
                      "-draw 'matte ", integer_to_list(Width-1), ",0 floodfill' ",
                      "-draw 'matte ", integer_to_list(Width-1), ",", integer_to_list(Height-1), " floodfill' "
                     ];
                 false ->
                     %% JPEG images get flood-filled with white to remove the background.
                     ["-matte -fill white -fuzz ", integer_to_list(Fuzz), "% ",
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
    {Width, Height, []}.


%% @spec fetch_crop(Filters) -> {Crop, Filters}
%% @doc Split the filters into size/crop and image manipulation filters.
fetch_crop(Filters) ->
    {Crop,OtherFilters} = lists:partition(
                                fun (crop) -> true;
                                    (F) when is_tuple(F) -> element(1,F) == 'crop';
                                    (_) -> false
                                end, Filters),
    CropPar = case Crop of
                  [{crop,None}|_] when None == false; None == undefined; None == ""; None == <<>> -> none;
                  [crop|_] -> center; % default crop = center
                  [{crop,Gravity}|_] -> Gravity; % center or one of the wind directions
                  _ -> none
              end,
    {CropPar,OtherFilters}.


%%@doc Calculate the size of the resulting image, depends on the crop and the original image size
calc_size(0, Height, ImageWidth, ImageHeight, CropPar, Orientation, IsUpscale) ->
    calc_size(1, Height, ImageWidth, ImageHeight, CropPar, Orientation, IsUpscale);
calc_size(Width, 0, ImageWidth, ImageHeight, CropPar, Orientation, IsUpscale) ->
    calc_size(Width, 1, ImageWidth, ImageHeight, CropPar, Orientation, IsUpscale);
calc_size(Width, Height, 0, ImageHeight, CropPar, Orientation, IsUpscale) ->
    calc_size(Width, Height, 1, ImageHeight, CropPar, Orientation, IsUpscale);
calc_size(Width, Height, ImageWidth, 0, CropPar, Orientation, IsUpscale) ->
    calc_size(Width, Height, ImageWidth, 1, CropPar, Orientation, IsUpscale);

calc_size(Width, Height, ImageWidth, ImageHeight, CropPar, Orientation, IsUpscale) when Orientation >= 5 ->
    calc_size(Width, Height, ImageHeight, ImageWidth, CropPar, 1, IsUpscale);

calc_size(undefined, undefined, ImageWidth, ImageHeight, _CropPar, _Orientation, _IsUpscale) ->
    {ImageWidth, ImageHeight, none};

calc_size(Width, undefined, ImageWidth, ImageHeight, CropPar, Orientation, IsUpscale) when CropPar /= none ->
    calc_size(Width, Width, ImageWidth, ImageHeight, CropPar, Orientation, IsUpscale);

calc_size(undefined, Height, ImageWidth, ImageHeight, CropPar, Orientation, IsUpscale) when CropPar /= none ->
    calc_size(Height, Height, ImageWidth, ImageHeight, CropPar, Orientation, IsUpscale);

calc_size(undefined, Height, ImageWidth, ImageHeight, none, 1, false) when ImageHeight < Height ->
    % Image will be extented
    {ImageWidth, Height, none};

calc_size(undefined, Height, ImageWidth, ImageHeight, CropPar, Orientation, IsUpscale) ->
    Width = round((ImageWidth / ImageHeight) * Height),
    calc_size(Width, Height, ImageWidth, ImageHeight, CropPar, Orientation, IsUpscale);

calc_size(Width, undefined, ImageWidth, ImageHeight, none, 1, false) when ImageWidth < Width ->
    % Image will be extented
    {Width, ImageHeight, none};

calc_size(Width, undefined, ImageWidth, ImageHeight, CropPar, Orientation, IsUpscale) ->
    Height = round((ImageHeight / ImageWidth) * Width),
    calc_size(Width, Height, ImageWidth, ImageHeight, CropPar, Orientation, IsUpscale);

calc_size(Width, Height, ImageWidth, ImageHeight, CropPar, _Orientation, false)
    when CropPar /= none, Width > ImageWidth, Height > ImageHeight ->
    {Width, Height, none};

calc_size(Width, Height, ImageWidth, ImageHeight, CropPar, _Orientation, _IsUpscale) ->
    ImageAspect = ImageWidth / ImageHeight,
    Aspect      = Width / Height,
    case CropPar of
        none ->
            case Aspect > ImageAspect of
                true  -> {s_ceil(ImageAspect * Height), Height, none};
                false -> {Width, s_ceil(Width / ImageAspect), none}
            end;
        _ ->
        %% When we are doing a crop then we have to calculate the
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


%% @spec string2filter(Filter, Arg) -> FilterTuple
%% @doc Map the list of known filters and known args to atoms.  Used when mapping preview urls back to filter args.
string2filter("crop", "none") ->
    {crop,none};
string2filter("crop", []) ->
    {crop,center};
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
    {crop,Dir};
string2filter("grey",[]) ->
    grey;
string2filter("gray",[]) ->
    grey;
string2filter("mono",[]) ->
    mono;
string2filter("flip",[]) ->
    flip;
string2filter("flop",[]) ->
    flop;
string2filter("extent",[]) ->
    extent;
string2filter("upscale",[]) ->
    upscale;
string2filter("blur",[]) ->
    blur;
string2filter("blur",Arg) ->
    {blur,Arg};
string2filter("quality", Arg) ->
    {quality, list_to_integer(Arg)};
string2filter("background", Arg) ->
    {background,Arg};
string2filter("lossless", _) ->
    lossless;
string2filter("removebg", []) ->
    {removebg, 5};
string2filter("removebg", Arg) ->
    {removebg, list_to_integer(Arg)};
string2filter("mediaclass", Arg) ->
    [MediaClass|Checksum] = string:tokens(Arg, "."),
    {mediaclass, {MediaClass, iolist_to_binary(Checksum)}}.


% simple ceil for positive numbers
s_ceil(A)  -> round(A + 0.499999).
%floor(A) -> round(A - 0.499999).

ensure_integer(A) ->
    integer_to_list(list_to_integer(A)).
