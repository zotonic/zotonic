%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2012 Marc Worrell
%% @doc Make still previews of media, using image manipulation functions.  Resize, crop, gray, etc.
%% This uses the command line imagemagick tools for all image manipulation.
%% This code is adapted from PHP GD2 code, so the resize/crop could've been done more efficiently, but it works :-)

%% Copyright 2009-2012 Marc Worrell
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
    size/3,
    can_generate_preview/1,
    out_mime/2,
    string2filter/2,
    cmd_args/3,
    calc_size/7
]).

-define(MAX_WIDTH,  5000).
-define(MAX_HEIGHT, 5000).

-include_lib("zotonic.hrl").


%% @spec convert(InFile, OutFile, Filters, Context) -> ok | {error, Reason}
%% @doc Convert the Infile to an outfile with a still image using the filters.
convert(InFile, InFile, _, _Context) ->
    lager:error("convert will overwrite input file ~p", [InFile]),
    {error, will_overwrite_infile};
convert(InFile, OutFile, Filters, Context) ->
    case z_media_identify:identify(InFile, Context) of
        {ok, FileProps} ->
            {mime, Mime} = proplists:lookup(mime, FileProps),
            case can_generate_preview(Mime) of
                true ->
                    case z_media_class:expand_mediaclass_checksum(Filters) of
                        {ok, FiltersExpanded} ->
                            convert_1(InFile, OutFile, Mime, FileProps, FiltersExpanded);
                        {error, _} = Error ->
                            lager:warning("cannot expand mediaclass for ~p (~p)", [Filters, Error]),
                            Error
                    end;
                false ->
                    lager:error("cannot convert a ~p (~p)", [Mime, InFile]),
                    {error, mime_type}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

    convert_1(InFile, OutFile, Mime, FileProps, Filters) ->
        OutMime = z_media_identify:guess_mime(OutFile),
        {EndWidth, EndHeight, CmdArgs} = cmd_args(FileProps, Filters, OutMime),
        z_utils:assert(EndWidth  < ?MAX_WIDTH, image_too_wide),
        z_utils:assert(EndHeight < ?MAX_HEIGHT, image_too_high),
        file:delete(OutFile),
        ok = filelib:ensure_dir(OutFile),
        Cmd = lists:flatten([
            "convert ",
            z_utils:os_filename(InFile++infile_suffix(Mime)), " ",
            lists:flatten(z_utils:combine(32, CmdArgs)), " ",
            z_utils:os_filename(OutFile)
        ]),
        lager:debug("~p", [Cmd]),
        Result = z_media_preview_server:exec(Cmd, OutFile),
        case filelib:is_regular(OutFile) of
            true ->
                ok;
            false -> 
                lager:error("convert cmd ~p failed, result ~p", [Cmd, Result]),
                {error, convert_error}
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
                {orientation, Orientation} = proplists:lookup(orientation, FileProps),
                
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
    {orientation, Orientation} = proplists:lookup(orientation, FileProps),
    ReqWidth   = proplists:get_value(width, Filters),
    ReqHeight  = proplists:get_value(height, Filters),
    {CropPar,Filters1} = fetch_crop(Filters),
    {ResizeWidth,ResizeHeight,CropArgs} = calc_size(ReqWidth, ReqHeight, ImageWidth, ImageHeight, CropPar, Orientation, is_enabled(upscale, Filters)),
    Filters2   = [  {make_image, Mime},
                    {correct_orientation, Orientation},
                    {resize, ResizeWidth, ResizeHeight, is_enabled(upscale, Filters)}, 
                    {crop, CropArgs},
                    {colorspace, "RGB"} | Filters1],
    Filters3 = case {CropArgs,is_enabled(extent, Filters)} of
                    {none,true} -> Filters2 ++ [{extent, ReqWidth, ReqHeight}];
                    _ -> Filters2
                end,
    Filters4 = case is_blurred(Filters3) of
                    true ->  Filters3;
                    false -> case Mime of
                                 "image/gif" -> Filters3;
                                 "image/png" -> Filters3;
                                 _ -> Filters3 ++ [sharpen_small]
                             end
               end,
    Filters5 = case proplists:get_value(background, Filters4) of
                    undefined -> default_background(OutMime) ++ Filters4;
                    _ -> Filters4
               end,

    {EndWidth,EndHeight,Args} = lists:foldl(fun (Filter, {W,H,Acc}) -> 
                                                {NewW,NewH,Arg} = filter2arg(Filter, W, H, Filters5),
                                                {NewW,NewH,[Arg|Acc]} 
                                            end,
                                            {ImageWidth,ImageHeight,[]},
                                            Filters5),
    {EndWidth, EndHeight, lists:reverse(Args)}.


default_background("image/gif") -> [coalesce];
default_background("image/png") -> [coalesce];
default_background(_) -> [{background,"white"}, {layers,"flatten"}].

%% @doc Check if there is a blurring filter that prevents us from sharpening the resulting image
is_blurred([]) -> false;
is_blurred([blur|_]) -> true;
is_blurred([{blur, _}|_]) -> true;
is_blurred([_|Rest]) -> is_blurred(Rest).


is_enabled(_F, []) -> false;
is_enabled(F, [F|_]) -> true;
is_enabled(F, [{F, Val}|_]) -> z_convert:to_bool(Val);
is_enabled(F, [_|R]) -> is_enabled(F, R).


%% @spec out_mime(Mime, Options) -> {Mime, Extension}
%% @doc Return the preferred mime type of the image generated by resizing an image of a certain type and size.
out_mime("image/gif", _) ->
    %% gif is gif, daar kan je gif op innemen
    {"image/gif", ".gif"};
out_mime(_Mime, Options) ->
    case lists:member("lossless", Options) orelse proplists:is_defined(lossless, Options) of
        false -> {"image/jpeg", ".jpg"};
        true  -> {"image/png", ".png"}
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
filter2arg({width, _}, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({height, _}, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({resize, Width, Height, _}, Width, Height, _AllFilters) ->
    {Width, Height, []};
filter2arg({resize, EndWidth, EndHeight, false}, Width, Height, _AllFilters) 
  when Width =< EndWidth andalso Height =< EndHeight ->
    % Prevent scaling up, perform an extent instead
    GArg = "-gravity West",
    EArg = ["-extent ", integer_to_list(EndWidth),$x,integer_to_list(EndHeight)],
    % Still thumbnail to remove extra info from the image
    RArg = ["-thumbnail ", z_utils:os_escape([integer_to_list(EndWidth),$x,integer_to_list(EndHeight),$!])],
    {EndWidth, EndHeight, [GArg, 32, EArg, 32, RArg]};
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
    RArg = "+repage",
    {CropWidth, CropHeight, [GArg,32,CArg,32,RArg]};
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
                  [{crop,None}] when None == false; None == undefined; None == ""; None == <<>> -> none;
                  [{crop,Gravity}] -> Gravity; % center or one of the wind directions
                  _ -> none
              end,
    {CropPar,OtherFilters}.


%%@doc Calculate the size of the resulting image, depends on the crop and the original image size
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
                true  -> {ceil(ImageAspect * Height), Height, none};
                false -> {Width, ceil(Width / ImageAspect), none}
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

            
        CropL = case CropPar of
        X when X == north_west; X == west; X == south_west -> 0;
        X when X == north_east; X == east; X == south_east -> ceil(W - Width);
        [X,_] when is_integer(X) -> X;
        _ -> ceil((W - Width) / 2)
        end,

            CropT = case CropPar of
        Y when Y == north_west; Y == north; Y == north_east -> 0;
        Y when Y == south_west; Y == south; Y == south_east -> ceil(H - Height);
        [_,Y] when is_integer(Y) -> Y;
        _ -> ceil((H - Height) / 2)
        end,

        %% @todo Prevent scaleup of the image, but preserve the result size
        %% The crop is relative to the original image
        {ceil(W), ceil(H), {CropL, CropT, Width, Height}}
    end.


%% @spec string2filter(Filter, Arg) -> FilterTuple
%% @doc Map the list of known filters and known args to atoms.  Used when mapping preview urls back to filter args.
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
        [C|_] = CropLT when C == $+ orelse C == $- ->
        {match, [[CropL], [CropT]]} = re:run(CropLT, "[+-][0-9]+", [global, {capture, first, list}]),
        [list_to_integer(CropL), list_to_integer(CropT)]
          end,
    {crop,Dir};
string2filter("grey",[]) ->
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
string2filter("lossless", []) ->
    lossless;
string2filter("removebg", []) ->
    {removebg, 5};
string2filter("removebg", Arg) ->
    {removebg, list_to_integer(Arg)};
string2filter("mediaclass", Arg) ->
    [MediaClass|Checksum] = string:tokens(Arg, "."),
    {mediaclass, {MediaClass, iolist_to_binary(Checksum)}}.


% simple ceil for positive numbers
ceil(A)  -> round(A + 0.499999).
%floor(A) -> round(A - 0.499999).

ensure_integer(A) ->
    integer_to_list(list_to_integer(A)).
