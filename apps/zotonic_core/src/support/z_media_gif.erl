%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2024 Marc Worrell
%% @doc Check if a GIF image is animated.
%% @end

%% Copyright 2024 Marc Worrell, Konstantin Nikiforov
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

-module(z_media_gif).

-export([
    is_animated_file/1,
    is_animated/1,
    frame_count_file/1,
    frame_count/1
]).


-include_lib("../../include/zotonic.hrl").

%% @doc Check if a GIF file is animated.
-spec is_animated_file(Filename) -> boolean() when
    Filename :: file:filename_all().
is_animated_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Data} -> is_animated(Data);
        {error, _} -> false
    end.

%% @doc Return the number of frames in a GIF. 0 if the GIF has errros.
-spec frame_count_file(Filename) -> non_neg_integer() when
    Filename :: file:filename_all().
frame_count_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Data} -> frame_count(Data);
        {error, _} -> 0
    end.

%% @doc Check if a GIF binary is animated.
-spec is_animated(Data) -> boolean() when
    Data :: binary().
is_animated(<<"GIF87a", Data/binary>>) ->
    frame_count(Data) > 1;
is_animated(<<"GIF89a", Data/binary>>) ->
    frame_count(Data) > 1;
is_animated(Data) when is_binary(Data) ->
    false.

%% @doc Return the number of frames in a GIF. Returns 0 for GIFs with
%% an error.
-spec frame_count(Data) -> non_neg_integer() when
    Data :: binary().
frame_count(<<"GIF87a", Data/binary>>) ->
    frame_count_1(Data);
frame_count(<<"GIF89a", Data/binary>>) ->
    frame_count_1(Data);
frame_count(Data) when is_binary(Data) ->
    0.

frame_count_1(<<
        _Width:16/little-unsigned-integer,
        _Height:16/little-unsigned-integer,
        Map:1, _Cr:3, _Sort:1, Pix:3,
        _Background:8,
        _AspectRatio:8,
        Rest/binary>>) ->
    R1 = skip_pallette(Map, Pix+1, Rest),
    read_data(R1, 0);
frame_count_1(_) ->
    1.

read_data(<<16#21, 16#f9,
            4, % Block size
            _:3, _DisposalMethod:3, _UserInput:1, _Transparent:1,
            _DelayTime:16/unsigned-little,
            _TransparentColor:8,
            0, % Terminator
            R/binary>>, Frames) ->
    % ctl block
    read_data(R, Frames + 1);
read_data(<<16#21, _Type, R/binary>>, Frames) ->
    % Other Extensions
    R1 = skip_blocks(R),
    read_data(R1, Frames);
read_data(<<16#2c,
            _Left:16/little, _Top:16/little,
            _Width:16/little, _Height:16/little,
            Map:1, _Interlaced:1, _Sort:1, _:2, Pix:3,
            Rest/binary>>, Frames) ->
    % Image data
    R1 = skip_pallette(Map, Pix+1, Rest),
    R2 = skip_pixels(R1),
    read_data(R2, Frames);
read_data(<<16#3b, _/binary>>, Frames) ->
    % Trailer
    Frames;
read_data(<<_, _Data/binary>>, Frames) ->
    % Error in GIF format
    Frames.

skip_pallette(0, _Pixel, Data) ->
    Data;
skip_pallette(1, Pixel, Data) ->
    Sz = (1 bsl Pixel),
    case Data of
        <<_:(Sz*3)/binary, R/binary>> -> R;
        _ -> error
    end.

skip_pixels(<<_LZWCodeSize, Data/binary>>) ->
    skip_blocks(Data);
skip_pixels(_) ->
    error.

skip_blocks(<<0, Data/binary>>) ->
    Data;
skip_blocks(<<Size, Data/binary>>) ->
    case Data of
        <<_:Size/binary, R/binary>> -> skip_blocks(R);
        _ -> error
    end.
