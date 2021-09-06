%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2020 Marc Worrell
%% @doc Fetch information about a video file using ffmpeg.

%% Copyright 2014-2020 Marc Worrell
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

-module(z_video_info).

-export([
    info/1
]).

-define(FFPROBE_CMDLINE, "ffprobe -loglevel quiet -show_format -show_streams -print_format json ").

-spec info( file:filename_all() ) -> map().
info(Path) ->
    Cmdline = case z_config:get(ffprobe_cmdline) of
        undefined -> ?FFPROBE_CMDLINE;
        <<>> -> ?FFPROBE_CMDLINE;
        "" -> ?FFPROBE_CMDLINE;
        CmdlineCfg -> z_convert:to_list(CmdlineCfg)
    end,
    FfprobeCmd = lists:flatten([
           Cmdline, " ", z_filelib:os_filename(Path)
       ]),
    lager:debug("Video info: ~p", [FfprobeCmd]),
    JSONText = unicode:characters_to_binary(os:cmd(FfprobeCmd)),
    try
        Ps = decode_json(JSONText),
        {Width, Height, Orientation} = fetch_size(Ps),
        Info = #{
            <<"duration">> => fetch_duration(Ps),
            <<"bit_rate">> => fetch_bit_rate(Ps),
            <<"tags">> => fetch_tags(Ps),
            <<"width">> => Width,
            <<"height">> => Height,
            <<"orientation">> => Orientation,
            <<"size">> => fetch_datasize(Ps),
            <<"audio_codec">> => fetch_codec(Ps, <<"audio">>),
            <<"video_codec">> => fetch_codec(Ps, <<"video">>)
        },
        maps:filter(
            fun(_K, V) -> V =/= undefined end,
            Info)
    catch
        error:E ->
            lager:warning("Unexpected ffprobe return (~p) ~p", [E, JSONText]),
            #{}
    end.

decode_json(JSONText) ->
    z_json:decode(JSONText).

fetch_duration(#{<<"format">> := #{<<"duration">> := Duration}}) ->
    round(z_convert:to_float(Duration));
fetch_duration(_) ->
    0.

fetch_bit_rate(#{<<"format">> := #{<<"bit_rate">> := BitRate}}) ->
    round(z_convert:to_float(BitRate));
fetch_bit_rate(_) ->
    undefined.

fetch_tags(#{ <<"format">> := #{ <<"tags">> := Tags }}) when is_map(Tags) ->
    maps:filter(fun is_tag_ok/2, Tags);
fetch_tags(_) ->
    undefined.

is_tag_ok(<<"iTunSMPB">>, _) -> false;
is_tag_ok(<<"iTunNORM">>, _) -> false;
is_tag_ok(_, _) -> true.

fetch_size(#{ <<"streams">> := Streams }) ->
    [ Video | _ ] = lists:dropwhile(
        fun( #{<<"codec_type">> := CodecType} ) ->
           CodecType =/= <<"video">>
        end,
        Streams),
    #{
        <<"width">> := Width,
        <<"height">> := Height
    } = Video,
    Tags = maps:get(<<"tags">>, Video, undefined),
    Orientation = orientation(Tags),
    case Orientation of
        6 -> {Height, Width, Orientation};
        8 -> {Height, Width, Orientation};
        _ -> {Width, Height, Orientation}
    end;
fetch_size(#{ <<"width">> := Width, <<"heigth">> := Height }) ->
    {z_convert:to_integer(Height), z_convert:to_integer(Width), 1}.


orientation(#{<<"rotate">> := Angle}) ->
    try
        case z_convert:to_integer(Angle) of
            90 -> 6;
            180 -> 3;
            270 -> 8;
            _ -> 1
        end
    catch
        _:_ ->
            1
    end;
orientation(_) ->
    1.

fetch_datasize(#{ <<"format">> := #{ <<"size">> := Size }} ) ->
    z_convert:to_integer(Size);
fetch_datasize(_) ->
    0.

fetch_codec(#{<<"streams">> := Streams}, Type) ->
    fetch_codec_1(Streams, Type);
fetch_codec(_, _) ->
    undefined.

fetch_codec_1([ #{ <<"codec_type">> := CT } = S | _ ], Type) when CT =:= Type ->
    maps:get(<<"codec_name">>, S, undefined);
fetch_codec_1([ _ | Streams ], Type) ->
    fetch_codec_1(Streams, Type);
fetch_codec_1([], _Type) ->
    undefined.
