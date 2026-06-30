%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2023 Marc Worrell
%% @doc Fetch information about a video file using ffmpeg.
%% @end

%% Copyright 2014-2023 Marc Worrell
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

-include_lib("kernel/include/logger.hrl").

% Default timeout for video info - 10 minutes
-define(FFMPEG_TIMEOUT, 10*60*1000).

-define(FFPROBE_CMDLINE, "ffprobe -loglevel quiet -show_format -show_streams -print_format json ").

-spec info( file:filename_all() ) -> map().
info(Path) ->
    Cmdline = case z_config:get(ffprobe_cmdline) of
        undefined -> ?FFPROBE_CMDLINE;
        <<>> -> ?FFPROBE_CMDLINE;
        "" -> ?FFPROBE_CMDLINE;
        CmdlineCfg -> CmdlineCfg
    end,
    FfprobeCmd = unicode:characters_to_binary([
           Cmdline, " ", z_filelib:os_filename(Path)
       ]),
    ?LOG_DEBUG(#{
        in => zotonic_mod_video,
        text => <<"Video info">>,
        command => FfprobeCmd
    }),
    case z_exec:run(FfprobeCmd, #{ timeout => ?FFMPEG_TIMEOUT }) of
        {ok, JSONText} ->
            try
                Ps = decode_json(JSONText),
                {Width, Height, Orientation} = fetch_size(Ps),
                Info = #{
                    <<"duration">> => fetch_duration(Ps),
                    <<"bit_rate">> => fetch_bit_rate(Ps),
                    <<"tags">> => fetch_tags(Ps),
                    <<"format_name">> => fetch_format_name(Ps),
                    <<"format_long_name">> => fetch_format_long_name(Ps),
                    <<"width">> => Width,
                    <<"height">> => Height,
                    <<"orientation">> => Orientation,
                    <<"size">> => fetch_datasize(Ps),
                    <<"audio_codec">> => fetch_codec(Ps, <<"audio">>),
                    <<"audio_bit_rate">> => fetch_stream_bit_rate(Ps, <<"audio">>),
                    <<"audio_channels">> => fetch_audio_channels(Ps),
                    <<"audio_channel_layout">> => fetch_audio_channel_layout(Ps),
                    <<"audio_sample_rate">> => fetch_audio_sample_rate(Ps),
                    <<"video_codec">> => fetch_codec(Ps, <<"video">>),
                    <<"video_bit_rate">> => fetch_stream_bit_rate(Ps, <<"video">>),
                    <<"video_frame_rate">> => fetch_video_frame_rate(Ps),
                    <<"video_pixel_format">> => fetch_video_pixel_format(Ps),
                    <<"video_profile">> => fetch_video_profile(Ps)
                },
                maps:filter(
                    fun(_K, V) -> V =/= undefined end,
                    Info)
            catch
                error:E ->
                    ?LOG_WARNING(#{
                        in => zotonic_mod_video,
                        text => <<"Unexpected ffprobe return">>,
                        command => FfprobeCmd,
                        result => error,
                        reason => E,
                        probe_output => JSONText
                    }),
                    #{}
            end;
        {error, Reason} ->
            ?LOG_WARNING(#{
                in => zotonic_mod_video,
                text => <<"Unexpected ffprobe command error">>,
                command => FfprobeCmd,
                result => error,
                reason => Reason
            }),
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

fetch_format_name(#{<<"format">> := #{<<"format_name">> := FormatName}}) ->
    FormatName;
fetch_format_name(_) ->
    undefined.

fetch_format_long_name(#{<<"format">> := #{<<"format_long_name">> := FormatLongName}}) ->
    FormatLongName;
fetch_format_long_name(_) ->
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

fetch_stream_bit_rate(#{<<"streams">> := Streams}, Type) ->
    fetch_stream_prop(Streams, Type, <<"bit_rate">>, fun z_convert:to_integer/1);
fetch_stream_bit_rate(_, _) ->
    undefined.

fetch_audio_channels(#{<<"streams">> := Streams}) ->
    fetch_stream_prop(Streams, <<"audio">>, <<"channels">>, fun z_convert:to_integer/1);
fetch_audio_channels(_) ->
    undefined.

fetch_audio_channel_layout(#{<<"streams">> := Streams}) ->
    fetch_stream_prop(Streams, <<"audio">>, <<"channel_layout">>);
fetch_audio_channel_layout(_) ->
    undefined.

fetch_audio_sample_rate(#{<<"streams">> := Streams}) ->
    fetch_stream_prop(Streams, <<"audio">>, <<"sample_rate">>, fun z_convert:to_integer/1);
fetch_audio_sample_rate(_) ->
    undefined.

fetch_video_frame_rate(#{<<"streams">> := Streams}) ->
    fetch_stream_prop(Streams, <<"video">>, <<"avg_frame_rate">>, fun frame_rate/1);
fetch_video_frame_rate(_) ->
    undefined.

fetch_video_pixel_format(#{<<"streams">> := Streams}) ->
    fetch_stream_prop(Streams, <<"video">>, <<"pix_fmt">>);
fetch_video_pixel_format(_) ->
    undefined.

fetch_video_profile(#{<<"streams">> := Streams}) ->
    fetch_stream_prop(Streams, <<"video">>, <<"profile">>);
fetch_video_profile(_) ->
    undefined.

fetch_stream_prop(Streams, Type, Prop) ->
    fetch_stream_prop(Streams, Type, Prop, fun(V) -> V end).

fetch_stream_prop([ #{ <<"codec_type">> := CT } = S | _ ], Type, Prop, Convert) when CT =:= Type ->
    case maps:get(Prop, S, undefined) of
        undefined -> undefined;
        Value -> safe_convert(Value, Convert)
    end;
fetch_stream_prop([ _ | Streams ], Type, Prop, Convert) ->
    fetch_stream_prop(Streams, Type, Prop, Convert);
fetch_stream_prop([], _Type, _Prop, _Convert) ->
    undefined.

safe_convert(Value, Convert) ->
    try
        Convert(Value)
    catch
        _:_ -> undefined
    end.

frame_rate(<<"0/0">>) ->
    undefined;
frame_rate(Rate) ->
    try
        case binary:split(z_convert:to_binary(Rate), <<"/">>) of
            [N, D] ->
                Denominator = z_convert:to_float(D),
                case Denominator == 0.0 of
                    true -> undefined;
                    false -> round_frame_rate(z_convert:to_float(N) / Denominator)
                end;
            [N] ->
                round_frame_rate(z_convert:to_float(N))
        end
    catch
        _:_ -> undefined
    end.

round_frame_rate(FrameRate) ->
    round(FrameRate * 100) / 100.
