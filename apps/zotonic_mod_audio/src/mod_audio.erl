%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc Audio support for Zotonic.

%% Copyright 2020 Marc Worrell
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

-module(mod_audio).
-moduledoc("
See also

[mod\\_video](/id/doc_module_mod_video), [mod\\_video\\_embed](/id/doc_module_mod_video_embed), [media](/id/doc_template_tag_tag_media)

Adds support for viewing and handling audio medium items.

This module parses audio files and extracts tags and an optional image from the audio file.

Note

mod\\_audio uses the command-line utilities `ffmpeg` and `ffprobe`. For mod\\_audio to function correctly they must be
present in the search path of Zotonic.



Uploading
---------

The audio module hooks into the media model to intercept any audio file upload.

If an audio file is uploaded then the file is parsed using `ffprobe` to fetch:

*   The bitrate
*   Audio duration
*   Tags, like artist, album
*   Album art image, extracted as png using `ffmpeg`

The optional album art is used as the preview image. This image is visible when the audio media item is rendered using a
`{% image ... %}` tag.



Viewing
-------

The audio module extends the `{% media %}` tag for viewing `audio/*` videos.

It uses the template `_audio_viewer.tpl` for viewing.



Editing
-------

The following properties are extracted from the audio file, and can be edited in the admin:

*   artist
*   album
*   album\\_artist
*   composer
*   genre
*   track
*   copyright
*   is\\_compilation (boolean flag)
*   org\\_pubdate, this is extracted form the `creation_time` tag

If there is no resource title given when creating the audio page then the title from the tags is used. If that one is
empty then the file’s basename is used.

Accepted Events
---------------

This module handles the following notifier callbacks:

- `observe_media_upload_props`: Set medium properties from the uploaded file using `z_media_archive:abspath`.
- `observe_media_upload_rsc_props`: Set resource properties from the medium properties using `z_datetime:to_datetime`.
- `observe_media_viewer`: Return the media viewer for the audio using `z_template:render`.

").

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Audio").
-mod_description("Play uploaded audio files.").

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    observe_media_viewer/2,
    observe_media_upload_props/3,
    observe_media_upload_rsc_props/3
]).

% For testing
-export([
    audio_info/1
]).

-define(FFPROBE_CMDLINE, "ffprobe -loglevel quiet -show_format -show_streams -print_format json ").
-define(PREVIEW_CMDLINE, "ffmpeg -i ~s -vcodec png -vframes 1 -an -f rawvideo -loglevel error -y").


%% @doc Return the media viewer for the audio
-spec observe_media_viewer(#media_viewer{}, z:context()) -> undefined | {ok, template_compiler:render_result()}.
observe_media_viewer(#media_viewer{props=Props, options=Options}, Context) ->
    case maps:get(<<"mime">>, Props, undefined) of
        <<"audio/", _/binary>> ->
            Vars = [
                {props, Props},
                {options, Options}
            ],
            {ok, z_template:render(#render{template="_audio_viewer.tpl", vars = Vars}, Context)};
        _ ->
            undefined
    end.


%% @doc Set medium properties from the uploaded file.
observe_media_upload_props(#media_upload_props{archive_file=undefined, mime= <<"audio/", _/binary>>}, Medium, _Context) ->
    Medium;
observe_media_upload_props(#media_upload_props{id=Id, archive_file=File, mime= <<"audio/", _/binary>>}, Medium, Context) ->
    FileAbs = z_media_archive:abspath(File, Context),
    Info = audio_info(FileAbs),
    Info2 = case audio_preview(FileAbs) of
        {ok, TmpFile} ->
            PreviewFilename = m_media:make_preview_unique(Id, <<".png">>, Context),
            PreviewPath = z_media_archive:abspath(PreviewFilename, Context),
            ok = z_media_preview:convert(TmpFile, PreviewPath, [], Context),
            {ok, PreviewInfo} = z_media_identify:identify_file(PreviewPath, Context),
            _ = file:delete(TmpFile),
            Info#{
                <<"preview_filename">> => PreviewFilename,
                <<"preview_width">> => maps:get(<<"width">>, PreviewInfo, undefined),
                <<"preview_height">> => maps:get(<<"height">>, PreviewInfo, undefined),
                <<"is_deletable_preview">> => true
            };
        {error, _} ->
            Info
    end,
    maps:merge(Medium, Info2);
observe_media_upload_props(#media_upload_props{}, Medium, _Context) ->
    Medium.


%% @doc Set resource properties from the medium properties
observe_media_upload_rsc_props(
        #media_upload_rsc_props{ id = insert_rsc, mime = <<"audio/", _/binary>>, medium = Medium },
        Props,
        _Context) ->
    case maps:get(<<"tags">>, Medium, #{}) of
        Tags when is_map(Tags) ->
            Props1 = maybe_date(<<"org_pubdate">>, Props, <<"creation_time">>, Tags),
            maps:fold(
                fun(Tag, Value, Acc) ->
                    case is_tag_prop(Tag) of
                        {true, Tag1} ->
                            case is_empty_prop(Tag1, Props) of
                                true ->
                                    Acc#{ Tag1 => Value };
                                false ->
                                    Acc
                            end;
                        false ->
                            Acc
                    end
                end,
                Props1,
                Tags);
        _ ->
            Props
    end;
observe_media_upload_rsc_props(#media_upload_rsc_props{}, Props, _Context) ->
    Props.


maybe_date(DateProp, Props, TagProp, Tags) ->
    case maps:get(TagProp, Tags, undefined) of
        undefined -> Props;
        <<>> -> Props;
        Date ->
            try
                Props#{
                    DateProp => z_datetime:to_datetime(Date)
                }
            catch
                _:_ -> Props
            end
    end.

is_tag_prop(<<"title">> = T) -> {true, T};
is_tag_prop(<<"artist">> = T) -> {true, T};
is_tag_prop(<<"album">> = T) -> {true, T};
is_tag_prop(<<"album_artist">> = T) -> {true, T};
is_tag_prop(<<"composer">> = T) -> {true, T};
is_tag_prop(<<"genre">> = T) -> {true, T};
is_tag_prop(<<"track">> = T) -> {true, T};
% is_tag_prop(<<"date">> = T) -> {true, T};
is_tag_prop(<<"copyright">> = T) -> {true, T};
is_tag_prop(<<"compilation">>) -> {true, <<"is_compilation">>};
is_tag_prop(_) -> false.

is_empty_prop(K, Props) ->
    z_utils:is_empty( maps:get(K, Props, undefined) ).


audio_info(Path) ->
    Cmdline = case z_config:get(ffprobe_cmdline) of
        undefined -> ?FFPROBE_CMDLINE;
        <<>> -> ?FFPROBE_CMDLINE;
        "" -> ?FFPROBE_CMDLINE;
        CmdlineCfg -> z_convert:to_list(CmdlineCfg)
    end,
    FfprobeCmd = lists:flatten([
           Cmdline, " ", z_filelib:os_filename(Path)
       ]),
    ?LOG_DEBUG(#{
        text => <<"Extract audio info">>,
        in => zotonic_mod_audio,
        command => FfprobeCmd
    }),
    JSONText = unicode:characters_to_binary(os:cmd(FfprobeCmd)),
    try
        Ps = decode_json(JSONText),
        Info = #{
            <<"duration">> => fetch_duration(Ps),
            <<"bit_rate">> => fetch_bit_rate(Ps),
            <<"tags">> => fetch_tags(Ps)
        },
        maps:filter(
            fun(_K, V) -> V =/= undefined end,
            Info)
    catch
        error:E ->
            ?LOG_WARNING(#{
                text => <<"Unexpected ffprobe return">>,
                in => zotonic_mod_audio,
                result => error,
                reason => E,
                ffprobe_return => JSONText
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

fetch_tags(#{ <<"format">> := #{ <<"tags">> := Tags }}) when is_map(Tags) ->
    maps:filter(fun is_tag_ok/2, Tags);
fetch_tags(_) ->
    undefined.

is_tag_ok(<<"iTunSMPB">>, _) -> false;
is_tag_ok(<<"iTunNORM">>, _) -> false;
is_tag_ok(_, _) -> true.



audio_preview(MovieFile) ->
    Cmdline = case z_config:get(ffmpeg_preview_cmdline) of
        undefined -> ?PREVIEW_CMDLINE;
        <<>> -> ?PREVIEW_CMDLINE;
        "" -> ?PREVIEW_CMDLINE;
        CmdlineCfg -> z_convert:to_list(CmdlineCfg)
    end,
    TmpFile = z_tempfile:new(),
    FfmpegCmd = z_convert:to_list(
        iolist_to_binary([
            case string:str(Cmdline, "-itsoffset") of
                0 -> io_lib:format(Cmdline, [MovieFile]);
                _ -> io_lib:format(Cmdline, [0, MovieFile])
            end,
            " ",
            z_filelib:os_filename(TmpFile)
        ])),
    jobs:run(media_preview_jobs,
        fun() ->
            case os:cmd(FfmpegCmd) of
                [] ->
                   {ok, TmpFile};
                Other ->
                   {error, Other}
            end
        end).

% "format": {
%     "filename": "Rammstein/RAMMSTEIN/01 DEUTSCHLAND.m4a",
%     "nb_streams": 2,
%     "nb_programs": 0,
%     "format_name": "mov,mp4,m4a,3gp,3g2,mj2",
%     "format_long_name": "QuickTime / MOV",
%     "start_time": "0.000000",
%     "duration": "323.082449",
%     "size": "11642783",
%     "bit_rate": "288292",
%     "probe_score": 100,
%     "tags": {
%         "major_brand": "M4A ",
%         "minor_version": "0",
%         "compatible_brands": "M4A mp42isom",
%         "creation_time": "2019-05-17 04:27:37",
%         "iTunSMPB": " 00000000 00000840 00000097 0000000000D95F29 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000",
%         "iTunNORM": " 00001693 00001AC2 00008845 000090D3 00033323 00033323 0000738D 00007382 00033AD9 0002F519",
%         "title": "DEUTSCHLAND",
%         "artist": "Rammstein",
%         "album_artist": "Rammstein",
%         "composer": "Richard Z. Kruspe, Paul Landers, Till Lindemann, Doktor Christian Lorenz, Oliver Riedel & Christoph Doom Schneider",
%         "album": "RAMMSTEIN",
%         "genre": "Rock",
%         "track": "1/11",
%         "date": "2019",
%         "compilation": "0"
%     }
% }
