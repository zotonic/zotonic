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

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Audio").
-mod_description("Play uploaded audio files.").

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    observe_media_viewer/2,
    observe_media_upload_props/3
]).

% For testing
-export([
    audio_info/1
]).

-define(FFPROBE_CMDLINE, "ffprobe -loglevel quiet -show_format -show_streams -print_format json ").


%% @doc Return the media viewer for the audio
-spec observe_media_viewer(#media_viewer{}, z:context()) -> undefined | {ok, template_compiler:render_result()}.
observe_media_viewer(#media_viewer{props=Props, options=Options}, Context) ->
    case maps:get(<<"mime">>, Props, undefined) of
        <<"audio/mpeg">> ->
            Vars = [
                {props, Props},
                {options, Options}
            ],
            {ok, z_template:render(#render{template="_audio_viewer.tpl", vars = Vars}, Context)};
        _ ->
            undefined
    end.

observe_media_upload_props(#media_upload_props{archive_file=undefined, mime= <<"audio/", _/binary>>}, Medium, _Context) ->
    Medium;
observe_media_upload_props(#media_upload_props{id=_Id, archive_file=File, mime= <<"audio/", _/binary>>}, Medium, Context) ->
    FileAbs = z_media_archive:abspath(File, Context),
    Info = audio_info(FileAbs),
    ?DEBUG(maps:merge(Medium, Info));
observe_media_upload_props(#media_upload_props{}, Medium, _Context) ->
    Medium.


audio_info(Path) ->
    Cmdline = case z_config:get(ffprobe_cmdline) of
        undefined -> ?FFPROBE_CMDLINE;
        <<>> -> ?FFPROBE_CMDLINE;
        "" -> ?FFPROBE_CMDLINE;
        CmdlineCfg -> z_convert:to_list(CmdlineCfg)
    end,
    FfprobeCmd = lists:flatten([
           Cmdline, " ", z_utils:os_filename(Path)
       ]),
    lager:debug("Audio info: ~p", [FfprobeCmd]),
    JSONText = unicode:characters_to_binary(os:cmd(FfprobeCmd)),
    try
        Ps = decode_json(JSONText),
        #{
            <<"duration">> => fetch_duration(Ps)
        }
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

% "format": {
%      "filename": "apps_user/foobar/priv/files/archive/2014/12/30/redacted.mp3",
%      "nb_streams": 1,
%      "nb_programs": 0,
%      "format_name": "mp3",
%      "format_long_name": "MP2/3 (MPEG audio layer 2/3)",
%      "start_time": "0.000000",
%      "duration": "162.037625",
%      "size": "1300397",
%      "bit_rate": "64202",
%      "probe_score": 51,
%      "tags": {
%          "TSS": "GarageBand 6.0.5",
%          "artist": "...",
%          "TCM": "...",
%          "TBP": "120",
%          "title": "...",
%          "album": "...",
%          "date": "2013-04-19 17:31"
%      }
%  }

