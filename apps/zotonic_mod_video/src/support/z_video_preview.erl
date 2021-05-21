%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2020 Marc Worrell
%% @doc Fetch a preview from a video file using ffmpeg.

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

-module(z_video_preview).

-export([
    preview/2,
    orientation_to_transpose/1
]).

-define(PREVIEW_CMDLINE, "ffmpeg -itsoffset -~p -i ~s -vcodec png -vframes 1 -an -f rawvideo -loglevel error -y").

-spec preview(file:filename_all(), map()) -> {ok, file:filename_all()} | {error, string()}.
preview(MovieFile, Props) ->
    #{
        <<"duration">> := Duration,
        <<"orientation">> := Orientation
    } = Props,
    Start = case Duration of
        N when N =< 1 -> 0;
        N when N =< 30 -> 1;
        _ -> 10
    end,
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
                _ -> io_lib:format(Cmdline, [Start, MovieFile])
            end,
            " ",
            orientation_to_transpose(Orientation),
            z_utils:os_filename(TmpFile)
        ])),
    jobs:run(media_preview_jobs,
        fun() ->
            lager:debug("Video preview: ~p", [FfmpegCmd]),
            case os:cmd(FfmpegCmd) of
                [] ->
                   lager:debug("Preview ok, file: ~p", [TmpFile]),
                   {ok, TmpFile};
                Other ->
                   lager:warning("Video preview error: ~p", [Other]),
                   {error, Other}
            end
        end).

orientation_to_transpose(8) -> " -vf 'transpose=2' ";
orientation_to_transpose(3) -> " -vf 'transpose=2,transpose=2' ";
orientation_to_transpose(6) -> " -vf 'transpose=1' ";
orientation_to_transpose(_) -> "".

