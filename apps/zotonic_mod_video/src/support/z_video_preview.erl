%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2023 Marc Worrell
%% @doc Fetch a preview from a video file using ffmpeg.
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

-module(z_video_preview).

-export([
    preview/2,
    orientation_to_transpose/1
]).

-include_lib("kernel/include/logger.hrl").

% Default timeout for video previews - 10 minutes
-define(FFMPEG_TIMEOUT, 10*60*1000).

-define(PREVIEW_CMDLINE, "ffmpeg -itsoffset -~p -i ~s -vcodec png -vframes 1 -an -f rawvideo -loglevel error -y").

-spec preview(file:filename_all(), map()) -> {ok, file:filename_all()} | {error, string()}.
preview(MovieFile, #{
        <<"duration">> := Duration,
        <<"orientation">> := Orientation
    }) ->
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
    FfmpegCmd = unicode:characters_to_binary([
            case string:str(Cmdline, "-itsoffset") of
                0 -> io_lib:format(Cmdline, [z_filelib:os_filename(MovieFile)]);
                _ -> io_lib:format(Cmdline, [Start, z_filelib:os_filename(MovieFile)])
            end,
            " ",
            orientation_to_transpose(Orientation),
            z_filelib:os_filename(TmpFile)
        ]),
    jobs:run(media_preview_jobs,
        fun() ->
            ?LOG_DEBUG(#{
                text => <<"Video preview">>,
                movie_file => MovieFile,
                tmp_file => TmpFile,
                command => FfmpegCmd
            }),
            case z_exec:run(FfmpegCmd, #{ timeout => ?FFMPEG_TIMEOUT }) of
                {ok, Stdout} ->
                   ?LOG_DEBUG(#{
                        text => <<"FFMPEG video preview ok">>,
                        result => ok,
                        command => FfmpegCmd,
                        movie_file => MovieFile,
                        tmp_file => TmpFile,
                        stdout => Stdout
                    }),
                    {ok, TmpFile};
                {error, Reason} ->
                   ?LOG_WARNING(#{
                        text => <<"FFMPEG video preview error">>,
                        result => error,
                        reason => Reason,
                        movie_file => MovieFile,
                        tmp_file => TmpFile,
                        command => FfmpegCmd
                    }),
                   {error, Reason}
            end
        end);
preview(MovieFile, _Props) ->
   ?LOG_WARNING(#{
        text => <<"Video preview skipped for non video">>,
        result => error,
        reason => novideo,
        movie_file => MovieFile
    }),
    {error, novideo}.

orientation_to_transpose(8) -> " -vf 'transpose=2' ";
orientation_to_transpose(3) -> " -vf 'transpose=2,transpose=2' ";
orientation_to_transpose(6) -> " -vf 'transpose=1' ";
orientation_to_transpose(_) -> "".

