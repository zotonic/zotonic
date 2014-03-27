%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Video support for Zotonic. Converts all video files to mp4 and extracts a previes image.

%% Copyright 2014 Marc Worrell
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

-module(mod_video).

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Video").
-mod_description("Play and convert uploaded videos. Requires ffmpeg.").

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

-define(TEMP_IMAGE, "images/processing.gif").
-define(POSTER_IMAGE, "images/poster.png").
-define(BROKEN_IMAGE, "images/broken.png").

-define(TASK_DELAY, 3600).


-include_lib("zotonic.hrl").

-export([
    observe_media_upload_preprocess/2,
    observe_media_upload_props/3,
    observe_media_viewer/2,
    observe_media_stillimage/2,

    post_insert_fun/5,
    remove_task/2,
    convert_task/2,
    queue_path/2,

    video_info/1,
    video_preview/2,

    orientation_to_transpose/1
    ]).

%% @doc If a video file is uploaded, queue it for conversion to video/mp4
observe_media_upload_preprocess(#media_upload_preprocess{mime="video/mp4", file=undefined}, _Context) ->
    undefined;
observe_media_upload_preprocess(#media_upload_preprocess{mime="video/x-mp4-broken"}, Context) ->
    do_media_upload_broken(Context);
observe_media_upload_preprocess(#media_upload_preprocess{mime="video/"++_, medium=Medium} = Upload, Context) ->
    case proplists:get_value(is_video_ok, Medium) of
        true ->
            undefined;
        undefined ->
            do_media_upload_preprocess(Upload, Context)
    end;
observe_media_upload_preprocess(#media_upload_preprocess{}, _Context) ->
    undefined.

do_media_upload_preprocess(Upload, Context) ->
    case z_module_indexer:find(lib, ?TEMP_IMAGE, Context) of
        {ok, #module_index{filepath=Filename}} ->
            ProcessNr = z_convert:to_binary(z_ids:identifier(20)),
            PostFun = fun(InsId, InsMedium, InsContext) ->
                            ?MODULE:post_insert_fun(InsId, InsMedium, Upload, ProcessNr, InsContext)
                      end,
            {ok, MInfo} = z_media_identify:identify_file(Filename, Context),
            #media_upload_preprocess{
                mime = "video/mp4",
                file = undefined,
                post_insert_fun = PostFun,
                original_filename = undefined, 
                medium = [
                    {preview_filename, "lib/"++?TEMP_IMAGE},
                    {preview_width, proplists:get_value(width, MInfo)},
                    {preview_height, proplists:get_value(height, MInfo)},
                    {width, proplists:get_value(width, MInfo)},
                    {height, proplists:get_value(height, MInfo)},
                    {is_deletable_preview, false},
                    {is_video_processing, true},
                    {video_processing_nr, ProcessNr}
                ]
            };
        {error, enoent} ->
            undefined
    end.

do_media_upload_broken(Context) ->
    case z_module_indexer:find(lib, ?BROKEN_IMAGE, Context) of
        {ok, #module_index{filepath=Filename}} ->
            {ok, MInfo} = z_media_identify:identify_file(Filename, Context),
            #media_upload_preprocess{
                mime = "video/mp4",
                file = undefined,
                original_filename = undefined, 
                medium = [
                    {preview_filename, "lib/"++?BROKEN_IMAGE},
                    {preview_width, proplists:get_value(width, MInfo)},
                    {preview_height, proplists:get_value(height, MInfo)},
                    {width, proplists:get_value(width, MInfo)},
                    {height, proplists:get_value(height, MInfo)},
                    {is_deletable_preview, false},
                    {is_video_broken, true}
                ]
            };
        {error, enoent} ->
            undefined
    end.

%% @doc After a video file is processed, generate a preview image.
observe_media_upload_props(#media_upload_props{archive_file=undefined, mime="video/" ++ _}, Medium, _Context) ->
    Medium;
observe_media_upload_props(#media_upload_props{id=Id, archive_file=File, mime="video/" ++ _}, Medium, Context) ->
    FileAbs = z_media_archive:abspath(File, Context),
    Info = video_info(FileAbs),
    Info2 = case video_preview(FileAbs, Info) of
             {ok, TmpFile} ->
                PreviewFilename = preview_filename(Id, File),
                PreviewPath = z_media_archive:abspath(PreviewFilename, Context),
                ok = z_media_preview:convert(TmpFile, PreviewPath, [{quality,70}], Context),
                _ = file:delete(TmpFile), 
                [
                    {preview_filename, PreviewFilename},
                    {preview_width, proplists:get_value(width, Info)},
                    {preview_height, proplists:get_value(height, Info)},
                    {is_deletable_preview, true}
                    | Info
                ];
             {error, _} ->
                 Info
           end,
    z_utils:props_merge(Info2, Medium); 
observe_media_upload_props(#media_upload_props{}, Medium, _Context) ->
    Medium.


%% @doc Return the media viewer for the mp4 video
-spec observe_media_viewer(#media_viewer{}, #context{}) -> undefined | {ok, list()|binary()}.
observe_media_viewer(#media_viewer{props=Props, options=Options}, Context) ->
    case proplists:get_value(mime, Props) of
        <<"video/mp4">> ->
            {ok, z_template:render(#render{template="_video_viewer.tpl", vars=[{props,Props},{options,Options}]}, Context)};
        _ ->
            undefined
    end.


%% @doc Return the filename of a still image to be used for image tags.
-spec observe_media_stillimage(#media_stillimage{}, #context{}) -> undefined | {ok, file:filename()}.
observe_media_stillimage(#media_stillimage{id=Id, props=Props}, Context) ->
    case proplists:get_value(mime, Props) of
        <<"video/mp4">> ->
            case m_rsc:p(Id, depiction, Context) of
                undefined ->
                    case z_convert:to_list(proplists:get_value(preview_filename, Props)) of
                        [] -> {ok, "lib/images/poster.png"};
                        PreviewFile -> {ok, PreviewFile}
                    end;
                DepictionProps ->
                    case z_convert:to_list(proplists:get_value(filename, DepictionProps)) of
                        [] -> undefined;
                        Filename -> {ok, Filename}
                    end
            end;
        _ ->
            undefined
    end.


start_link(Args) ->
    {context, Context} = proplists:lookup(context, Args), 
    ensure_job_queues(),
    supervisor:start_link({local, z_utils:name_for_host(?SERVER, Context)}, ?MODULE, []).

ensure_job_queues() ->
    case jobs:queue_info(video_jobs) of
        undefined ->
            jobs:add_queue(video_jobs, [
                    {regulators, [
                          {counter, [
                                {limit, 1},
                                {modifiers, [{cpu, 1}]}
                          ]}
                    ]}
                ]);
        {queue, _} ->
            ok
    end,
    case jobs:queue_info(media_preview_jobs) of
        undefined ->
            jobs:add_queue(media_preview_jobs, [
                    {regulators, [
                          {counter, [
                                {limit, 3},
                                {modifiers, [{cpu, 1}]}
                          ]}
                    ]}
                ]);
        {queue, _} ->
            ok
    end.


init([]) ->
    Element = {z_video_convert, {z_video_convert, start_link, []},
               temporary, brutal_kill, worker, [z_video_convert]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.


%% @doc The medium record has been inserted, queue a conversion
post_insert_fun(Id, Medium, Upload, ProcessNr, Context) ->
    % Move the temp file to the video_queue in the files folder
    UploadedFile = Upload#media_upload_preprocess.file,
    QueueFilename = lists:flatten([integer_to_list(Id), $-, z_convert:to_list(ProcessNr)]),
    QueuePath = queue_path(QueueFilename, Context),
    ok = filelib:ensure_dir(QueuePath),
    case z_tempfile:is_tempfile(UploadedFile) of
        true ->
            case file:rename(UploadedFile, QueuePath) of
                %% cross-fs rename is not supported by erlang, so copy and delete the file
                {error, exdev} ->
                    {ok, _BytesCopied} = file:copy(UploadedFile, QueuePath),
                    ok = file:delete(UploadedFile);
                ok -> 
                    ok
            end;
        false ->
            {ok, _BytesCopied} = file:copy(UploadedFile, QueuePath)
    end,
    Task = {convert_v2, Id, Medium, Upload, QueueFilename, ProcessNr, z_context:pickle(Context)},
    z_pivot_rsc:insert_task_after(?TASK_DELAY, ?MODULE, convert_task, QueueFilename, [Task], Context),
    supervisor:start_child(z_utils:name_for_host(?SERVER, Context), [Task, z_context:prune_for_async(Context)]),
    ok.

remove_task(QueueFilename, Context) ->
    z_pivot_rsc:delete_task(?MODULE, convert_task, QueueFilename, Context).

convert_task(Task, Context) ->
    _ = supervisor:start_child(z_utils:name_for_host(?SERVER, Context), [Task, Context]),
    {delay, ?TASK_DELAY}.

queue_path(Filename, Context) ->
    QueueDir = z_path:files_subdir("video_queue", Context),
    filename:join(QueueDir, Filename).

video_info(Path) ->
    FfprobeCmd = lists:flatten([
            "ffprobe -loglevel quiet -show_format -show_streams -print_format json ",
            z_utils:os_filename(Path) 
        ]),
    lager:debug("Video info: ~p", [FfprobeCmd]),
    JSONText = z_convert:to_binary(os:cmd(FfprobeCmd)),
    try
        {struct, Ps} = decode_json(JSONText),
        {Width, Height, Orientation} = fetch_size(Ps),
        [
            {duration, fetch_duration(Ps)},
            {width, Width},
            {height, Height},
            {orientation, Orientation}
        ]
    catch
        error:E ->
            lager:warning("Unexpected ffprobe return (~p) ~p", [E, JSONText]),
            []
    end.

decode_json(JSONText) ->
    try
        mochijson2:decode(JSONText)
    catch
        throw:invalid_utf8 ->
            decode_json(list_to_binary([ C band 127 || C <- binary_to_list(JSONText) ]))
    end.

fetch_duration(Ps) ->
    case proplists:get_value(<<"format">>, Ps) of
        {struct, Fs} ->
            Duration = proplists:get_value(<<"duration">>, Fs),
            round(z_convert:to_float(Duration));
        undefined ->
            0
    end.

fetch_size(Ps) ->
    Streams = proplists:get_value(<<"streams">>, Ps),
    [{struct, Video}|_] = lists:dropwhile(
                                fun({struct,S}) -> 
                                    proplists:get_value(<<"codec_type">>, S) =/= <<"video">> 
                                end, Streams),
    {<<"width">>, Width} = proplists:lookup(<<"width">>, Video),
    {<<"height">>, Height} = proplists:lookup(<<"height">>, Video),
    Orientation = orientation(proplists:get_value(<<"tags">>, Video)),
    case Orientation of
        6 -> {Height, Width, Orientation};
        8 -> {Height, Width, Orientation};
        _ -> {Width, Height, Orientation}
    end.


orientation({struct, Tags}) ->
    case proplists:get_value(<<"rotate">>, Tags) of
        undefined -> 
            1;
        Angle ->
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
            end
    end;
orientation(_) ->
    1.


video_preview(MovieFile, Props) ->
    Duration = proplists:get_value(duration, Props),
    Start = case Duration of
                N when N =< 1 -> 0;
                N when N =< 30 -> 1;
                _ -> 10
            end,
    TmpFile = z_tempfile:new(),
    FfmpegCmd = lists:flatten([
            "ffmpeg ",
            " -itsoffset -", integer_to_list(Start),
            " -i ", z_utils:os_filename(MovieFile),
            " -vcodec png ",
            " -vframes 1 ",
            " -an ",
            " -f rawvideo ",
            " -loglevel error ",
            " -y ",
            orientation_to_transpose(proplists:get_value(orientation, Props)),
            z_utils:os_filename(TmpFile) 
        ]),
    jobs:run(media_preview_jobs,
            fun() ->
                lager:debug("Video preview: ~p", [FfmpegCmd]),
                case os:cmd(FfmpegCmd) of
                    [] ->
                        {ok, TmpFile};
                    Other ->
                        {error, Other}
                end
            end).

orientation_to_transpose(8) -> " -vf 'transpose=2' ";
orientation_to_transpose(3) -> " -vf 'transpose=2,transpose=2' ";
orientation_to_transpose(6) -> " -vf 'transpose=1' ";
orientation_to_transpose(_) -> "".

preview_filename(Id, File) ->
    {{Y,M,D},_} = calendar:local_time(),
    Basename = filename:basename(File) ++ "-" ++ z_ids:identifier(10) ++ ".jpg",
    filename:join([ "preview",
                    integer_to_list(Y),
                    integer_to_list(M),
                    integer_to_list(D),
                    id_to_list(Id) ++ "-" ++ Basename]).

id_to_list(N) when is_integer(N) -> integer_to_list(N);
id_to_list(insert_rsc) -> "video".
