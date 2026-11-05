%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2023 Marc Worrell
%% @doc Video support for Zotonic. Converts all video files to mp4 and extracts a previes image.
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

-module(mod_video).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "content_editor", "module", "video_management", "media_management", "file_uploads", "transform"
    ]
}).
-moduledoc("
Adds support for viewing and handling video medium items.

This module converts uploaded videos to h264 and adds a poster (preview) image of the movie.

::: note
mod_video uses the command-line utilities `ffmpeg` and `ffprobe`. For mod_video to function correctly they must be
present in the search path of Zotonic.
:::



Uploading & conversion
----------------------

The video module hooks into the media model to intercept any video upload. If a video is uploaded the following steps
are done:

*   The original video is stored in CAFS and pinned by its persistent conversion task.
*   A video conversion task is added to the pivot task queue, this task will restart a video conversion in case of any problems.
*   A video conversion process is started, supervised by the video module.
*   The uploaded medium is replaced by a static `lib/images/processing.png` image.

The worker obtains a temporary local input from CAFS, including when the original
has been moved to remote storage. Completion or cancellation releases the source
pin. Existing tasks using `files/video_queue/` remain supported.

Only a single video conversion process is allowed to run at any time. This to prevent overloading the server.

After the video is converted the resource’s medium record is replaced with the converted video. The frame at 10
seconds (at 1 second for movies shorter than 30 seconds) is added as the preview image of the video.

Validated terminal conversion failures replace the medium with `lib/images/broken.png`.
Temporary runner failures retain the task and original (including legacy queue files),
retrying with exponential backoff from 30 seconds to one hour, plus up to ten seconds
of jitter. After 24 hours of failed automatic retries the task pauses. Invalid
credentials, configuration, integrity errors and unclassified failures pause immediately.
A failed medium commit also retains its input. Paused tasks reconcile hourly so a
replaced or deleted medium releases its retained source.

Retry metadata is stored in the pivot task's `{convert_v4, OriginalTask, RetryMap}`
argument. It records `attempts`, `started`, `last_reason`, `next_attempt` and `paused`.
Administrators can resume a stored task with `mod_video:retry_task(Task, Context)`.
Retry preserves the processing identity and the expected-medium check; it cannot
overwrite a newer upload. These retry policy values are fixed defaults, not site
configuration keys.



Viewing
-------

The video module extends the `{% media %}` tag for viewing `video/mp4` videos.

It uses the template `_video_viewer.tpl` for viewing. For the best viewing results, add `css/video.css` to your included
css files.

Accepted Events
---------------

This module handles the following notifier callbacks:

- `observe_media_stillimage`: Return the filename of a still image to be used for image tags using `supervisor:start_link`.
- `observe_media_upload_preprocess`: If a video file is uploaded, queue it for conversion to video/mp4 using `z_video_info:info`.
- `observe_media_upload_props`: After a video file is processed, generate a preview image using `z_media_archive:abspath`.
- `observe_media_viewer`: Return the media viewer for the mp4 video using `z_template:render`.
").

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Video").
-mod_description("Play and convert uploaded videos. Requires ffmpeg.").

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

-define(TEMP_IMAGE, <<"images/processing.gif">>).
-define(BROKEN_IMAGE, <<"images/broken.png">>).
% -define(POSTER_IMAGE, <<"images/poster.png">>).

-define(TASK_DELAY, 3600).


-include_lib("zotonic_core/include/zotonic.hrl").

-export([
         observe_media_upload_preprocess/2,
         observe_media_upload_props/3,
         observe_media_viewer/2,
         observe_media_stillimage/2,

         post_insert_fun/5,
         remove_task/2,
         convert_task/2,
         retry_task/2,
         queue_path/2
        ]).

%% @doc If a video file is uploaded, queue it for conversion to video/mp4
observe_media_upload_preprocess(#media_upload_preprocess{mime= <<"video/mp4">>, file=undefined}, _Context) ->
    undefined;
observe_media_upload_preprocess(#media_upload_preprocess{mime= <<"video/x-mp4-broken">>}, Context) ->
    do_media_upload_broken(Context);
observe_media_upload_preprocess(#media_upload_preprocess{mime= <<"video/", _/binary>> = Mime, medium=Medium, file=File} = Upload, Context) ->
    case maps:get(<<"is_video_ok">>, Medium, undefined) of
        true ->
            undefined;
        undefined ->
            case is_video_process_needed(Mime, File, Context) of
                true ->
                    do_media_upload_preprocess(Upload, Context);
                false ->
                    undefined
            end
    end;
observe_media_upload_preprocess(#media_upload_preprocess{}, _Context) ->
    undefined.

%% @doc Do not process landscape mp4 files with aac/h264 codecs.
is_video_process_needed(<<"video/mp4">>, File, Context) ->
    Info = z_video_info:info(File, Context),
    not (
                is_orientation_ok(Info)
        andalso is_audio_ok(Info)
        andalso is_video_ok(Info)
    );
is_video_process_needed(_Mime, _File, _Context) ->
    true.

is_orientation_ok(#{ <<"orientation">> := 1 }) -> true;
is_orientation_ok(#{ <<"orientation">> := undefined }) -> true;
is_orientation_ok(_) -> false.

is_audio_ok(#{ <<"audio_codec">> := <<"aac">> }) -> true;
is_audio_ok(#{ <<"audio_codec">> := undefined }) -> true;
is_audio_ok(#{ <<"audio_codec">> := _ }) -> false;
is_audio_ok(_) -> true.

is_video_ok(#{ <<"video_codec">> := <<"h264">> }) -> true;
is_video_ok(#{ <<"video_codec">> := undefined }) -> true;
is_video_ok(#{ <<"video_codec">> := _ }) -> false;
is_video_ok(_) -> true.


do_media_upload_preprocess(Upload, Context) ->
    case z_module_indexer:find(lib, ?TEMP_IMAGE, Context) of
        {ok, #module_index{ filepath = Filename }} ->
            ProcessNr = z_ids:identifier(20),
            PostFun = fun(InsId, InsMedium, InsContext) ->
                          ?MODULE:post_insert_fun(InsId, InsMedium, Upload, ProcessNr, InsContext)
                      end,
            {ok, MInfo} = z_media_identify:identify_file(Filename, Context),
            #media_upload_preprocess{
                mime = <<"video/mp4">>,
                file = undefined,
                post_insert_fun = PostFun,
                original_filename = undefined,
                medium = #{
                    <<"preview_filename">> => <<"lib/", ?TEMP_IMAGE/binary>>,
                    <<"preview_width">> => maps:get(<<"width">>, MInfo, undefined),
                    <<"preview_height">> => maps:get(<<"height">>, MInfo, undefined),
                    <<"width">> => maps:get(<<"width">>, MInfo, undefined),
                    <<"height">> => maps:get(<<"height">>, MInfo, undefined),
                    <<"is_deletable_preview">> => false,
                    <<"is_video_processing">> => true,
                    <<"video_processing_nr">> => ProcessNr,
                    <<"original_filename">> => Upload#media_upload_preprocess.original_filename
                }
            };
        {error, enoent} ->
            undefined
    end.

do_media_upload_broken(Context) ->
    case z_module_indexer:find(lib, ?BROKEN_IMAGE, Context) of
        {ok, #module_index{filepath=Filename}} ->
            {ok, MInfo} = z_media_identify:identify_file(Filename, Context),
            #media_upload_preprocess{
                 mime = <<"video/mp4">>,
                 file = undefined,
                 original_filename = undefined,
                 medium = #{
                     <<"preview_filename">> => <<"lib/", ?BROKEN_IMAGE/binary>>,
                     <<"preview_width">> => maps:get(<<"width">>, MInfo, undefined),
                     <<"preview_height">> => maps:get(<<"height">>, MInfo, undefined),
                     <<"width">> => maps:get(<<"width">>, MInfo, undefined),
                     <<"height">> => maps:get(<<"height">>, MInfo, undefined),
                     <<"is_deletable_preview">> => false,
                     <<"is_video_broken">> => true
              }
            };
        {error, enoent} ->
            undefined
    end.

%% @doc After a video file is processed, generate a preview image.
observe_media_upload_props(#media_upload_props{archive_file=undefined, mime= <<"video/", _/binary>>}, Medium, _Context) ->
    Medium;
observe_media_upload_props(#media_upload_props{id=Id, archive_file=File, mime= <<"video/", _/binary>>}, Medium, Context) ->
    FileAbs = z_media_archive:abspath(File, Context),
    Info = z_video_info:info(FileAbs, Context),
    Info2 = case z_video_preview:preview(FileAbs, Info, Context) of
        {ok, TmpFile} ->
            PreviewFilename = preview_filename(Id, Context),
            PreviewPath = z_media_archive:abspath(PreviewFilename, Context),
            ok = z_media_preview:convert(TmpFile, PreviewPath, [{quality,70}], Context),
            _ = file:delete(TmpFile),
            Info#{
                <<"preview_filename">> => PreviewFilename,
                <<"preview_width">> => maps:get(<<"width">>, Info, undefined),
                <<"preview_height">> => maps:get(<<"height">>, Info, undefined),
                <<"is_deletable_preview">> => true
            };
        {error, _} ->
            Info
    end,
    maps:merge(Medium, Info2);
observe_media_upload_props(#media_upload_props{}, Medium, _Context) ->
    Medium.


%% @doc Return the media viewer for the mp4 video
-spec observe_media_viewer(#media_viewer{}, z:context()) -> undefined | {ok, template_compiler:render_result()}.
observe_media_viewer(#media_viewer{props=Props, options=Options}, Context) ->
    case maps:get(<<"mime">>, Props, undefined) of
        <<"video/mp4">> ->
            Vars = [
                {props, Props},
                {options, Options}
            ],
            {ok, z_template:render(#render{template="_video_viewer.tpl", vars = Vars}, Context)};
        _ ->
            undefined
    end.


%% @doc Return the filename of a still image to be used for image tags.
-spec observe_media_stillimage(#media_stillimage{}, z:context()) -> undefined | {ok, file:filename_all()}.
observe_media_stillimage(#media_stillimage{ props = #{ <<"mime">> := <<"video/mp4">> } = Props }, _Context) ->
    case z_convert:to_binary(maps:get(<<"preview_filename">>, Props, undefined)) of
        <<>> -> {ok, <<"lib/images/poster.png">>};
        PreviewFile -> {ok, PreviewFile}
    end;
observe_media_stillimage(#media_stillimage{}, _Context) ->
    undefined.

%% --------------- Supervisor callbacks ---------------

start_link(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    ensure_job_queues(),
    supervisor:start_link({local, z_utils:name_for_site(?SERVER, Context)}, ?MODULE, []).

init([]) ->
    Element = {z_video_convert, {z_video_convert, start_link, []},
               temporary, brutal_kill, worker, [z_video_convert]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

%% --------------- Support routines ---------------

ensure_job_queues() ->
  jobs:run(zotonic_singular_job, fun ensure_job_queues_1/0).

ensure_job_queues_1() ->
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


%% @doc The medium record has been inserted, queue a conversion
post_insert_fun(Id, Medium, Upload, ProcessNr, Context) ->
    UploadedFile = Upload#media_upload_preprocess.file,
    QueueFilename = iolist_to_binary([integer_to_binary(Id), $-, ProcessNr]),
    {ok, #{hash := Hash, token := Token}} = m_cafs:stage_file(UploadedFile, Context),
    Owner = <<"video:", QueueFilename/binary>>,
    Task = {convert_v3, Id, Medium, Upload, QueueFilename, ProcessNr, z_context:pickle(Context), Hash},
    ok = z_db:transaction(fun(Ctx) ->
        ok = m_cafs:pin(Hash, Owner, Ctx),
        z_pivot_rsc:insert_task_after(?TASK_DELAY, ?MODULE, convert_task, QueueFilename, [Task], Ctx),
        m_cafs:unpin(Hash, Token, Ctx)
    end, Context),
    supervisor:start_child(z_utils:name_for_site(?SERVER, Context), [Task, z_context:prune_for_async(Context)]),
    ok.

-spec remove_task( file:filename_all(), z:context() ) -> non_neg_integer().
remove_task(QueueFilename, Context) ->
    z_pivot_rsc:delete_task(?MODULE, convert_task, QueueFilename, Context).

%% The worker owns the next due date. Schedule recovery before starting it, so
%% insert_task_after replaces the pivot row (and its id), so the scheduler's
%% update of its original row cannot overwrite the worker's next attempt.
convert_task(Task, Context) ->
    Original = case Task of {convert_v4, T, _} -> T; _ -> Task end,
    z_pivot_rsc:insert_task_after(?TASK_DELAY, ?MODULE, convert_task, element(5, Original), [Task], Context),
    _ = supervisor:start_child(z_utils:name_for_site(?SERVER, Context), [Task, Context]),
    {delay, ?TASK_DELAY}.

%% @doc Resume a retained conversion using its stored task arguments (admin API).
-spec retry_task(tuple(), z:context()) -> {delay, pos_integer()} | {error, eacces}.
retry_task(Task, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            Original = case Task of {convert_v4, T, _} -> T; _ -> Task end,
            convert_task(Original, Context);
        false -> {error, eacces}
    end.

queue_path(Filename, Context) ->
    QueueDir = z_path:files_subdir("video_queue", Context),
    filename:join(QueueDir, Filename).


preview_filename(Id, Context) ->
    m_media:make_preview_unique(Id, <<".jpg">>, Context).
