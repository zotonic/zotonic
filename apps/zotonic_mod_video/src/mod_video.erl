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
            case is_video_process_needed(Mime, File) of
                true ->
                    do_media_upload_preprocess(Upload, Context);
                false ->
                    undefined
            end
    end;
observe_media_upload_preprocess(#media_upload_preprocess{}, _Context) ->
    undefined.

%% @doc Do not process landscape mp4 files with aac/h264 codecs.
is_video_process_needed(<<"video/mp4">>, File) ->
    Info = z_video_info:info(File),
    not (
                is_orientation_ok(Info)
        andalso is_audio_ok(Info)
        andalso is_video_ok(Info)
    );
is_video_process_needed(_Mime, _File) ->
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
                    <<"video_processing_nr">> => ProcessNr
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
    Info = z_video_info:info(FileAbs),
    Info2 = case z_video_preview:preview(FileAbs, Info) of
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
    % Move the temp file to the video_queue in the files folder
    UploadedFile = Upload#media_upload_preprocess.file,
    QueueFilename = lists:flatten([integer_to_list(Id), $-, z_convert:to_list(ProcessNr)]),
    QueuePath = queue_path(QueueFilename, Context),
    ok = z_filelib:ensure_dir(QueuePath),
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
    supervisor:start_child(z_utils:name_for_site(?SERVER, Context), [Task, z_context:prune_for_async(Context)]),
    ok.

-spec remove_task( file:filename_all(), z:context() ) -> non_neg_integer().
remove_task(QueueFilename, Context) ->
    z_pivot_rsc:delete_task(?MODULE, convert_task, QueueFilename, Context).

convert_task(Task, Context) ->
    _ = supervisor:start_child(z_utils:name_for_site(?SERVER, Context), [Task, Context]),
    {delay, ?TASK_DELAY}.

queue_path(Filename, Context) ->
    QueueDir = z_path:files_subdir("video_queue", Context),
    filename:join(QueueDir, Filename).


preview_filename(Id, Context) ->
    m_media:make_preview_unique(Id, <<".jpg">>, Context).

