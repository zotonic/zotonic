%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2023 Marc Worrell
%% @doc Process for converting a video to mp4
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

-module(z_video_convert).

-behaviour(gen_server).

-export([
         start_link/2,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-record(state, {
          task,
          retry = #{},
          id,
          medium,
          upload,
          queue_filename,
          source_hash,
          process_nr,
          site,
          pickled_context
         }).

% Default timeout for video conversions - 6 hours
-define(FFMPEG_TIMEOUT, 6*3600*1000).

start_link({convert_v4, Task, _Retry} = Args, Context) ->
    QueueFilename = element(5, Task),
    gen_server:start_link({via, z_proc, {video_convert, z_convert:to_binary(QueueFilename)}},
        ?MODULE, [Args, z_context:site(Context)], []);
start_link({convert_v1, _Id, _Medium, _Upload, _QueueFilename, _PickledContext}, _Context) ->
                                                % Flush old convert queue
    ok;
start_link({convert_v3, _Id, _Medium, _Upload, QueueFilename, _ProcessNr, _PickledContext, _Hash} = Args, Context) ->
    gen_server:start_link({via, z_proc, {video_convert, z_convert:to_binary(QueueFilename)}},
        ?MODULE, [Args, z_context:site(Context)], []);
start_link({convert_v2, _Id, _Medium, _Upload, QueueFilename, _ProcessNr, _PickledContext} = Args, Context) ->
    gen_server:start_link({via, z_proc, {video_convert, z_convert:to_binary(QueueFilename)}},
                          ?MODULE,
                          [Args, z_context:site(Context)],
                          []).

init([{convert_v4, Task, Retry}, Site]) ->
    {ok, State} = init([Task, Site]),
    {ok, State#state{retry=Retry}};
init([{convert_v3, Id, Medium, Upload, QueueFilename, ProcessNr, PickledContext, Hash} = Task, Site]) ->
    gen_server:cast(self(), convert),
    {ok, #state{task=Task, id=Id, medium=Medium, upload=Upload, queue_filename=QueueFilename,
        process_nr=ProcessNr, site=Site, pickled_context=PickledContext, source_hash=Hash}};
init([{convert_v2, Id, Medium, Upload, QueueFilename, ProcessNr, PickledContext} = Task, Site]) ->
    gen_server:cast(self(), convert),
    {ok, #state{
            task = Task,
            id = Id,
            medium = Medium,
            upload = Upload,
            queue_filename = QueueFilename,
            process_nr = ProcessNr,
            site = Site,
            pickled_context = PickledContext
           }}.

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_msg}, State}.

handle_cast(convert, State) ->
    Context = z_context:depickle(State#state.pickled_context),
    case is_current_upload(State, Context) of
        false -> finish(State);
        true ->
            case maps:get(paused, State#state.retry, false) of
                true -> save_retry(State#state.retry, 3600, State);
                false -> conversion_result(convert_source(State, Context), State)
            end
    end,
    {stop, normal, State}.

convert_source(#state{source_hash=Hash} = State, Context) when is_binary(Hash) ->
    m_cafs:with_local_file(Hash, fun(File) -> do_convert(File, State) end, Context);
convert_source(State, Context) ->
    do_convert(mod_video:queue_path(State#state.queue_filename, Context), State).

conversion_result({ok, _}, State) -> finish(State);
conversion_result({error, stale_medium}, State) -> finish(State);
conversion_result({error, Reason}, State) ->
    Now = erlang:system_time(second),
    Retry = State#state.retry,
    Started = maps:get(started, Retry, Now),
    Attempts = maps:get(attempts, Retry, 0) + 1,
    Paused = z_media_error:classify(Reason) =:= pause orelse Now - Started >= 86400,
    Delay = case Paused of
        true -> 3600;
        false -> min(3600, 30 * (1 bsl min(Attempts - 1, 7))) + rand:uniform(10)
    end,
    ?LOG_WARNING(#{text => <<"Video conversion retained for retry">>, in => zotonic_mod_video,
        rsc_id => State#state.id, reason => Reason, paused => Paused, attempts => Attempts}),
    save_retry(#{started => Started, attempts => Attempts, last_reason => Reason,
        next_attempt => Now + Delay, paused => Paused}, Delay, State).

save_retry(Retry, Delay, State) ->
    z_pivot_rsc:insert_task_after(Delay, mod_video, convert_task, State#state.queue_filename,
        [{convert_v4, State#state.task, Retry}], z_context:new(State#state.site)).

finish(State) ->
    remove_task(State),
    case State#state.source_hash of
        undefined -> file:delete(mod_video:queue_path(State#state.queue_filename,
            z_context:new(State#state.site)));
        _ -> ok
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec do_convert(file:filename_all(), #state{}) -> {ok, integer()} | {error, term()}.
do_convert(QueuePath, State) ->
    Upload = State#state.upload,
    case video_convert(QueuePath, Upload#media_upload_preprocess.mime, z_context:new(State#state.site)) of
        {ok, TmpFile} ->
            insert_movie(TmpFile, State);
        {error, Reason} ->
            ?LOG_WARNING(#{
                text => <<"ffmpeg conversion error">>,
                in => zotonic_mod_video,
                rsc_id => State#state.id,
                filename => QueuePath,
                result => error,
                reason => Reason
            }),
            case z_media_error:classify(Reason) of
                terminal -> insert_broken(State);
                _ -> {error, Reason}
            end
    end.

-spec insert_movie(file:filename_all(), #state{}) -> {ok, m_rsc:resource_id()} | {error, atom()}.
insert_movie(TmpFile, State) ->
    Context = z_context:depickle(State#state.pickled_context),
    case is_current_upload(State, Context) of
        true ->
            OrgFile = original_filename(State#state.upload),
            PropsMedia = #{
                <<"is_video_ok">> => true
            },
            m_media:replace_file(
                #upload{filename=OrgFile, tmpfile=TmpFile},
                State#state.id,
                #{},
                PropsMedia,
                [no_touch, {update_reason, media_processing},
                    {expected_medium, {<<"video_processing_nr">>, State#state.process_nr}}],
                Context);
        false ->
            ?LOG_INFO(#{
                text => <<"Video conversion (ok): medium is not current anymore">>,
                in => zotonic_mod_video,
                rsc_id => State#state.id
            }),
            {error, stale_medium}
    end.

-spec original_filename( #media_upload_preprocess{} ) -> binary().
original_filename(#media_upload_preprocess{original_filename=undefined}) ->
    <<"movie.mp4">>;
original_filename(#media_upload_preprocess{original_filename=OrgFile}) ->
    Root = z_string:to_rootname(OrgFile),
    << (z_convert:to_binary(Root))/binary, ".mp4">>.

insert_broken(State) ->
    Context = z_context:depickle(State#state.pickled_context),
    case is_current_upload(State, Context) of
        true ->
            PropsMedia = #{
                <<"mime">> => <<"video/x-mp4-broken">>,
                <<"size">> => 0
            },
            m_media:replace_medium(PropsMedia, State#state.id, #{},
                [{expected_medium, {<<"video_processing_nr">>, State#state.process_nr}}], Context);
        false ->
            ?LOG_NOTICE(#{
                text => <<"Video conversion (broken): medium is not current anymore">>,
                in => zotonic_mod_video,
                rsc_id => State#state.id
            }),
            {error, stale_medium}
    end.

is_current_upload(State, Context) ->
    case m_rsc:exists(State#state.id, Context) of
        true ->
            case m_media:get(State#state.id, Context) of
                #{ <<"video_processing_nr">> := PNr } ->
                    PNr =:= State#state.process_nr;
                _ ->
                    false
            end;
        false ->
            false
    end.

remove_task(State) ->
    Context = z_context:new(State#state.site),
    z_db:transaction(fun(Ctx) ->
        mod_video:remove_task(State#state.queue_filename, Ctx),
        case State#state.source_hash of
            undefined -> ok;
            Hash -> m_cafs:unpin(Hash, <<"video:", (State#state.queue_filename)/binary>>, Ctx)
        end
    end, Context).

video_convert(QueuePath, Mime, Context) ->
    Info = z_video_info:info(QueuePath, Context),
    video_convert_1(QueuePath, maps:get(<<"orientation">>, Info, 1), Mime, Context).

-define(CMDLINE,
        "ffmpeg -i "
        "~s"
        " -vcodec libx264 "
        " -loglevel fatal "
        " -f mp4 "
        " -strict -2 "
        " -y "
        " -pix_fmt yuv420p "
        " -movflags +faststart "
        " -preset medium "
        " -metadata:s:v:0 rotate=0 ").

-spec video_convert_1(file:filename_all(), integer(), string() | binary(), z:context()) ->
    {ok, file:filename_all()} | term().
video_convert_1(QueuePath, Orientation, Mime, Context) ->
    Cmdline = case z_config:get(ffmpeg_cmdline) of
                  undefined -> ?CMDLINE;
                  <<>> -> ?CMDLINE;
                  "" -> ?CMDLINE;
                  CmdLineCfg -> z_convert:to_list(CmdLineCfg)
              end,
    jobs:run(video_jobs,
             fun() ->
                TransposeOption = z_video_preview:orientation_to_transpose(Orientation),
                case maybe_reset_metadata(TransposeOption, QueuePath, Mime, Context) of
                    {ok, QueuePath1} ->
                        TmpFile = z_tempfile:new(),
                        FfmpegCmd = unicode:characters_to_binary(
                                        [io_lib:format(Cmdline, [z_filelib:os_filename(unicode:characters_to_list(QueuePath1))]),
                                         " ",
                                         TransposeOption,
                                         " ",
                                         z_filelib:os_filename(unicode:characters_to_list(TmpFile))
                                        ]),
                        ?LOG_INFO(#{
                            in => zotonic_mod_video,
                            text => <<"Video convert starting">>,
                            command => FfmpegCmd,
                            filename => unicode:characters_to_binary(QueuePath)
                        }),
                        RunOptions = #{
                            timeout => ?FFMPEG_TIMEOUT,
                            read => [QueuePath1], write => [TmpFile]
                        },
                        StartTimestamp = z_datetime:timestamp(),
                        case z_exec:run(ffmpeg, FfmpegCmd, RunOptions, Context) of
                            {ok, Stdout} ->
                                case filelib:file_size(TmpFile) of
                                    0 ->
                                        ?LOG_WARNING(#{
                                            in => zotonic_mod_video,
                                            text => <<"Video convert error: (empty result file)">>,
                                            result => error,
                                            reason => convert,
                                            command => FfmpegCmd,
                                            stdout => Stdout,
                                            filename => unicode:characters_to_binary(QueuePath)
                                        }),
                                        {error, convert};
                                    _ ->
                                        ?LOG_INFO(#{
                                            in => zotonic_mod_video,
                                            text => <<"Video convert done">>,
                                            filename => unicode:characters_to_binary(QueuePath),
                                            duration => z_datetime:timestamp() - StartTimestamp
                                        }),
                                        {ok, TmpFile}
                                end;
                            {error, Reason} ->
                                ?LOG_ERROR(#{
                                    in => zotonic_mod_video,
                                    text => <<"Video convert error">>,
                                    result => error,
                                    reason => Reason,
                                    command => FfmpegCmd,
                                    filename => unicode:characters_to_binary(QueuePath)
                                }),
                                {error, Reason}
                        end;
                    {error, _} = Error ->
                        Error
                end
             end).

  -define(CMDLINE_RESETMETA,
        "ffmpeg -i "
        "~s"
        " -strict -2 "
        " -loglevel fatal "
        " -codec copy -y "
        " -metadata:s:v:0 rotate=0 ").

maybe_reset_metadata("", QueuePath, _Mime, _Context) ->
    {ok, QueuePath};
maybe_reset_metadata(_TransposeOption, QueuePath, Mime, Context) ->
    TmpFile = z_tempfile:new(z_media_identify:extension(Mime)),
    FfmpegCmd = unicode:characters_to_binary(
                    [io_lib:format(?CMDLINE_RESETMETA, [z_filelib:os_filename(unicode:characters_to_list(QueuePath))]),
                     " ",
                     z_filelib:os_filename(unicode:characters_to_list(TmpFile))
                    ]),
    RunOptions = #{
        timeout => ?FFMPEG_TIMEOUT,
        read => [QueuePath], write => [TmpFile]
    },
    case z_exec:run(ffmpeg, FfmpegCmd, RunOptions, Context) of
        {ok, Stdout} ->
            case filelib:file_size(TmpFile) of
                0 ->
                    ?LOG_WARNING(#{
                        in => zotonic_mod_video,
                        text => <<"Video convert error: (empty result file during metadata reset)">>,
                        result => error,
                        reason => convert,
                        command => FfmpegCmd,
                        stdout => Stdout,
                        filename => QueuePath
                    }),
                    {error, convert};
                _ ->
                    {ok, TmpFile}
            end;
        {error, Reason} ->
            ?LOG_WARNING(#{
                in => zotonic_mod_video,
                text => <<"Video convert error: (during metadata reset)">>,
                command => FfmpegCmd,
                result => error,
                reason => Reason,
                filename => QueuePath
            }),
            {error, Reason}
    end.
