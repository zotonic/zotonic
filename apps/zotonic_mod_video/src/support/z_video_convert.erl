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
          id,
          medium,
          upload,
          queue_filename,
          process_nr,
          site,
          pickled_context
         }).

% Default timeout for video conversions - 6 hours
-define(FFMPEG_TIMEOUT, 6*3600*1000).

start_link({convert_v1, _Id, _Medium, _Upload, _QueueFilename, _PickledContext}, _Context) ->
                                                % Flush old convert queue
    ok;
start_link({convert_v2, _Id, _Medium, _Upload, QueueFilename, _ProcessNr, _PickledContext} = Args, Context) ->
    gen_server:start_link({via, z_proc, {video_convert, z_convert:to_binary(QueueFilename)}},
                          ?MODULE,
                          [Args, z_context:site(Context)],
                          []).

init([{convert_v2, Id, Medium, Upload, QueueFilename, ProcessNr, PickledContext}, Site]) ->
    gen_server:cast(self(), convert),
    {ok, #state{
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
    QueuePath = mod_video:queue_path(State#state.queue_filename, Context),
    case is_current_upload(State, Context) andalso filelib:is_regular(QueuePath) of
        true ->
            do_convert(QueuePath, State),
            file:delete(QueuePath),
            remove_task(State);
        false ->
            % Queue file was deleted, remove our task
            ?LOG_NOTICE(#{
                text => <<"Video conversion (startup): medium is not current or queue file missing">>,
                in => zotonic_mod_video,
                rsc_id => State#state.id,
                filename => State#state.queue_filename
            }),
            remove_task(State)
    end,
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec do_convert(file:filename_all(), #state{}) -> ok.
do_convert(QueuePath, State) ->
    Upload = State#state.upload,
    case video_convert(QueuePath, Upload#media_upload_preprocess.mime) of
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
            insert_broken(State)
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
                [no_touch],
                Context);
        false ->
            ?LOG_INFO(#{
                text => <<"Video conversion (ok): medium is not current anymore">>,
                in => zotonic_mod_video,
                rsc_id => State#state.id
            })
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
                <<"mime">> => <<"video/x-mp4-broken">>
            },
            m_media:replace_file(undefined, State#state.id, #{}, PropsMedia, [no_touch], Context);
        false ->
            ?LOG_NOTICE(#{
                text => <<"Video conversion (broken): medium is not current anymore">>,
                in => zotonic_mod_video,
                rsc_id => State#state.id
            })
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
    mod_video:remove_task(State#state.queue_filename, Context).

video_convert(QueuePath, Mime) ->
    Info = z_video_info:info(QueuePath),
    video_convert_1(QueuePath, maps:get(<<"orientation">>, Info, 1), Mime).

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

-spec video_convert_1(file:filename_all(), integer(), string() | binary()) ->
    {ok, file:filename_all()} | term().
video_convert_1(QueuePath, Orientation, Mime) ->
    Cmdline = case z_config:get(ffmpeg_cmdline) of
                  undefined -> ?CMDLINE;
                  <<>> -> ?CMDLINE;
                  "" -> ?CMDLINE;
                  CmdLineCfg -> z_convert:to_list(CmdLineCfg)
              end,
    jobs:run(video_jobs,
             fun() ->
                TransposeOption = z_video_preview:orientation_to_transpose(Orientation),
                case maybe_reset_metadata(TransposeOption, QueuePath, Mime) of
                    {ok, QueuePath1} ->
                        TmpFile = z_tempfile:new(),
                        FfmpegCmd = unicode:characters_to_binary(
                                        [io_lib:format(Cmdline, [z_filelib:os_filename(QueuePath1)]),
                                         " ",
                                         TransposeOption,
                                         " ",
                                         z_filelib:os_filename(TmpFile)
                                        ]),
                        ?LOG_INFO(#{
                            in => zotonic_mod_video,
                            text => <<"Video convert starting">>,
                            command => FfmpegCmd,
                            filename => unicode:characters_to_binary(QueuePath)
                        }),
                        RunOptions = #{
                            timeout => ?FFMPEG_TIMEOUT
                        },
                        StartTimestamp = z_datetime:timestamp(),
                        case z_exec:run(FfmpegCmd, RunOptions) of
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
        " -codec copy "
        " -metadata:s:v:0 rotate=0 ").

maybe_reset_metadata("", QueuePath, _Mime) ->
    {ok, QueuePath};
maybe_reset_metadata(_TransposeOption, QueuePath, Mime) ->
    TmpFile = z_tempfile:new(z_media_identify:extension(Mime)),
    FfmpegCmd = unicode:characters_to_binary(
                    [io_lib:format(?CMDLINE_RESETMETA, [z_filelib:os_filename(QueuePath)]),
                     " ",
                     z_filelib:os_filename(TmpFile)
                    ]),
    RunOptions = #{
        timeout => ?FFMPEG_TIMEOUT
    },
    case z_exec:run(FfmpegCmd, RunOptions) of
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
