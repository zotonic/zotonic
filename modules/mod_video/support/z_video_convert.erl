%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Process for converting a video to mp4

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

-include_lib("zotonic.hrl").

-record(state, {
        id,
        medium,
        upload,
        queue_filename,
        process_nr,
        site,
        pickled_context
    }).

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
    {reply, {error, uknown_msg}, State}.

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
            lager:debug("Video conversion (startup): medium is not current or queue file missing (id ~p, file ~p)", [State#state.id, State#state.queue_filename]),
            remove_task(State)
    end,
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_convert(QueuePath, State) ->
    Upload = State#state.upload,
    case video_convert(QueuePath, Upload#media_upload_preprocess.mime) of
        {ok, TmpFile} ->
            insert_movie(TmpFile, State);
        Error ->
            lager:warning("ffmpeg conversion error on ~p: ~p", [State#state.id, Error]),
            insert_broken(State)
    end,
    Context = z_context:depickle(State#state.pickled_context),
    mod_signal:emit({medium_update, [{id,State#state.id}]}, Context).

insert_movie(Filename, State) ->
    Context = z_context:depickle(State#state.pickled_context),
    case is_current_upload(State, Context) of
        true ->
            OrgFile = original_filename(State#state.upload),
            PropsMedia = [
                {is_video_ok, true}
            ],
            m_media:replace_file(#upload{filename=OrgFile, tmpfile=Filename}, State#state.id, [], PropsMedia, [no_touch], Context);
        false ->
            lager:debug("Video conversion (ok): medium is not current anymore (id ~p)", [State#state.id])
    end.

original_filename(#media_upload_preprocess{original_filename=undefined}) ->
    "movie.mp4";
original_filename(#media_upload_preprocess{original_filename=OrgFile}) ->
    z_string:to_rootname(OrgFile) ++ ".mp4".    

insert_broken(State) ->
    Context = z_context:depickle(State#state.pickled_context),
    case is_current_upload(State, Context) of
        true ->
            PropsMedia = [
                {mime, "video/x-mp4-broken"}
            ],
            m_media:replace_file(undefined, State#state.id, [], PropsMedia, [no_touch], Context);
        false ->
            lager:debug("Video conversion (broken): medium is not current anymore (id ~p)", [State#state.id])
    end.

is_current_upload(State, Context) ->
    case m_rsc:exists(State#state.id, Context) of
        true ->
            case m_media:get(State#state.id, Context) of
                undefined ->
                    false;
                Props ->
                    proplists:get_value(video_processing_nr, Props) =:= State#state.process_nr
            end;
        false ->
            false
    end.

remove_task(State) ->
    Context = z_context:new(State#state.site),
    mod_video:remove_task(State#state.queue_filename, Context).

video_convert(QueuePath, Mime) ->
    Info = mod_video:video_info(QueuePath),
    video_convert_1(QueuePath, proplists:get_value(orientation, Info), Mime).

video_convert_1(QueuePath, Orientation, _Mime) ->
    TmpFile = z_tempfile:new(),
    FfmpegCmd = lists:flatten([
            "ffmpeg -i ", z_utils:os_filename(QueuePath),
            " -vcodec libx264 ",
            " -loglevel fatal ",
            " -f mp4 ",
            " -strict -2 ",
            " -y ",
            " -movflags +faststart ",
            " -preset medium ",
            " -metadata:s:v:0 rotate=0 ",
            mod_video:orientation_to_transpose(Orientation),
            z_utils:os_filename(TmpFile) 
        ]),
    jobs:run(video_jobs,
            fun() ->
                lager:debug("Video convert: ~p", [FfmpegCmd]),
                case os:cmd(FfmpegCmd) of
                    [] ->
                        case filelib:file_size(TmpFile) of
                            0 ->
                                lager:warning("Video convert error: (empty result file)  [queue: ~p]", [QueuePath]),
                                {error, convert};
                            _ ->
                                {ok, TmpFile}
                        end; 
                    Other ->
                        lager:warning("Video convert error: ~p [queue: ~p]", [Other, QueuePath]),
                        {error, Other}
                end
            end).
