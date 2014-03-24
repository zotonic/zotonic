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
        site,
        pickled_context
    }).

-define(BROKEN_IMAGE, "images/broken.png").

start_link({convert_v1, _Id, _Medium, _Upload, QueueFilename, _PickledContext} = Args, Context) ->
    gen_server:start_link({via, gproc, {n,l,{video_convert, QueueFilename}}}, 
                          ?MODULE, 
                          [Args, z_context:site(Context)],
                          []).

init([{convert_v1, Id, Medium, Upload, QueueFilename, PickledContext}, Site]) ->
    gen_server:cast(self(), convert),
    {ok, #state{
        id = Id,
        medium = Medium,
        upload = Upload,
        queue_filename = QueueFilename,
        site = Site,
        pickled_context = PickledContext
    }}.

handle_call(_Msg, _From, State) ->
    {reply, {error, uknown_msg}, State}.

handle_cast(convert, State) ->
    QueuePath = mod_video:queue_path(State#state.queue_filename, z_context:depickle(State#state.pickled_context)),
    case filelib:is_regular(QueuePath) of
        true ->
            do_convert(QueuePath, State),
            file:delete(QueuePath),
            remove_task(State);
        false ->
            % Queue file was deleted, remove our task
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
    end.

insert_movie(Filename, State) ->
    Context = z_context:depickle(State#state.pickled_context),
    OrgFile = original_filename(State#state.upload),
    m_media:replace_file(#upload{filename=OrgFile, tmpfile=Filename}, State#state.id, [], [no_touch], Context).

original_filename(#media_upload_preprocess{original_filename=undefined}) ->
    "movie.mp4";
original_filename(#media_upload_preprocess{original_filename=OrgFile}) ->
    z_string:to_rootname(OrgFile) ++ ".mp4".    

insert_broken(State) ->
    Context = z_context:depickle(State#state.pickled_context),
    case z_module_indexer:find(lib, ?BROKEN_IMAGE, Context) of
        {ok, #module_index{filepath=Filename}} ->
            m_media:replace_file(Filename, State#state.id, [], [no_touch], Context);
        {error, enoent} ->
            lager:warning("No broken image for failed ffmpeg converts: ~p", [?BROKEN_IMAGE])
    end.

remove_task(State) ->
    Context = z_context:new(State#state.site),
    mod_video:remove_task(State#state.queue_filename, Context).

video_convert(QueuePath, Mime) ->
    Info = mod_video:video_info(QueuePath),
    video_convert_1(QueuePath, proplists:get_value(orientation, Info), Mime).

video_convert_1(QueuePath, 1, "video/mp4") ->
    {ok, QueuePath};
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
