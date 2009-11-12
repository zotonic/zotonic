%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-15
%% @doc Enables embedding video's as media pages.  Handles the embed information for showing video's.
%% The embed information is stored in the medium table associated with the page. You can not have embed
%% information and a medium file. Either one or the other.

%% Copyright 2009 Marc Worrell
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

-module(mod_video_embed).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Video embed").
-mod_description("Embed youtube, vimeo and other movies as media pages.").
-mod_prio(600).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    rsc_update/3,
    media_viewer/2,
    media_stillimage/2,
    event/2
]).

-include_lib("zotonic.hrl").

-record(state, {context}).

%% @doc Fantasy mime type to distinguish embeddable html fragments.
-define(EMBED_MIME, <<"text/html-video-embed">>).

%% @doc Check if the update contains video embed information.  If so then update the attached medium item.
%% @spec rsc_update({rsc_update, ResourceId, OldResourceProps}, {Changed, UpdateProps}, Context) -> {NewChanged, NewUpdateProps}
rsc_update({rsc_update, Id, _OldProps}, {Changed, Props}, Context) ->
    case proplists:is_defined(video_embed_code, Props) of
        true -> 
            EmbedChanged = case proplists:get_value(video_embed_code, Props) of
                Empty when Empty == undefined; Empty == <<>>; Empty == [] ->
                    % Delete the media record iff the media mime type is our mime type
                    case m_media:identify(Id, Context) of
                        {ok, Props} ->
                            case proplists:get_value(mime, Props) of
                                ?EMBED_MIME -> 
                                    m_media:delete(Id, Context),
                                    true;
                                _ -> 
                                    false
                            end;
                        _ ->
                            false
                    end;
                EmbedCode ->
                    EmbedCodeRaw = z_html:unescape(EmbedCode),
                    EmbedService = proplists:get_value(video_embed_service, Props, ""),
                    MediaProps = [
                        {mime, ?EMBED_MIME},
                        {video_embed_code, EmbedCodeRaw},
                        {video_embed_service, EmbedService}
                    ],

                    case m_media:get(Id, Context) of
                        undefined ->
                            %% Will change
                            ok = m_media:replace(Id, MediaProps, Context),
                            true;
                        OldMediaProps ->
                            case        z_utils:are_equal(proplists:get_value(mime, OldMediaProps), ?EMBED_MIME)
                                andalso z_utils:are_equal(proplists:get_value(video_embed_code, OldMediaProps), EmbedCodeRaw)
                                andalso z_utils:are_equal(proplists:get_value(video_embed_service, OldMediaProps), EmbedService) of
                            
                                true ->
                                    %% Not changed
                                    false; 
                                false -> 
                                    %% Changed, update the medium record
                                    ok = m_media:replace(Id, MediaProps, Context),
                                    true
                            end
                    end
            end,

            Props1 = proplists:delete(video_embed_code, 
                        proplists:delete(video_embed_service, Props)),
            {Changed or EmbedChanged, Props1};
        false ->
            {Changed, Props}
    end.


%% @doc Return the media viewer for the embedded video (that is, when it is an embedded media).
%% @spec media_viewer(Notification, Context) -> undefined | {ok, Html}
media_viewer({media_viewer, Props, _Filename, _Options}, _Context) ->
    case proplists:get_value(mime, Props) of
        ?EMBED_MIME ->
            case proplists:get_value(video_embed_code, Props) of
                undefined -> undefined;
                EmbedCode -> {ok, EmbedCode}
            end;
        _ ->
            undefined
    end.


%% @doc Return the filename of a still image to be used for image tags.
%% @spec media_stillimage(Notification, _Context) -> undefined | {ok, Html}
media_stillimage({media_stillimage, Props}, _Context) ->
    case proplists:get_value(mime, Props) of
        ?EMBED_MIME ->
            case proplists:get_value(video_embed_service, Props) of
                <<"youtube">> -> {ok, "lib/images/youtube.jpg"};
                <<"vimeo">> -> {ok, "lib/images/vimeo.jpg"};
                _ -> {ok, "lib/images/embed.jpg"}
            end;
        _ ->
            undefined
    end.


%% @doc Handle the form submit from the "new media" dialog.  The form is defined in templates/_media_upload_panel.tpl.
%% @spec event(Event, Context1) -> Context2
event({submit, {add_video_embed, EventProps}, _TriggerId, _TargetId}, Context) ->
    Actions = proplists:get_value(actions, EventProps, []),
    Id = proplists:get_value(id, EventProps),
    Stay = z_convert:to_bool(proplists:get_value(stay, EventProps, false)),
    EmbedService = z_context:get_q("video_embed_service", Context),
    EmbedCode = z_context:get_q_validated("video_embed_code", Context),

    case Id of
        %% Create a new page
        undefined ->
            SubjectId = proplists:get_value(subject_id, EventProps),
            Predicate = proplists:get_value(predicate, EventProps, depiction),
            Title   = z_context:get_q_validated("title", Context),
            GroupId = list_to_integer(z_context:get_q("group_id", Context)),

            Props = [
                {title, Title},
                {category, video},
                {group_id, GroupId},
                {video_embed_service, EmbedService},
                {video_embed_code, EmbedCode}
            ],

            F = fun(Ctx) ->
                case m_rsc:insert(Props, Context) of
                    {ok, MediaRscId} ->
                        case SubjectId of
                            undefined -> nop;
                            _ -> m_edge:insert(SubjectId, Predicate, MediaRscId, Ctx)
                        end,
                        {ok, MediaRscId};
                    {error, Error} ->
                        throw({error, Error})
                end 
            end,
    
            case z_db:transaction(F, Context) of
                {ok, MediaId} ->
                    ContextRedirect = case SubjectId of
                        undefined ->
                            case Stay of
                                false -> z_render:wire({redirect, [{dispatch, "admin_edit_rsc"}, {id, MediaId}]}, Context);
                                true -> Context
                            end;
                        _ -> Context
                    end,
                    z_render:wire([{dialog_close, []}, {growl, [{text, "Made the media page."}]} | Actions], ContextRedirect);
                {rollback, {_Error, _Trace}} ->
                    ?ERROR("~p~n~p", [_Error, _Trace]),
                    z_render:growl_error("Could not create the media page.", Context)
            end;
        
        %% Update the current page
        N when is_integer(N) ->
            Props = [
                {category, video},
                {video_embed_service, EmbedService},
                {video_embed_code, EmbedCode}
            ],
            case m_rsc:update(Id, Props, Context) of
                {ok, _} ->
                    z_render:wire([{dialog_close, []} | Actions], Context);
                {error, _} ->
                    z_render:growl_error("Could not update the page with the new embed code.", Context)
            end
    end.
    


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    z_notifier:observe(rsc_update, {?MODULE, rsc_update}, Context),
    z_notifier:observe(media_viewer, {?MODULE, media_viewer}, Context),
    z_notifier:observe(media_stillimage, {?MODULE, media_stillimage}, Context),
    {ok, #state{context=z_context:new(Context)}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    z_notifier:detach(rsc_update, {?MODULE, rsc_update}, State#state.context),
    z_notifier:detach(media_viewer, {?MODULE, media_viewer}, State#state.context),
    z_notifier:detach(media_stillimage, {?MODULE, media_stillimage}, State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

