%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-07-15
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

-mod_title("Video embed").
-mod_description("Embed youtube, vimeo and other movies as media pages.").
-mod_prio(600).
-mod_depends([admin]).
-mod_provides([video_embed]).

%% interface functions
-export([
    observe_rsc_update/3,
    observe_media_viewer/2,
    observe_media_stillimage/2,
    event/2
]).

-include_lib("zotonic.hrl").


%% Fantasy mime type to distinguish embeddable html fragments.
-define(EMBED_MIME, <<"text/html-video-embed">>).

%% @doc Check if the update contains video embed information.  If so then update the attached medium item.
%% @spec observe_rsc_update({rsc_update, ResourceId, OldResourceProps}, {Changed, UpdateProps}, Context) -> {NewChanged, NewUpdateProps}
observe_rsc_update(#rsc_update{id=Id}, {Changed, Props}, Context) ->
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
                            ok = m_media:replace(Id, MediaProps, Context),
                            preview_create(Id, MediaProps, Context),
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
                                    preview_create(Id, MediaProps, Context),
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
%% @spec observe_media_viewer(Notification, Context) -> undefined | {ok, Html}
observe_media_viewer(#media_viewer{props=Props}, _Context) ->
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
%% @spec observe_media_stillimage(Notification, _Context) -> undefined | {ok, Filename}
observe_media_stillimage(#media_stillimage{id=Id, props=Props}, Context) ->
    case proplists:get_value(mime, Props) of
        ?EMBED_MIME ->
            case m_rsc:p(Id, depiction, Context) of
                undefined ->
                    case z_convert:to_list(proplists:get_value(preview_filename, Props)) of
                        [] ->
                            case proplists:get_value(video_embed_service, Props) of
                                <<"youtube">> -> {ok, "lib/images/youtube.jpg"};
                                <<"vimeo">> -> {ok, "lib/images/vimeo.jpg"};
                                <<"yandex">> -> {ok, "lib/images/yandex.jpg"};
                                _ -> {ok, "lib/images/embed.jpg"}
                            end;
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


%% @doc Handle the form submit from the "new media" dialog.  The form is defined in templates/_media_upload_panel.tpl.
%% @spec event(Event, Context1) -> Context2
event(#submit{message={add_video_embed, EventProps}}, Context) ->
    Actions = proplists:get_value(actions, EventProps, []),
    Id = proplists:get_value(id, EventProps),
    Stay = z_convert:to_bool(proplists:get_value(stay, EventProps, false)),
    EmbedService = z_context:get_q("video_embed_service", Context),
    EmbedCode = z_context:get_q_validated("video_embed_code", Context),
    Callback = proplists:get_value("callback", EventProps), 

    case Id of
        %% Create a new page
        undefined ->
            SubjectId = proplists:get_value(subject_id, EventProps),
            Predicate = proplists:get_value(predicate, EventProps, depiction),
            Title   = z_context:get_q_validated("title", Context),
            Props = [
                {title, Title},
                {is_published, true},
                {category, video},
                {video_embed_service, EmbedService},
                {video_embed_code, EmbedCode}
            ],

            case m_rsc:insert(Props, Context) of
                {ok, MediaId} ->
                    spawn(fun() -> preview_create(MediaId, Props, Context) end),

                    {_, ContextLink} = mod_admin:do_link(z_convert:to_integer(SubjectId), Predicate, 
                                                         MediaId, Callback, Context),

                    ContextRedirect = case SubjectId of
                        undefined ->
                            case Stay of
                                false -> z_render:wire({redirect, [{dispatch, "admin_edit_rsc"}, {id, MediaId}]}, ContextLink);
                                true -> ContextLink
                            end;
                        _ -> ContextLink
                    end,
                    z_render:wire([
                        {dialog_close, []},
                        {growl, [{text, "Made the media page."}]}
                        | Actions], ContextRedirect);
                {error, _} = Error ->
                    ?ERROR("~p", [Error]),
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
%% support functions
%%====================================================================

%% Fetch or create a preview for the movie
preview_create(MediaId, InsertProps, Context) ->
    case z_convert:to_list(proplists:get_value(video_embed_service, InsertProps)) of
        "youtube" -> 
            spawn(fun() -> preview_youtube(MediaId, InsertProps, z_context:prune_for_async(Context)) end);
        "vimeo" -> 
            spawn(fun() -> preview_vimeo(MediaId, InsertProps, z_context:prune_for_async(Context)) end);
        "yandex" -> 
            spawn(fun() -> preview_yandex(MediaId, InsertProps, z_context:prune_for_async(Context)) end);
        _ -> nop
    end.

% @doc Fetch the preview image of a youtube video. The preview is located at: http://img.youtube.com/vi/[code]/0.jpg
% @todo Make this more robust wrt http errors.
preview_youtube(MediaId, InsertProps, Context) ->
    case z_convert:to_list(proplists:get_value(video_embed_code, InsertProps)) of
        [] -> nop;
        Embed ->
            case re:run(Embed, "youtube(\-nocookie)?\\.com/(v|embed)/([^\?\"'&]+)", [{capture,[3],list}]) of
                {match, [Code]} ->
                    Url = "http://img.youtube.com/vi/"++Code++"/0.jpg",
                    case httpc:request(Url) of
                        {ok, {{_,200,_}, _Header, Data}} ->
                            %% Received the preview image, move it to a file.
                            m_media:save_preview(MediaId, Data, "image/jpeg", Context);
                        {error, _Reason} ->
                            %% Too bad - no preview available - ignore for now (see todo above)
                            nop
                    end;
                _ ->
                    nop
            end
    end.


% @doc Fetch the preview image of a vimeo video. http://stackoverflow.com/questions/1361149/get-img-thumbnails-from-vimeo
% @todo Make this more robust wrt http errors.
preview_vimeo(MediaId, InsertProps, Context) ->
    case z_convert:to_list(proplists:get_value(video_embed_code, InsertProps)) of
        [] -> nop;
        Embed ->
            case re:run(Embed, "clip_id=([0-9]+)", [{capture,[1],list}]) of
                {match, [Code]} ->
                    JsonUrl = "http://vimeo.com/api/v2/video/" ++ Code ++ ".json",
                    case httpc:request(JsonUrl) of
                        {ok, {_StatusLine, _Header, Data}} ->
                            {array, [{struct, Props}]} = mochijson:decode(Data),
                            case proplists:get_value("thumbnail_large", Props) of
                                undefined ->
                                    nop;
                                ImgUrl ->
                                    case httpc:request(ImgUrl) of
                                        {ok, {_StatusLine1, _Header1, ImgData}} ->
                                            %% Received the preview image, move it to a file.
                                            m_media:save_preview(MediaId, ImgData, "image/jpeg", Context);
                                        {error, _Reason} ->
                                            %% Error retrieving preview
                                            nop
                                    end
                            end;
                        {error, _Reason} ->
                            %% Too bad - no preview available - ignore for now (see todo above)
                            nop
                    end;
                _ ->
                    nop
            end
    end.


%% @doc Fetch the preview image of a yandex video. The preview is located at:
%% http://static.video.yandex.ru/get/[user]/[code]/1.m450x334.jpg
%% @todo Make this more robust wrt http errors.
preview_yandex(MediaId, InsertProps, Context) ->
    case z_convert:to_list(proplists:get_value(video_embed_code, InsertProps)) of
        [] -> nop;
        Embed ->
            case re:run(Embed, "flv\\.video\\.yandex\\.ru/lite/([^/]+)/([^\"'&/]+)", [{capture, [1, 2], list}]) of
                {match, [User, Code]} ->
                    Url = lists:flatten(["http://static.video.yandex.ru/get/", User, $/, Code, "/1.m450x334.jpg"]),
                    case httpc:request(Url) of
                        {ok, {_StatusLine, _Header, Data}} ->
                            %% Received the preview image, move it to a file.
                            m_media:save_preview(MediaId, Data, "image/jpeg", Context);
                        {error, _Reason} ->
                            %% Too bad - no preview available - ignore for now (see todo above)
                            nop
                    end;
                _ ->
                    nop
            end
    end.
