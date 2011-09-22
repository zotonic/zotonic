%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-09-14
%% @doc Enables embedding media from their URL.

%% Copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
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

-module(mod_oembed).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-mod_title("OEmbed support").
-mod_description("Add external media in your site by their URL.").
-mod_prio(600).

%% interface functions
-export([
    init/1,
    observe_rsc_update/3,
    observe_media_viewer/2,
    observe_media_stillimage/2,
    event/2
]).

-include_lib("zotonic.hrl").
-include_lib("include/oembed.hrl").

%% Fantasy mime type to distinguish embeddable html fragments.
-define(OEMBED_MIME, <<"text/html-oembed">>).

%% @doc Start the oembed client.
init(Context) ->
    oembed_client:start_link(oembed_providers:list(Context)),
    ok.


%% @doc Check if the update contains video embed information.  If so
%% then try to get the oembed information from the provider and update
%% the attached medium item.
%% @spec observe_rsc_update({rsc_update, ResourceId, OldResourceProps}, {Changed, UpdateProps}, Context) -> {NewChanged, NewUpdateProps}
observe_rsc_update({rsc_update, Id, _OldProps}, {Changed, Props}, Context) ->
    case proplists:is_defined(oembed_url, Props) of
        true -> 
            EmbedChanged = case proplists:get_value(oembed_url, Props) of
                Empty when Empty == undefined; Empty == <<>>; Empty == [] ->
                    % Delete the media record iff the media mime type is our mime type
                    case m_media:identify(Id, Context) of
                        {ok, Props} ->
                            case proplists:get_value(mime, Props) of
                                ?OEMBED_MIME -> 
                                    m_media:delete(Id, Context),
                                    true;
                                _ -> 
                                    false
                            end;
                        _ ->
                            false
                    end;
                EmbedUrl ->
                    MediaProps = [
                        {mime, ?OEMBED_MIME},
                        {oembed_url, EmbedUrl}
                    ],

                    case m_media:get(Id, Context) of
                        undefined ->
                            ok = m_media:replace(Id, MediaProps, Context),
                            preview_create(Id, MediaProps, Context),
                            true;
                        OldMediaProps ->
                            case        z_utils:are_equal(proplists:get_value(mime, OldMediaProps), ?OEMBED_MIME)
                                andalso z_utils:are_equal(proplists:get_value(oembed_url, OldMediaProps), EmbedUrl) of
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

            Props1 = proplists:delete(oembed_url, Props),
            {Changed or EmbedChanged, Props1};
        false ->
            {Changed, Props}
    end.


%% @doc Return the media viewer for the embedded video (that is, when
%% it is an embedded media). First tries the template called
%% <tt>_oembed_embeddable_(providername).tpl</tt>; if not found, falls back to
%% the HTML code that the oembed provider gave us; if none found,
%% falls back to the generic template <tt>_oembed_embeddable.tpl</tt>.
%% @spec observe_media_viewer(Notification, Context) -> undefined | {ok, Html}
observe_media_viewer({media_viewer, Id, Props, Filename, Options}, Context) ->
    case proplists:get_value(mime, Props) of
        ?OEMBED_MIME ->
            TplOpts = [{id, Id}, {medium, Props}, {options, Options}, {filename, Filename}],
            {oembed, OEmbed} = proplists:lookup(oembed, Props),
            Html = case proplists:lookup(provider_name, OEmbed) of
                       {provider_name, N} ->
                           Tpl = "_oembed_embeddable_" ++ z_string:to_lower(z_convert:to_list(N)) ++ ".tpl",
                           case z_template:find_template(Tpl, Context) of
                               {ok, _} ->
                                   z_template:render(Tpl, TplOpts, Context);
                               {error, _} ->
                                   media_viewer_fallback(OEmbed, TplOpts, Context)
                           end;
                       none ->
                           media_viewer_fallback(OEmbed, TplOpts, Context)
                   end,
            {ok, Html};
        _ ->
            undefined
    end.

media_viewer_fallback(OEmbed, TplOpts, Context) ->
    case proplists:lookup(html, OEmbed) of
        {html, Html} ->
            Html;
        none ->
            z_template:render("_oembed_embeddable.tpl", TplOpts, Context)
    end.

%% @doc Return the filename of a still image to be used for image tags.
%% @spec observe_media_stillimage(Notification, _Context) -> undefined | {ok, Filename}
observe_media_stillimage({media_stillimage, Id, Props}, Context) ->
    case proplists:get_value(mime, Props) of
        ?OEMBED_MIME ->
            case m_rsc:p(Id, depiction, Context) of
                undefined ->
                    case z_convert:to_list(proplists:get_value(preview_filename, Props)) of
                        [] ->
                            {ok, "lib/images/embed.jpg"};
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
event({submit, {add_video_embed, EventProps}, _TriggerId, _TargetId}, Context) ->
    Actions = proplists:get_value(actions, EventProps, []),
    Id = proplists:get_value(id, EventProps),
    Stay = z_convert:to_bool(proplists:get_value(stay, EventProps, false)),
    EmbedUrl = z_context:get_q_validated("oembed_url", Context),

    case Id of
        %% Create a new page
        undefined ->
            SubjectId = proplists:get_value(subject_id, EventProps),
            Predicate = proplists:get_value(predicate, EventProps, depiction),
            Title   = z_context:get_q_validated("title", Context),
            Summary = z_context:get_q("summary", Context),
            Props = [
                {title, Title},
                {summary, Summary},
                {is_published, true},
                {category, media},
                {mime, ?OEMBED_MIME},
                {oembed_url, EmbedUrl}
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
                    spawn(fun() -> preview_create(MediaId, Props, Context) end),
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
                {oembed_url, EmbedUrl}
            ],
            case m_rsc:update(Id, Props, Context) of
                {ok, _} ->
                    z_render:wire([{dialog_close, []} | Actions], Context);
                {error, _} ->
                    z_render:growl_error("Could not update the page with the new embed code.", Context)
            end
    end;

%% @doc When entering the embed URL for a new media item, we trigger the detecting early to guess title/description.
event({postback, {do_oembed, []}, _TriggerId, _TargetId}, Context) ->
    Url = z_context:get_q(triggervalue, Context),
    case oembed_request(Url, Context) of
        error ->
            z_render:growl_error(?__("Invalid media URL", Context), Context);
        Json ->
            %% Fill title
            z_context:add_script_page(["$('#oembed-title').val('", z_utils:js_escape(proplists:get_value(title, Json, [])), "');"], Context),
            %% And summary
            z_context:add_script_page(["$('#oembed-summary').val('", z_utils:js_escape(proplists:get_value(description, Json, [])), "');"], Context),
            z_render:growl("Detected media item", Context)
    end.


%%====================================================================
%% support functions
%%====================================================================

%% Fetch or create a preview for the movie
preview_create(MediaId, MediaProps, Context) ->
    case z_convert:to_list(proplists:get_value(oembed_url, MediaProps)) of
        [] -> ok;
        Url -> 
            Json = oembed_request(Url, Context),
            %% store found properties in the media part of the rsc
            ok = m_media:replace(MediaId, [{oembed, Json} | MediaProps], Context),
            Type = proplists:get_value(type, Json),
            case preview_url_from_json(Type, Json) of
                undefined -> nop;
                ThumbUrl ->
                    {CT, ImageData} = thumbnail_request(ThumbUrl, Context),
                    {ok, _} = m_media:save_preview(MediaId, ImageData, CT, Context),
                    %% move to correct category if rsc is a 'media'
                    case m_rsc:is_a(MediaId, media, Context) of
                        true -> m_rsc:update(MediaId, [{category, type_to_category(Type)}], Context);
                        false -> m_rsc:touch(MediaId, Context)
                    end
            end
    end.


%% @doc Perform OEmbed discovery on a given URL.
%% @spec oembed_request(string(), #context{}) -> [{Key, Value}]
oembed_request(Url, Context) ->
    F = fun() ->
                oembed_client:discover(Url)
        end,
    z_depcache:memo(F, {oembed, Url}, 3600, Context).



%% @doc Given a thumbnail URL, download it and return the content type plus image data pair.
thumbnail_request(ThumbUrl, Context) ->
    F = fun() ->
                {ok, {{_, 200, _}, Headers, ImageData}} = http:request(get, {z_convert:to_list(ThumbUrl), []}, [], []),
                CT = case proplists:lookup("content-type", Headers) of
                         {"content-type", C} -> C;
                         _ -> "image/jpeg"
                     end,
                {CT, ImageData}
        end,
    z_depcache:memo(F, {oembed_thumbnail, ThumbUrl}, 3600, Context).


%% @doc Get the preview URL from JSON structure. Either the thumbnail
%% URL for non-photo elements, or the full URL for photo elements.
preview_url_from_json(<<"photo">>, Json) ->
    proplists:get_value(url, Json);
preview_url_from_json(_Type, Json) ->
    proplists:get_value(thumbnail_url, Json).


type_to_category(<<"photo">>) -> image;
type_to_category(<<"video">>) -> video;
type_to_category(_) -> document.
