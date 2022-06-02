%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%% @doc Enables embedding video's as media pages.  Handles the embed information for showing video's.
%% The embed information is stored in the medium table associated with the page. You can not have embed
%% information and a medium file. Either one or the other.

%% Copyright 2009-2017 Marc Worrell
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
-export([observe_rsc_update/3,
         observe_media_viewer/2,
         observe_media_stillimage/2,
         observe_media_import/2,
         observe_media_import_medium/2,
         event/2,
         spawn_preview_create/3]).
-export([test/0,
         videoid_to_image/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% Fantasy mime type to distinguish embeddable html fragments.
-define(EMBED_MIME, <<"text/html-video-embed">>).

%% @doc Check if the update contains video embed information.  If so then update the attached medium item.
-spec observe_rsc_update(#rsc_update{  }, {ok, m_rsc:props()} | {error, term()}, z:context()) ->
                            {ok, m_rsc:props()} | {error, term()}.
observe_rsc_update(#rsc_update{
                       action = insert,
                       id = Id
                   },
                   {ok, Props},
                   Context) ->
    case maps:get(<<"video_embed_code">>, Props, undefined) of
        undefined ->
            {ok, Props};
        "" ->
            {ok, maps:remove(<<"video_embed_code">>, Props)};
        <<>> ->
            {ok, maps:remove(<<"video_embed_code">>, Props)};
        EmbedCodeRaw ->
            case z_acl:is_allowed(insert, #acl_media{ mime = ?EMBED_MIME }, Context) of
                true ->
                    EmbedCode =
                        z_sanitize:html(
                            z_html:unescape(EmbedCodeRaw), Context),
                    EmbedService =
                        z_convert:to_binary(
                            maps:get(<<"video_embed_service">>, Props, <<>>)),
                    {EmbedService1, EmbedId} = fetch_videoid_from_embed(EmbedService, EmbedCode),
                    MediaProps =
                        #{
                            <<"mime">> => ?EMBED_MIME,
                            <<"video_embed_code">> => EmbedCode,
                            <<"video_embed_service">> => EmbedService1,
                            <<"video_embed_id">> => EmbedId
                        },
                    ok = m_media:replace(Id, MediaProps, Context),
                    spawn_preview_create(Id, MediaProps, Context);
                false ->
                    ?LOG_NOTICE("Denied user ~p to embed ~p: ~p", [z_acl:user(Context), ?EMBED_MIME, EmbedCodeRaw]),
                    ok
            end,
            Props1 = maps:remove(<<"video_embed_code">>, maps:remove(<<"video_embed_service">>, Props)),
            {ok, Props1}
    end;
observe_rsc_update(#rsc_update{
                       action = update,
                       id = Id
                   },
                   {ok, #{ <<"video_embed_code">> := EmbedCodeRaw } = Props},
                   Context) ->
    OldMediaProps = m_media:get(Id, Context),
    case EmbedCodeRaw of
        Empty when Empty =:= undefined; Empty =:= <<>>; Empty =:= "" ->
            % Delete the media record iff the media mime type is our mime type
            case OldMediaProps of
                #{ <<"mime">> := ?EMBED_MIME } ->
                    m_media:delete(Id, Context);
                _ ->
                    ok
            end;
        _ ->
            EmbedCode =
                z_sanitize:html(
                    z_html:unescape(EmbedCodeRaw), Context),
            EmbedService = maps:get(<<"video_embed_service">>, Props, <<>>),
            {EmbedService1, EmbedId} = fetch_videoid_from_embed(EmbedService, EmbedCode),
            MediaProps =
                #{
                    <<"mime">> => ?EMBED_MIME,
                    <<"video_embed_code">> => EmbedCode,
                    <<"video_embed_service">> => EmbedService1,
                    <<"video_embed_id">> => EmbedId
                },
            case OldMediaProps of
                #{ <<"mime">> := ?EMBED_MIME } ->
                    case z_utils:are_equal(
                             maps:get(<<"video_embed_code">>, OldMediaProps, undefined), EmbedCode)
                         andalso z_utils:are_equal(
                                     maps:get(<<"video_embed_service">>, OldMediaProps, undefined), EmbedService)
                    of
                        true ->
                            ok;
                        false ->
                            ok = m_media:replace(Id, MediaProps, Context),
                            spawn_preview_create(Id, MediaProps, Context)
                    end;
                _ ->
                    ok = m_media:replace(Id, MediaProps, Context),
                    spawn_preview_create(Id, MediaProps, Context)
            end
    end,
    Props1 = maps:remove(<<"video_embed_code">>, maps:remove(<<"video_embed_service">>, Props)),
    {ok, Props1};
observe_rsc_update(#rsc_update{  }, {ok, Props}, _Context) ->
    {ok, Props};
observe_rsc_update(#rsc_update{  }, {error, _} = Error, _Context) ->
    Error.

%% @doc Return the media viewer for the embedded video (that is, when it is an embedded media).
-spec observe_media_viewer(#media_viewer{  }, z:context()) -> undefined | {ok, template_compiler:render_result()}.
observe_media_viewer(#media_viewer{ props = #{ <<"mime">> := ?EMBED_MIME } = Props }, Context) ->
    case maps:get(<<"video_embed_code">>, Props, undefined) of
        EmbedCode when is_binary(EmbedCode) ->
            EmbedCode1 = binary:replace(EmbedCode, <<"http://">>, <<"https://">>, [global]),
            IsIframe = binary:match(EmbedCode1, <<"<iframe">>) =/= nomatch,
            Vars = [{html, EmbedCode1}, {is_iframe, IsIframe}, {medium, Props}],
            Html = z_template:render("_video_embed.tpl", Vars, Context),
            {ok, Html};
        _ ->
            undefined
    end;
observe_media_viewer(#media_viewer{  }, _Context) ->
    undefined.

%% @doc Return the filename of a still image to be used for image tags.
-spec observe_media_stillimage(#media_stillimage{  }, z:context()) -> undefined | {ok, file:filename_all()}.
observe_media_stillimage(#media_stillimage{
                             id = _Id,
                             props = #{ <<"mime">> := ?EMBED_MIME } = Props
                         },
                         _Context) ->
    case maps:get(<<"preview_filename">>, Props, undefined) of
        PreviewFile when is_binary(PreviewFile), PreviewFile =/= <<>> ->
            {ok, PreviewFile};
        _ ->
            case maps:get(<<"video_embed_service">>, Props, undefined) of
                <<"youtube">> ->
                    {ok, <<"lib/images/youtube.jpg">>};
                <<"vimeo">> ->
                    {ok, <<"lib/images/vimeo.jpg">>};
                _ ->
                    {ok, <<"lib/images/embed.jpg">>}
            end
    end;
observe_media_stillimage(#media_stillimage{  }, _Context) ->
    undefined.

%% @doc Import a embedded medium for a rsc_import. Sanitize the provided html.
-spec observe_media_import_medium(#media_import_medium{  }, z:context()) -> undefined | ok.
observe_media_import_medium(#media_import_medium{
                                id = Id,
                                medium =
                                    #{
                                        <<"mime">> := ?EMBED_MIME,
                                        <<"video_embed_code">> := EmbedCode,
                                        <<"video_embed_service">> := EmbedService,
                                        <<"video_embed_id">> := EmbedId
                                    } =
                                        Medium
                            },
                            Context) ->
    MediaProps =
        #{
            <<"mime">> => ?EMBED_MIME,
            <<"video_embed_code">> => z_sanitize:html(EmbedCode, Context),
            <<"video_embed_service">> => z_string:to_name(EmbedService),
            <<"video_embed_id">> => EmbedId,
            <<"height">> => as_int(maps:get(<<"height">>, Medium, undefined)),
            <<"width">> => as_int(maps:get(<<"width">>, Medium, undefined)),
            <<"orientation">> => as_int(maps:get(<<"orientation">>, Medium, undefined)),
            <<"media_import">> =>
                z_sanitize:uri(
                    maps:get(<<"media_import">>, Medium, undefined))
        },
    OldMediaProps = m_media:get(Id, Context),
    case OldMediaProps of
        #{ <<"mime">> := ?EMBED_MIME } ->
            case z_utils:are_equal(
                     maps:get(<<"video_embed_code">>, OldMediaProps, undefined), EmbedCode)
                 andalso z_utils:are_equal(
                             maps:get(<<"video_embed_service">>, OldMediaProps, undefined), EmbedService)
            of
                true ->
                    ok;
                false ->
                    ok = m_media:replace(Id, MediaProps, Context),
                    spawn_preview_create(Id, MediaProps, Context)
            end;
        _ ->
            ok = m_media:replace(Id, MediaProps, Context),
            spawn_preview_create(Id, MediaProps, Context)
    end,
    ok;
observe_media_import_medium(#media_import_medium{  }, _Context) ->
    undefined.

as_int(undefined) ->
    undefined;
as_int(N) ->
    z_convert:to_integer(N).

%% @doc Recognize youtube and vimeo URLs, generate the correct embed code
observe_media_import(#media_import{
                         host_rev = [<<"com">>, <<"youtube">> | _],
                         metadata = MD
                     } =
                         MI,
                     Context) ->
    media_import(<<"youtube">>, ?__("Youtube Video", Context), MD, MI, Context);
observe_media_import(#media_import{
                         host_rev = [<<"com">>, <<"vimeo">> | _],
                         metadata = MD
                     } =
                         MI,
                     Context) ->
    media_import(<<"vimeo">>, ?__("Vimeo Video", Context), MD, MI, Context);
observe_media_import(#media_import{  }, _Context) ->
    undefined.

media_import(Service, Descr, MD, MI, Context) ->
    H = z_convert:to_integer(
            z_url_metadata:p([<<"og:video:height">>, <<"twitter:player:height">>], MD)),
    W = z_convert:to_integer(
            z_url_metadata:p([<<"og:video:width">>, <<"twitter:player:width">>], MD)),
    VideoId = fetch_videoid_from_url(Service, MI#media_import.url),
    case is_integer(H) andalso is_integer(W) andalso VideoId =/= <<>> of
        true ->
            VideoIdBin = z_convert:to_binary(VideoId),
            PreviewUrl = videoid_to_image(Service, VideoIdBin),
            [media_import_props_video(Service, Descr, MD, MI, H, W, VideoId, PreviewUrl, Context),
             media_import_props_image(MD, PreviewUrl, Context)];
        false ->
            undefined
    end.

media_import_props_video(Service, Descr, MD, MI, H, W, VideoId, PreviewUrl, Context) ->
    VideoIdBin = z_convert:to_binary(VideoId),
    PreviewUrl1 =
        case PreviewUrl of
            undefined ->
                z_url_metadata:p(image, MD);
            PU ->
                PU
        end,
    #media_import_props{
        prio = 1,
        category = video,
        module = ?MODULE,
        description = Descr,
        rsc_props =
            #{
                <<"title">> => z_url_metadata:p(title, MD),
                <<"summary">> => z_url_metadata:p(summary, MD),
                <<"website">> => MI#media_import.url
            },
        medium_props =
            #{
                <<"mime">> => ?EMBED_MIME,
                <<"width">> => W,
                <<"height">> => H,
                <<"video_embed_service">> => Service,
                <<"video_embed_code">> => embed_code(Service, H, W, VideoId, Context),
                <<"video_embed_id">> => VideoIdBin,
                <<"media_import">> => MI#media_import.url
            },
        preview_url = PreviewUrl1
    }.

media_import_props_image(_MD, undefined, _Context) ->
    undefined;
media_import_props_image(MD, PreviewUrl, Context) ->
    #media_import_props{
        prio = 10,
        category = image,
        description = m_rsc:p_no_acl(image, title, Context),
        rsc_props =
            #{
                <<"title">> => z_url_metadata:p(title, MD),
                <<"summary">> => z_url_metadata:p(summary, MD),
                <<"website">> => z_url_metadata:p(url, MD)
            },
        medium_props =
            #{
                <<"mime">> =>
                    z_convert:to_binary(
                        z_media_identify:guess_mime(PreviewUrl))
            },
        medium_url = z_convert:to_binary(PreviewUrl)
    }.

fetch_videoid_from_embed(_Service, undefined) ->
    {<<>>, undefined};
fetch_videoid_from_embed(_Service, <<>>) ->
    {<<>>, undefined};
fetch_videoid_from_embed(Service, EmbedCode) ->
    case re:run(EmbedCode, <<"(src|href)=\"([^\"]*)\"">>, [global, notempty, {capture, all, binary}]) of
        {match, [[_, _, Url] | _]} ->
            case url_to_service(Url) of
                undefined ->
                    {Service, <<>>};
                UrlService ->
                    {z_convert:to_binary(UrlService), fetch_videoid_from_url(UrlService, Url)}
            end;
        nomatch ->
            {Service, <<>>}
    end.

fetch_videoid_from_url(<<"youtube">>, Url) ->
    [Url1 | _] = binary:split(Url, <<"?">>),
    case binary:split(Url1, <<"/embed/">>) of
        [_, Code] ->
            Code;
        _ ->
            {_Protocol, _Host, _Path, Qs, _Hash} =
                mochiweb_util:urlsplit(
                    z_convert:to_list(Url)),
            Qs1 = mochiweb_util:parse_qs(Qs),
            z_convert:to_binary(
                proplists:get_value("v", Qs1))
    end;
fetch_videoid_from_url(<<"vimeo">>, Url) ->
    {_Protocol, _Host, Path, _Qs, _Hash} =
        mochiweb_util:urlsplit(
            z_convert:to_list(Url)),
    P1 = lists:last(
             string:tokens(Path, "/")),
    case z_utils:only_digits(P1) of
        true ->
            z_convert:to_binary(P1);
        false ->
            <<>>
    end;
fetch_videoid_from_url(_Service, _Url) ->
    <<>>.

url_to_service(<<"https://", Url/binary>>) ->
    url_to_service(Url);
url_to_service(<<"http://", Url/binary>>) ->
    url_to_service(Url);
url_to_service(<<"//", Url/binary>>) ->
    url_to_service(Url);
url_to_service(<<"www.youtube.com/", _/binary>>) ->
    <<"youtube">>;
url_to_service(<<"youtube.com/", _/binary>>) ->
    <<"youtube">>;
url_to_service(<<"www.vimeo.com/", _/binary>>) ->
    <<"vimeo">>;
url_to_service(<<"vimeo.com/", _/binary>>) ->
    <<"vimeo">>;
url_to_service(<<"player.vimeo.com/", _/binary>>) ->
    <<"vimeo">>;
url_to_service(_) ->
    undefined.

embed_code(<<"youtube">>, H, W, V, Context) ->
    iolist_to_binary([<<"<iframe width=\"">>,
                      integer_to_list(W),
                      <<"\" height=\"">>,
                      integer_to_list(H),
                      <<"\" src=\"//www.youtube.com/embed/">>,
                      z_url:url_encode(V),
                      <<"\" sandbox=\"">>,
                      z_sanitize:default_sandbox_attr(Context),
                      <<"\" style=\"border:none;\" allowfullscreen></iframe>">>]);
embed_code(<<"vimeo">>, H, W, V, Context) ->
    iolist_to_binary([<<"<iframe width=\"">>,
                      integer_to_list(W),
                      <<"\" height=\"">>,
                      integer_to_list(H),
                      <<"\" src=\"//player.vimeo.com/video/">>,
                      z_url:url_encode(V),
                      <<"\" sandbox=\"">>,
                      z_sanitize:default_sandbox_attr(Context),
                      <<"\" style=\"border:none;\" allowfullscreen></iframe>">>]).

%% @doc Handle the form submit from the "new media" dialog.  The form is defined in templates/_media_upload_panel.tpl.
%% @spec event(Event, Context1) -> Context2
event(#submit{ message = {add_video_embed, EventProps} }, Context) ->
    Actions = proplists:get_value(actions, EventProps, []),
    Id = proplists:get_value(id, EventProps),
    Callback = proplists:get_value(callback, EventProps),
    Stay =
        z_convert:to_bool(
            proplists:get_value(stay, EventProps, false)),
    EmbedService = z_context:get_q(<<"video_embed_service">>, Context),
    EmbedCode = z_context:get_q_validated(<<"video_embed_code">>, Context),

    case Id of
        %% Create a new page
        undefined ->
            SubjectId = proplists:get_value(subject_id, EventProps),
            ContentGroupdId =
                case proplists:get_value(content_group_id, EventProps) of
                    undefined ->
                        m_rsc:p_no_acl(SubjectId, content_group_id, Context);
                    CGId ->
                        CGId
                end,
            Predicate = proplists:get_value(predicate, EventProps, depiction),
            Title = z_context:get_q_validated(<<"title">>, Context),
            Props =
                #{
                    <<"title">> => Title,
                    <<"is_published">> => true,
                    <<"category_id">> => video,
                    <<"video_embed_service">> => EmbedService,
                    <<"video_embed_code">> => EmbedCode,
                    <<"content_group_id">> => ContentGroupdId
                },
            try m_rsc:insert(Props, Context) of
                {ok, MediaId} ->
                    spawn_preview_create(MediaId, Props, Context),

                    {_, ContextLink} =
                        mod_admin:do_link(
                            z_convert:to_integer(SubjectId), Predicate, MediaId, Callback, Context),

                    ContextRedirect =
                        case SubjectId of
                            undefined ->
                                case Stay of
                                    false ->
                                        z_render:wire({redirect, [{dispatch, "admin_edit_rsc"}, {id, MediaId}]},
                                                      ContextLink);
                                    true ->
                                        ContextLink
                                end;
                            _ ->
                                ContextLink
                        end,
                    z_render:wire([{dialog_close, []}, {growl, [{text, "Made the media page."}]} | Actions],
                                  ContextRedirect)
            catch
                {error, Error} ->
                    ?LOG_ERROR("[mod_video_embed] Error in add_video_embed: ~p on ~p", [Error, Props]),
                    z_render:growl_error("Could not create the media page.", Context)
            end;
        %% Update the current page
        N when is_integer(N) ->
            Props =
                #{
                    <<"category_id">> => video,
                    <<"video_embed_service">> => EmbedService,
                    <<"video_embed_code">> => EmbedCode
                },
            try
                {ok, _} = m_rsc:update(Id, Props, Context)
            catch
                _ ->
                    z_render:growl_error("Could not update the page with the new embed code.", Context)
            end
    end.

%%====================================================================
%% support functions
%%====================================================================

%% Fetch or create a preview for the movie
spawn_preview_create(MediaId, InsertProps, Context) ->
    case z_convert:to_binary(
             maps:get(<<"video_embed_service">>, InsertProps, undefined))
    of
        <<"youtube">> ->
            spawn(fun() ->
                     preview_youtube(MediaId, InsertProps, z_context:prune_for_async(Context))
                  end);
        <<"vimeo">> ->
            spawn(fun() ->
                     preview_vimeo(MediaId, InsertProps, z_context:prune_for_async(Context))
                  end);
        _ ->
            nop
    end.

% @doc Fetch the preview image of a youtube video. The preview is located at: http://img.youtube.com/vi/[code]/0.jpg
% @todo Make this more robust wrt http errors.
preview_youtube(MediaId, InsertProps, Context) ->
    case z_convert:to_binary(
             maps:get(<<"video_embed_id">>, InsertProps, <<>>))
    of
        <<>> ->
            static_preview(MediaId, <<"images/youtube.jpg">>, Context);
        EmbedId ->
            Url = "https://img.youtube.com/vi/" ++ z_convert:to_list(EmbedId) ++ "/0.jpg",
            m_media:save_preview_url(MediaId, Url, Context)
    end.

% @doc Fetch the preview image of a vimeo video.
% @todo Make this more robust wrt http errors.
preview_vimeo(MediaId, InsertProps, Context) ->
    case z_convert:to_binary(
             maps:get(<<"video_embed_id">>, InsertProps, <<>>))
    of
        <<>> ->
            static_preview(MediaId, <<"images/vimeo.jpg">>, Context);
        EmbedId ->
            case videoid_to_image(<<"vimeo">>, EmbedId) of
                undefined ->
                    static_preview(MediaId, <<"images/vimeo.jpg">>, Context);
                ImgUrl ->
                    m_media:save_preview_url(MediaId, ImgUrl, Context)
            end
    end.

videoid_to_image(<<"youtube">>, EmbedId) ->
    "https://img.youtube.com/vi/" ++ z_convert:to_list(EmbedId) ++ "/0.jpg";
videoid_to_image(<<"vimeo">>, EmbedId) ->
    JsonUrl = "https://vimeo.com/api/v2/video/" ++ z_convert:to_list(EmbedId) ++ ".json",
    case httpc:request(get, {JsonUrl, []}, [], [{body_format, binary}]) of
        {ok, {{_Http, 200, _Ok}, _Header, Data}} ->
            [JSON | _] = z_json:decode(Data),
            #{ <<"thumbnail_large">> := Thumbnail } = JSON,
            iolist_to_binary(re:replace(Thumbnail, <<"_[0-9]+(x[0-9]+)?$">>, <<"_1280">>));
        {ok, {StatusCode, _Header, Data}} ->
            ?LOG_WARNING("Vimeo metadata fetch returns ~p ~p", [StatusCode, Data]),
            undefined;
        _ ->
            undefined
    end;
videoid_to_image(_, _) ->
    undefined.

-spec static_preview(m_rsc:resource_id(), binary(), z:context()) -> nop | {ok, file:filename_all()} | {error, term()}.
static_preview(MediaId, LibFile, Context) ->
    case z_module_indexer:find(lib, LibFile, Context) of
        {ok, #module_index{ filepath = File }} ->
            Mime = z_media_identify:guess_mime(File),
            {ok, Bin} = file:read_file(File),
            m_media:save_preview(MediaId, Bin, Mime, Context);
        {error, enoent} ->
            nop
    end.

test() ->
    Html =
        <<"<iframe width=\"560\" height=\"315\" src=\"https://www.youtube.com/embed/PSb4ZfKif4Y\" frameborder=\"0\" allowfullscreen></iframe>">>,
    fetch_videoid_from_embed(<<"?">>, Html).
