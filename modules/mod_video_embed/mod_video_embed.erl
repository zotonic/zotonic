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
-export([
    observe_rsc_update/3,
    observe_media_viewer/2,
    observe_media_stillimage/2,
    observe_media_import/2,
    event/2,

    spawn_preview_create/3
]).

-export([
    test/0
    ]).

-include_lib("zotonic.hrl").
-include_lib("z_stdlib/include/z_url_metadata.hrl").


%% Fantasy mime type to distinguish embeddable html fragments.
-define(EMBED_MIME, <<"text/html-video-embed">>).

%% @doc Check if the update contains video embed information.  If so then update the attached medium item.
-spec observe_rsc_update(#rsc_update{}, {boolean(), list()}, #context{}) -> {boolean(), list()}.
observe_rsc_update(#rsc_update{action=insert, id=Id}, {Changed, Props}, Context) ->
    case proplists:get_value(video_embed_code, Props) of
        undefined ->
            {Changed, Props};
        [] ->
            {true, proplists:delete(video_embed_code, Props)};
        <<>> ->
            {true, proplists:delete(video_embed_code, Props)};
        EmbedCodeRaw ->
            case z_acl:is_allowed(insert, #acl_media{mime=?EMBED_MIME}, Context) of
                true ->
                    EmbedCode = z_sanitize:html(z_html:unescape(EmbedCodeRaw), Context),
                    EmbedService = z_convert:to_binary(
                                        proplists:get_value(video_embed_service, Props, <<>>)),
                    {EmbedService1, EmbedId} = fetch_videoid_from_embed(EmbedService, EmbedCode),
                    MediaProps = [
                        {mime, ?EMBED_MIME},
                        {video_embed_code, EmbedCode},
                        {video_embed_service, EmbedService1},
                        {video_embed_id, EmbedId}
                    ],
                    ok = m_media:replace(Id, MediaProps, Context),
                    spawn_preview_create(Id, MediaProps, Context);
                false ->
                    lager:info("Denied user ~p to embed ~p: ~p",
                               [z_acl:user(Context), ?EMBED_MIME, EmbedCodeRaw]),
                    ok
            end,
            Props1 = proplists:delete(video_embed_code,
                        proplists:delete(video_embed_service, Props)),
            {true, Props1}
    end;
observe_rsc_update(#rsc_update{action=update, id=Id}, {Changed, Props}, Context) ->
    case proplists:is_defined(video_embed_code, Props) of
        true ->
            OldMediaProps = m_media:get(Id, Context),
            EmbedChanged = case proplists:get_value(video_embed_code, Props) of
                Empty when Empty =:= undefined; Empty =:= <<>>; Empty =:= [] ->
                    % Delete the media record iff the media mime type is our mime type
                    case OldMediaProps of
                        undefined ->
                            false;
                        _ ->
                            case proplists:get_value(mime, OldMediaProps) of
                                ?EMBED_MIME ->
                                    m_media:delete(Id, Context),
                                    true;
                                _ ->
                                    false
                            end
                    end;
                EmbedCodeRaw ->
                    EmbedCode = z_sanitize:html(z_html:unescape(EmbedCodeRaw), Context),
                    EmbedService = proplists:get_value(video_embed_service, Props, <<>>),
                    {EmbedService1, EmbedId} = fetch_videoid_from_embed(EmbedService, EmbedCode),
                    MediaProps = [
                        {mime, ?EMBED_MIME},
                        {video_embed_code, EmbedCode},
                        {video_embed_service, EmbedService1},
                        {video_embed_id, EmbedId}
                    ],
                    case OldMediaProps of
                        undefined ->
                            ok = m_media:replace(Id, MediaProps, Context),
                            spawn_preview_create(Id, MediaProps, Context),
                            true;
                        _ ->
                            case        z_utils:are_equal(proplists:get_value(mime, OldMediaProps), ?EMBED_MIME)
                                andalso z_utils:are_equal(proplists:get_value(video_embed_code, OldMediaProps), EmbedCode)
                                andalso z_utils:are_equal(proplists:get_value(video_embed_service, OldMediaProps), EmbedService)
                            of
                                true ->
                                    false;
                                false ->
                                    ok = m_media:replace(Id, MediaProps, Context),
                                    spawn_preview_create(Id, MediaProps, Context),
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


%% @doc Recognize youtube and vimeo URLs, generate the correct embed code
observe_media_import(#media_import{host_rev=[<<"com">>, <<"youtube">> | _], metadata=MD} = MI, Context) ->
    media_import(youtube, ?__("Youtube Video", Context), MD, MI, Context);
observe_media_import(#media_import{host_rev=[<<"com">>, <<"vimeo">> | _], metadata=MD} = MI, Context) ->
    media_import(vimeo, ?__("Vimeo Video", Context), MD, MI, Context);
observe_media_import(#media_import{}, _Context) ->
    undefined.

media_import(Service, Descr, MD, MI, Context) ->
    H = z_convert:to_integer(z_url_metadata:p([<<"og:video:height">>, <<"twitter:player:height">>], MD)),
    W = z_convert:to_integer(z_url_metadata:p([<<"og:video:width">>, <<"twitter:player:width">>], MD)),
    VideoId = fetch_videoid_from_url(Service, MI#media_import.url),
    case is_integer(H) andalso is_integer(W) andalso VideoId =/= <<>> of
        true ->
            [
                media_import_props_video(Service, Descr, MD, MI, H, W, VideoId),
                media_import_props_image(Service, MD, VideoId, Context)
            ];
        false ->
            undefined
    end.

media_import_props_video(Service, Descr, MD, MI, H, W, VideoId) ->
    #media_import_props{
        prio = 1,
        category = video,
        module = ?MODULE,
        description = Descr,
        rsc_props = [
            {title, z_url_metadata:p(title, MD)},
            {summary, z_url_metadata:p(summary, MD)},
            {website, MI#media_import.url}
        ],
        medium_props = [
            {mime, ?EMBED_MIME},
            {width, W},
            {height, H},
            {video_embed_service, z_convert:to_binary(Service)},
            {video_embed_code, embed_code(Service, H, W, VideoId)},
            {video_embed_id, z_convert:to_binary(VideoId)},
            {media_import, MI#media_import.url}
        ],
        preview_url = z_url_metadata:p(image, MD)
    }.

media_import_props_image(Service, MD, VideoId, Context) ->
    case videoid_to_image(Service, VideoId) of
        undefined ->
            undefined;
        ImgUrl ->
            #media_import_props{
                prio = 10,
                category = image,
                description = m_rsc:p_no_acl(image, title, Context),
                rsc_props = [
                    {title, z_url_metadata:p(title, MD)},
                    {summary, z_url_metadata:p(summary, MD)},
                    {website, z_url_metadata:p(url, MD)}
                ],
                medium_props = [
                    {mime, z_convert:to_binary(z_media_identify:guess_mime(ImgUrl))}
                ],
                medium_url = z_convert:to_binary(ImgUrl)
            }
    end.

fetch_videoid_from_embed(_Service, undefined) ->
    {<<>>, undefined};
fetch_videoid_from_embed(_Service, <<>>) ->
    {<<>>, undefined};
fetch_videoid_from_embed(Service, EmbedCode) ->
    case re:run(EmbedCode,
                <<"(src|href)=\"([^\"]*)\"">>,
                [global, notempty, {capture, all, binary}])
    of
        {match, [[_,_,Url]|_]} ->
            case url_to_service(Url) of
                undefined ->
                    {Service, <<>>};
                UrlService ->
                    {z_convert:to_binary(UrlService), fetch_videoid_from_url(UrlService, Url)}
            end;
        nomatch ->
            {Service, <<>>}
    end.

fetch_videoid_from_url(youtube, Url) ->
    [Url1|_] = binary:split(Url, <<"?">>),
    case binary:split(Url1, <<"/embed/">>) of
        [_, Code] ->
            Code;
        _ ->
            {_Protocol, _Host, _Path, Qs, _Hash} = mochiweb_util:urlsplit(z_convert:to_list(Url)),
            Qs1 = mochiweb_util:parse_qs(Qs),
            z_convert:to_binary(proplists:get_value("v", Qs1))
    end;
fetch_videoid_from_url(vimeo, Url) ->
    {_Protocol, _Host, Path, _Qs, _Hash} = mochiweb_util:urlsplit(z_convert:to_list(Url)),
    P1 = lists:last(string:tokens(Path, "/")),
    case z_utils:only_digits(P1) of
        true -> z_convert:to_binary(P1);
        false -> <<>>
    end;
fetch_videoid_from_url(_Service, _Url) ->
    <<>>.

url_to_service(<<"https://", Url/binary>>) -> url_to_service(Url);
url_to_service(<<"http://", Url/binary>>) -> url_to_service(Url);
url_to_service(<<"//", Url/binary>>) -> url_to_service(Url);
url_to_service(<<"www.youtube.com/", _/binary>>) -> youtube;
url_to_service(<<"youtube.com/", _/binary>>) -> youtube;
url_to_service(<<"www.vimeo.com/", _/binary>>) -> vimeo;
url_to_service(<<"vimeo.com/", _/binary>>) -> vimeo;
url_to_service(<<"player.vimeo.com/", _/binary>>) -> vimeo;
url_to_service(_) -> undefined.


embed_code(youtube, H, W, V) ->
    iolist_to_binary([
        <<"<iframe width=\"">>,integer_to_list(W),
        <<"\" height=\"">>,integer_to_list(H),
        <<"\" src=\"//www.youtube.com/embed/">>, z_url:url_encode(V),
        <<"\" style=\"border:none;\" allowfullscreen></iframe>">>
        ]);
embed_code(vimeo, H, W, V) ->
    iolist_to_binary([
        <<"<iframe width=\"">>,integer_to_list(W),
        <<"\" height=\"">>,integer_to_list(H),
        <<"\" src=\"//player.vimeo.com/video/">>, z_url:url_encode(V),
        <<"\" style=\"border:none;\" allowfullscreen></iframe>">>
        ]).

%% @doc Handle the form submit from the "new media" dialog.  The form is defined in templates/_media_upload_panel.tpl.
%% @spec event(Event, Context1) -> Context2
event(#submit{message={add_video_embed, EventProps}}, Context) ->
    Actions = proplists:get_value(actions, EventProps, []),
    Id = proplists:get_value(id, EventProps),
    Callback = proplists:get_value(callback, EventProps),
    Stay = z_convert:to_bool(proplists:get_value(stay, EventProps, false)),
    EmbedService = z_context:get_q(<<"video_embed_service">>, Context),
    EmbedCode = z_context:get_q_validated(<<"video_embed_code">>, Context),

    case Id of
        %% Create a new page
        undefined ->
            SubjectId = proplists:get_value(subject_id, EventProps),
            ContentGroupdId = case proplists:get_value(content_group_id, EventProps) of
                                    undefined -> m_rsc:p_no_acl(SubjectId, content_group_id, Context);
                                    CGId -> CGId
                              end,
            Predicate = proplists:get_value(predicate, EventProps, depiction),
            Title   = z_context:get_q_validated(<<"title">>, Context),
            Props = [
                {title, Title},
                {is_published, true},
                {category, video},
                {video_embed_service, EmbedService},
                {video_embed_code, EmbedCode},
                {content_group_id, ContentGroupdId}
            ],

            try m_rsc:insert(Props, Context) of
                {ok, MediaId} ->
                    spawn_preview_create(MediaId, Props, Context),

                    {_, ContextLink} = mod_admin:do_link(z_convert:to_integer(SubjectId), Predicate,
                                                         MediaId, Callback, Context),

                    ContextRedirect = case SubjectId of
                        undefined ->
                            case Stay of
                                false -> z_render:wire(
                                    {redirect, [
                                        {dispatch, "admin_edit_rsc"}, {id, MediaId}
                                    ]},
                                    ContextLink
                                );
                                true -> ContextLink
                            end;
                        _ -> ContextLink
                    end,
                    z_render:wire([
                        {dialog_close, []},
                        {growl, [{text, "Made the media page."}]}
                        | Actions], ContextRedirect)
            catch
                {error, Error} ->
                    lager:error("[mod_video_embed] Error in add_video_embed: ~p on ~p", [Error, Props]),
                    z_render:growl_error("Could not create the media page.", Context)
            end;

        %% Update the current page
        N when is_integer(N) ->
            Props = [
                {category, video},
                {video_embed_service, EmbedService},
                {video_embed_code, EmbedCode}
            ],

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
    case z_convert:to_binary(proplists:get_value(video_embed_service, InsertProps)) of
        <<"youtube">> ->
            spawn(fun() -> preview_youtube(MediaId, InsertProps, z_context:prune_for_async(Context)) end);
        <<"vimeo">> ->
            spawn(fun() -> preview_vimeo(MediaId, InsertProps, z_context:prune_for_async(Context)) end);
        _ -> nop
    end.

% @doc Fetch the preview image of a youtube video. The preview is located at: http://img.youtube.com/vi/[code]/0.jpg
% @todo Make this more robust wrt http errors.
preview_youtube(MediaId, InsertProps, Context) ->
    case z_convert:to_binary(proplists:get_value(video_embed_id, InsertProps)) of
        <<>> ->
            static_preview(MediaId, "images/youtube.jpg", Context);
        EmbedId ->
            Url = "http://img.youtube.com/vi/"++z_convert:to_list(EmbedId)++"/0.jpg",
            m_media:save_preview_url(MediaId, Url, Context)
    end.

% @doc Fetch the preview image of a vimeo video. http://stackoverflow.com/questions/1361149/get-img-thumbnails-from-vimeo
% @todo Make this more robust wrt http errors.
preview_vimeo(MediaId, InsertProps, Context) ->
    case z_convert:to_binary(proplists:get_value(video_embed_id, InsertProps)) of
        <<>> ->
            static_preview(MediaId, "images/vimeo.jpg", Context);
        EmbedId ->
            case videoid_to_image(vimeo, EmbedId) of
                undefined ->
                    static_preview(MediaId, "images/vimeo.jpg", Context);
                ImgUrl ->
                    m_media:save_preview_url(MediaId, ImgUrl, Context)
            end
    end.

videoid_to_image(youtube, EmbedId) ->
    "http://img.youtube.com/vi/"++z_convert:to_list(EmbedId)++"/0.jpg";
videoid_to_image(vimeo, EmbedId) ->
    JsonUrl = "http://vimeo.com/api/v2/video/" ++ z_convert:to_list(EmbedId) ++ ".json",
    case httpc:request(JsonUrl) of
        {ok, {{_Http, 200, _Ok}, _Header, Data}} ->
            {array, [{struct, Props}]} = mochijson:decode(Data),
            proplists:get_value("thumbnail_large", Props);
        {ok, {StatusCode, _Header, Data}} ->
            lager:warning("Vimeo metadata fetch returns ~p ~p", [StatusCode, Data]),
            undefined;
        _ ->
            undefined
    end;
videoid_to_image(_Service, _EmbedId) ->
    undefined.

static_preview(MediaId, LibFile, Context) ->
    case z_module_indexer:find(lib, LibFile, Context) of
        {ok, #module_index{filepath=File}} ->
            Mime = z_media_identify:guess_mime(File),
            {ok, Bin} = file:read_file(File),
            m_media:save_preview(MediaId, Bin, Mime, Context);
        {error, enoent} ->
            nop
    end.


test() ->
    Html = <<"<iframe width=\"560\" height=\"315\" src=\"https://www.youtube.com/embed/PSb4ZfKif4Y\" frameborder=\"0\" allowfullscreen></iframe>">>,
    fetch_videoid_from_embed(<<"?">>, Html).
