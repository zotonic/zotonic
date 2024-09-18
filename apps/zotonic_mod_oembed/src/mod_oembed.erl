%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011-2023 Arjan Scherpenisse <arjan@scherpenisse.net>
%% @doc Enables embedding media from their URL.
%% @end

%% Copyright 2011-2023 Arjan Scherpenisse <arjan@scherpenisse.net>
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
    observe_rsc_update/3,
    observe_media_viewer/2,
    observe_media_stillimage/2,
    observe_media_import/2,
    observe_media_import_medium/2,

    event/2,

    preview_create/2,
    oembed_request/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% Fantasy mime type to distinguish embeddable html fragments.
-define(OEMBED_MIME, <<"text/html-oembed">>).

%% @doc Check if the update contains video embed information.  If so
%% then try to get the oembed information from the provider and update
%% the attached medium item.
-spec observe_rsc_update(#rsc_update{}, {ok, m_rsc:props()} | {error, term()}, z:context()) ->
          {ok, m_rsc:props()}
        | {error, term()}.
observe_rsc_update(#rsc_update{action=insert, id=Id}, {ok, Props}, Context) ->
    case maps:get(<<"oembed_url">>, Props, undefined) of
        undefined ->
            {ok, Props};
        "" ->
            {ok, maps:remove(<<"oembed_url">>, Props)};
        <<>> ->
            {ok, maps:remove(<<"oembed_url">>, Props)};
        EmbedUrl ->
            case z_acl:is_allowed(insert, #acl_media{mime=?OEMBED_MIME}, Context) of
                true ->
                    MediaProps = #{
                        <<"mime">> => ?OEMBED_MIME,
                        <<"oembed_url">> => EmbedUrl
                    },
                    case preview_create_from_medium(Id, MediaProps, z_acl:sudo(Context)) of
                        undefined ->
                            {ok, maps:remove(<<"oembed_url">>, Props)};
                        OEmbedTitle ->
                            Title = z_trans:lookup_fallback( maps:get(<<"title">>, Props, <<>>), Context ),
                            Props1 = case z_utils:is_empty(Title) of
                                        true ->
                                            Props#{
                                                <<"title">> => z_html:escape(OEmbedTitle)
                                            };
                                        false ->
                                            Props
                                     end,
                            {ok, maps:remove(<<"oembed_url">>, Props1)}
                    end;
                false ->
                    {ok, maps:remove(<<"oembed_url">>, Props)}
            end
    end;
observe_rsc_update(#rsc_update{action=update, id=Id, props=CurrProps}, {ok, UpdateProps}, Context) ->
    case maps:is_key(<<"oembed_url">>, UpdateProps) of
        true ->
            OldMediaProps = m_media:get(Id, Context),
            {EmbedChanged, OEmbedTitle} = case maps:get(<<"oembed_url">>, UpdateProps) of
                Empty when Empty =:= undefined; Empty =:= <<>>; Empty =:= "" ->
                    % Delete the media record iff the media mime type is our mime type
                    case OldMediaProps of
                        #{ <<"mime">> := ?OEMBED_MIME } ->
                            m_media:delete(Id, Context);
                        _ ->
                            nop
                    end,
                    {false, undefined};
                EmbedUrl ->
                    MediaProps = #{
                        <<"mime">> => ?OEMBED_MIME,
                        <<"oembed_url">> => EmbedUrl
                    },
                    case OldMediaProps of
                        undefined ->
                            {true, preview_create_from_medium(Id, MediaProps, Context)};
                        #{ <<"mime">> := ?OEMBED_MIME } ->
                            case        z_utils:are_equal(maps:get(oembed_url, OldMediaProps, undefined), EmbedUrl)
                                andalso maps:get(<<"oembed">>, OldMediaProps, undefined) =/= undefined
                            of
                                true ->
                                    %% Not changed
                                    {false, undefined};
                                false ->
                                    %% Changed, update the medium record
                                    {true, preview_create_from_medium(Id, MediaProps, Context)}
                            end;
                        _ ->
                            {true, preview_create_from_medium(Id, MediaProps, Context)}
                    end
            end,
            % Set the rsc title if it was not yet defined
            UpdateProps1 = case EmbedChanged of
                true when is_binary(OEmbedTitle) ->
                    CurrTitle = maps:get(<<"title">>, UpdateProps, maps:get(<<"title">>, CurrProps, <<>>)),
                    case z_utils:is_empty(z_trans:lookup_fallback(CurrTitle, Context)) of
                        true ->
                            UpdateProps#{
                                <<"title">> => z_html:escape_check(OEmbedTitle)
                            };
                        false ->
                            UpdateProps
                    end;
                true ->
                    UpdateProps;
                false ->
                    UpdateProps
            end,
            {ok, maps:remove(<<"oembed_url">>, UpdateProps1)};
        false ->
            {ok, UpdateProps}
    end;
observe_rsc_update(#rsc_update{}, {error, _} = Error, _Context) ->
    Error.



%% @doc Return the media viewer for the embedded video (that is, when
%% it is an embedded media). First tries the template called
%% <tt>_oembed_embeddable_(providername).tpl</tt>; if not found, falls back to
%% the HTML code that the oembed provider gave us; if none found,
%% falls back to the generic template <tt>_oembed_embeddable.tpl</tt>.
%% @spec observe_media_viewer(Notification, Context) -> undefined | {ok, Html}
observe_media_viewer(#media_viewer{
            id = Id,
            props= #{ <<"mime">> := ?OEMBED_MIME } = Props,
            filename = Filename,
            options = Options
        }, Context) ->
    TplOpts = [
        {id, Id},
        {medium, Props},
        {options, Options},
        {filename, Filename}
    ],
    case maps:find(<<"oembed">>, Props) of
       {ok, OEmbed} ->
            EmbedCode = case lookup(<<"provider_name">>, provider_name, OEmbed) of
                {ok, N} ->
                    Tpl = iolist_to_binary(["_oembed_embeddable_",z_string:to_name(N),".tpl"]),
                    case z_module_indexer:find(template, Tpl, Context) of
                        {ok, Found} ->
                            {TplHtml, _} = z_template:render_to_iolist(Found, TplOpts, Context),
                            TplHtml;
                        {error, _} ->
                            media_viewer_fallback(OEmbed, TplOpts, Context)
                    end;
                error ->
                    media_viewer_fallback(OEmbed, TplOpts, Context)
            end,
            case z_notifier:first(#media_viewer_consent{
                    id = Id,
                    consent = all,
                    html = EmbedCode,
                    viewer_props = Props,
                    viewer_options = Options
                 }, Context)
             of
                undefined ->
                    {ok, EmbedCode};
                {ok, _} = ConsentHtml ->
                    ConsentHtml
             end;
        error ->
            {ok, <<"<!-- No oembed code found -->">>}
    end;
observe_media_viewer(#media_viewer{}, _Context) ->
    undefined.

lookup(K1, K2, OEmbed) when is_list(OEmbed) ->
    case proplists:lookup(K1, OEmbed) of
        {K1, V} -> {ok, V};
        none ->
            case proplists:lookup(K2, OEmbed) of
                {K2, V} -> {ok, V};
                none -> error
            end
    end;
lookup(K1, K2, OEmbed) when is_map(OEmbed) ->
    case maps:find(K1, OEmbed) of
        {ok, V} -> {ok, V};
        error -> maps:find(K2, OEmbed)
    end.

media_viewer_fallback(OEmbed, TplOpts, Context) ->
    Vars = case lookup(<<"html">>, html, OEmbed) of
        {ok, Html} ->
            Html1 = binary:replace(Html, <<"http://">>, <<"https://">>, [global]),
            IsIframe = binary:match(Html1, <<"<iframe">>) =/= nomatch,
            [
                {html, Html1},
                {is_iframe, IsIframe}
                | TplOpts
            ];
        error ->
            TplOpts
    end,
    {EmbedHtml, _} = z_template:render_to_iolist("_oembed_embeddable.tpl", Vars, Context),
    EmbedHtml.


% @doc Recognize youtube and vimeo URLs, generate the correct embed code
observe_media_import(#media_import{url=Url, metadata=MD}, Context) ->
    case oembed_request(Url, Context) of
        {ok, Json} ->
            Category = type_to_category(maps:get(<<"type">>, Json, undefined)),
            #media_import_props{
                prio = case Category of
                            website -> 11; % Prefer our own 'website' extraction
                            video -> 3;
                            document -> 4;
                            _ -> 6
                       end,
                category = Category,
                module = ?MODULE,
                description = ?__("Embedded Content", Context),
                rsc_props = #{
                    <<"title">> => z_html:unescape(
                            first([
                                maps:get(<<"title">>, Json, undefined),
                                z_url_metadata:p(title, MD)
                            ])),
                    <<"summary">> => z_html:unescape(
                            first([
                                maps:get(<<"description">>, Json, undefined),
                                z_url_metadata:p(summary, MD)
                            ])),
                    <<"website">> => Url
                },
                medium_props = #{
                    <<"mime">> => ?OEMBED_MIME,
                    <<"width">> => maps:get(<<"width">>, Json, undefined),
                    <<"height">> => maps:get(<<"height">>, Json, undefined),
                    <<"oembed_service">> => maps:get(<<"provider_name">>, Json, undefined),
                    <<"oembed_url">> => Url,
                    <<"oembed">> => Json,
                    <<"media_import">> => Url
                },
                preview_url = maps:get(<<"thumbnail_url">>, Json, undefined)
            };
        {error, _} ->
            undefined
    end.

first([]) -> undefined;
first([undefined|Xs]) -> first(Xs);
first([""|Xs]) -> first(Xs);
first([<<>>|Xs]) -> first(Xs);
first([X|_]) -> X.


%% @doc Import a embedded medium for a rsc_import. Sanitize the provided html. Try to fetch
%% a fresh embed copy, as sometimes the preview URLs are not valid anymore. If the fresh OEmbed
%% request fails then use the JSON from the importer.
-spec observe_media_import_medium(#media_import_medium{}, z:context()) -> undefined | ok.
observe_media_import_medium(#media_import_medium{
        id = Id,
        medium = #{
            <<"mime">> := ?OEMBED_MIME,
            <<"oembed_url">> := Url,
            <<"oembed">> := ImportJson
        } = Medium }, Context) when is_map(ImportJson) ->
    case m_media:get(Id, Context) of
        #{ <<"oembed_url">> := CurrentUrl } when CurrentUrl =:= Url ->
            ok;
        _Other ->
            SafeUrl = z_sanitize:uri(Url),
            {MediaProps, MediaJson} = case oembed_request(SafeUrl, Context) of
                {ok, EmbedJson} ->
                    {#{
                        <<"mime">> => ?OEMBED_MIME,
                        <<"width">> => maps:get(<<"width">>, EmbedJson, undefined),
                        <<"height">> => maps:get(<<"height">>, EmbedJson, undefined),
                        <<"oembed_service">> => maps:get(<<"provider_name">>, EmbedJson, undefined),
                        <<"oembed_url">> => SafeUrl,
                        <<"oembed">> => EmbedJson,
                        <<"media_import">> => SafeUrl
                    }, EmbedJson};
                {error, _} ->
                    SafeJson = sanitize_json(ImportJson, Context),
                    Service = z_convert:to_binary(maps:get(<<"oembed_service">>, Medium, undefined)),
                    {#{
                        <<"mime">> => ?OEMBED_MIME,
                        <<"oembed_service">> => Service,
                        <<"oembed_url">> => SafeUrl,
                        <<"oembed">> => SafeJson,
                        <<"height">> => as_int(maps:get(<<"height">>, Medium, undefined)),
                        <<"width">> => as_int(maps:get(<<"width">>, Medium, undefined)),
                        <<"orientation">> => as_int(maps:get(<<"orientation">>, Medium, undefined)),
                        <<"media_import">> => z_sanitize:uri( maps:get(<<"media_import">>, Medium, undefined ))
                    }, SafeJson}
            end,
            ok = m_media:replace(Id, MediaProps, Context),
            preview_create_from_json(Id, MediaJson, Context),
            ok
    end;
observe_media_import_medium(#media_import_medium{}, _Context) ->
    undefined.

as_int(undefined) -> undefined;
as_int(N) -> z_convert:to_integer(N).


%% @doc Return the filename of a still image to be used for image tags.
%% @spec observe_media_stillimage(Notification, _Context) -> undefined | {ok, Filename}
observe_media_stillimage(#media_stillimage{ props = #{ <<"mime">> := ?OEMBED_MIME } = Props}, _Context) ->
    case maps:get(<<"preview_filename">>, Props, <<>>) of
        undefined -> {ok, <<"lib/images/embed.jpg">>};
        <<>> -> {ok, <<"lib/images/embed.jpg">>};
        PreviewFile -> {ok, PreviewFile}
    end;
observe_media_stillimage(#media_stillimage{}, _Context) ->
    undefined.

event(#postback{ message = fix_missing }, Context) ->
    case oembed_admin:count_missing(Context) of
        0 ->
            z_render:growl(?__("No embedded videos found which need fixing.", Context), Context);
        N ->
            spawn(fun() -> oembed_admin:count_missing(Context) end),
            Msg = ?__("Attempting to fix ~p videos.", Context),
            z_render:growl(lists:flatten(io_lib:format(Msg, [N])), Context)
    end;

event(#submit{ message = admin_oembed }, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            EmbedlyKey = z_string:trim(z_context:get_q(<<"embedly_key">>, Context)),
            m_config:set_value(mod_oembed, embedly_key, EmbedlyKey, Context),
            z_render:growl(?__("Saved the Embedly settings.", Context), Context);
        false ->
            z_render:growl(?__("You don't have permission to change the Embedly settings.", Context), Context)
    end.


%% @doc (Re)create a preview from the stored oembed information
preview_create(Id, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            case m_media:get(Id, Context) of
                Ms when is_map(Ms) ->
                    case maps:get(<<"oembed">>, Ms, undefined) of
                        Json when is_map(Json) ->
                            preview_create_from_json(Id, Json, Context);
                        Json when is_list(Json) ->
                            {ok, Map} = z_props:from_list(Json),
                            preview_create_from_json(Id, Map, Context);
                        undefined ->
                            {error, no_oembed}
                    end;
                undefined ->
                    {error, enoent}
            end;
        false ->
            {error, eacces}
    end.

%%====================================================================
%% support functions
%%====================================================================

%% Fetch or create a preview for the movie. Returns the media title
%% that need to be set on the rsc if the rsc has no title.
preview_create_from_medium(MediaId, #{ <<"oembed_url">> := Url } = MediaProps, Context)
    when is_binary(Url),
         Url =/= <<>> ->
    case oembed_request(Url, Context) of
        {ok, Json} ->
            case maps:get(<<"type">>, Json, undefined) of
                <<"link">> ->
                    % The selected images for "link" are quite bad, so don't
                    % embed anything for this type.
                    undefined;
                _Type ->
                    %% store found properties in the media part of the rsc
                    Html = maps:get(<<"html">>, Json, <<>>),
                    {EmbedService, EmbedId} = fetch_videoid_from_embed(<<>>, Html),
                    MediaProps1 = MediaProps#{
                        <<"oembed">> => Json,
                        <<"video_embed_service">> => EmbedService,
                        <<"video_embed_id">> => EmbedId
                    },
                    ok = m_media:replace(MediaId, MediaProps1, Context),
                    _ = preview_create_from_json(MediaId, Json, Context),
                    maps:get(<<"title">>, Json, undefined)
            end;
        {error, {http, Code, Body}} ->
            Err = #{
                <<"error">> => http_error,
                <<"code">> => Code,
                <<"body">> => Body
            },
            MediaProps1 = MediaProps#{
                <<"oembed">> => Err
            },
            ok = m_media:replace(MediaId, MediaProps1, Context),
            undefined;
        {error, _} ->
            undefined
    end.

preview_create_from_json(MediaId, Json, Context) ->
    Type = maps:get(<<"type">>, Json, undefined),
    case preview_url_from_json(Type, Json) of
        undefined ->
            nop;
        ThumbUrl ->
            case thumbnail_request(ThumbUrl, Context) of
                {ok, {CT, ImageData}} ->
                    {ok, _} = m_media:save_preview(MediaId, ImageData, CT, Context),
                    %% move to correct category if rsc is a 'media'
                    case m_rsc:is_a(MediaId, media, Context) of
                        true -> m_rsc:update(MediaId, [{category, type_to_category(Type)}], Context);
                        false -> m_rsc:touch(MediaId, Context)
                    end;
                {error, _} ->
                    nop
            end
    end.

%% @doc Perform OEmbed discovery on a given URL.
-spec oembed_request( string() | binary(), z:context() ) -> {ok, map()} | {error, term()}.
oembed_request(Url, Context) ->
    F = fun() ->
        oembed_client:discover(Url, Context)
    end,
    case z_depcache:memo(F, {oembed, Url}, 1800, Context) of
        {ok, Json} when is_map(Json) ->
            {ok, sanitize_json(Json, Context)};
        {error, _} = Error ->
            Error
    end.

-spec sanitize_json( map(), z:context() ) -> map().
sanitize_json(Json, Context) ->
    maps:fold(
        fun(K, V, Acc) ->
            case sanitize_json1(K, V, Context) of
                {K1, V1} ->
                    Acc#{ K1 => V1 };
                false ->
                    Acc
            end
        end,
        #{},
        Json).

sanitize_json1(_K, null, _Context) ->
    false;
sanitize_json1(<<"html">>, <<>>, _Context) ->
    false;
sanitize_json1(<<"html">>, Html, Context) ->
    case z_sanitize:html(Html,Context) of
        <<>> -> false;
        Html1 -> {<<"html">>, Html1}
    end;
sanitize_json1(<<"body">>, Body, Context) ->
    {<<"body">>, z_sanitize:html(Body, Context)};
sanitize_json1(<<"url">>, Url, _Context) ->
    {<<"url">>, z_sanitize:uri(Url)};
sanitize_json1(<<"provider_url">>, Url, _Context) ->
    {<<"provider_url">>, z_sanitize:uri(Url)};
sanitize_json1(<<"author_url">>, Url, _Context) ->
    {<<"author_url">>, z_sanitize:uri(Url)};
sanitize_json1(K, V, _Context) when is_binary(V) ->
    {K,  z_html:escape_check(V)};
sanitize_json1(K, V, _Context) when is_boolean(V); is_integer(V) ->
    {K, V};
sanitize_json1(_K, _V, _Context) ->
    false.

%% @doc Given a thumbnail URL, download it and return the content type plus image data pair.
thumbnail_request(ThumbUrl, _Context) ->
    case httpc:request(get, {z_convert:to_list(ThumbUrl), []}, [], []) of
        {ok, {{_, 200, _}, Headers, ImageData}} ->
            CT = case proplists:lookup("content-type", Headers) of
                     {"content-type", C} -> z_convert:to_binary(C);
                     _ -> <<"image/jpeg">>
                 end,
            {ok, {CT, ImageData}};
        {ok, {{_, 404, _}, _Headers, _ImageData}} ->
            ?LOG_INFO(#{
                text => <<"mod_oembed: 404 on thumbnail url">>,
                in => zotonic_mod_oembed,
                result => error,
                reason => enoent,
                url => ThumbUrl
            }),
            {error, enoent};
        Other ->
            ?LOG_WARNING(#{
                text => <<"mod_oembed: unexpected result for thumbnail url">>,
                in => zotonic_mod_oembed,
                result => error,
                reason => Other,
                url => ThumbUrl
            }),
            {error, httpc}
    end.


%% @doc Get the preview URL from JSON structure. Either the thumbnail
%% URL for non-photo elements, or the full URL for photo elements.
preview_url_from_json(<<"photo">>, Json) ->
    case maps:get(<<"url">>, Json, undefined) of
        undefined ->
            maps:get(<<"thumbnail_url">>, Json, undefined);
        Url ->
            Url
    end;
preview_url_from_json(_Type, Json) ->
    maps:get(<<"thumbnail_url">>, Json, undefined).


type_to_category(<<"photo">>) -> image;
type_to_category(<<"video">>) -> video;
type_to_category(<<"link">>) -> website;
type_to_category(<<"rich">>) -> document;
type_to_category(_Type) -> document.


%% This is a copy from mod_video_embed, should be combined (which is in the works)
fetch_videoid_from_embed(_Service, undefined) ->
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
                    {z_convert:to_binary(UrlService), fetch_videoid(UrlService, Url)}
            end;
        nomatch ->
            {Service, <<>>}
    end.

fetch_videoid(youtube, Url) ->
    [Url1|_] = binary:split(Url, <<"?">>),
    case binary:split(Url1, <<"/embed/">>) of
        [_, Code] ->
            Code;
        _ ->
            {_Protocol, _Host, _Path, Qs, _Hash} = mochiweb_util:urlsplit(z_convert:to_list(Url)),
            Qs1 = mochiweb_util:parse_qs(Qs),
            z_convert:to_binary(proplists:get_value("v", Qs1))
    end;
fetch_videoid(vimeo, Url) ->
    {_Protocol, _Host, Path, _Qs, _Hash} = mochiweb_util:urlsplit(z_convert:to_list(Url)),
    P1 = lists:last(string:tokens(Path, "/")),
    case z_utils:only_digits(P1) of
        true -> z_convert:to_binary(P1);
        false -> <<>>
    end;
fetch_videoid(_Service, _Url) ->
    <<>>.

url_to_service(<<"https://", Url/binary>>) -> url_to_service(Url);
url_to_service(<<"http://", Url/binary>>) -> url_to_service(Url);
url_to_service(<<"//", Url/binary>>) -> url_to_service(Url);
url_to_service(<<"www.youtube.com/", _/binary>>) -> youtube;
url_to_service(<<"youtube.com/", _/binary>>) -> youtube;
url_to_service(<<"www.vimeo.com/", _/binary>>) -> vimeo;
url_to_service(<<"vimeo.com/", _/binary>>) -> vimeo;
url_to_service(<<"flv.video.yandex.ru/", _/binary>>) -> yandex;
url_to_service(<<"static.video.yandex.ru/", _/binary>>) -> yandex;
url_to_service(_) -> undefined.
