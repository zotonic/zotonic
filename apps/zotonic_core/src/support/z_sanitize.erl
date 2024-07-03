%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2024 Marc Worrell
%% @doc Interface to z_html sanitizers, sets options and adds embed sanitization.
%% @end

%% Copyright 2014-2024 Marc Worrell
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

-module(z_sanitize).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    uri/1,
    default_sandbox_attr/1,
    ensure_safe_js_callback/1,
    escape_props/1,
    escape_props/2,
    escape_props_check/1,
    escape_props_check/2,
    escape_link/1,
    escape_link/2,
    html/1,
    html/2
]).

-include_lib("zotonic.hrl").


% Youtube needs at least: allow-popups allow-same-origin allow-scripts
% See: https://csplite.com/csp/test186/
-define(IFRAME_SANDBOX, <<"allow-popups allow-scripts allow-same-origin">>).


uri(Uri) ->
    z_html:sanitize_uri(Uri).


default_sandbox_attr(Context) ->
    case m_config:get_value(site, html_iframe_sandbox, Context) of
        undefined -> ?IFRAME_SANDBOX;
        <<>> -> ?IFRAME_SANDBOX;
        Sb -> Sb
    end.

%% @doc Escape a Javascript callback function. Crash if not a safe callback function name.
-spec ensure_safe_js_callback( string() | binary() ) -> binary().
ensure_safe_js_callback(Callback) ->
    nomatch = re:run(Callback, "[^a-zA-Z0-9_\\.]"),
    iolist_to_binary(Callback).

escape_props(Props) ->
    z_html:escape_props(Props, default_options()).

escape_props(Props, Context) ->
    z_html:escape_props(Props, context_options(Context)).

escape_props_check(Props) ->
    z_html:escape_props_check(Props, default_options()).

escape_props_check(Props, Context) ->
    z_html:escape_props_check(Props, context_options(Context)).

escape_link(#trans{ tr = Tr }) ->
    [ {Lang, escape_link(Text)} || {Lang, Text} <- Tr ];
escape_link(V) ->
    z_html:escape_link(V).

escape_link(V, Context) ->
    z_html:escape_link(z_trans:lookup_fallback(V, Context)).

html(Html) ->
    z_html:sanitize(Html, default_options()).

html(Html, Context) ->
    z_html:sanitize(Html, context_options(Context)).


context_options(Context) ->
    [
        {elt_extra, m_config:get_value(site, html_elt_extra, <<"embed,iframe,object,script">>, Context)},
        {attr_extra, m_config:get_value(site, html_attr_extra, <<"data,allowfullscreen,frameborder,scrolling,async,defer,allow">>, Context)},
        {element, fun(Element, Stack, Opts) -> sanitize_element(Element, Stack, Opts, Context) end}
    ].

default_options() ->
    [
        {elt_extra, <<>>},
        {attr_extra, <<>>},
        {element, fun sanitize_element_opts/3}
    ].


sanitize_element(Element, Stack, Opts, Context) ->
    case z_notifier:foldl(#sanitize_element{element=Element, stack=Stack}, Element, Context) of
        Element ->
            sanitize_element_1(Element, Stack, Opts, Context);
        NewElement ->
            NewElement
    end.

sanitize_element_1({<<"iframe">>, Props, _Inner}, _Stack, _Opts, Context) ->
    sanitize_iframe(Props, Context);
sanitize_element_1({<<"embed">>, Props, _Inner}, _Stack, _Opts, Context) ->
    sanitize_embed(Props, Context);
sanitize_element_1({<<"object">>, Props, []}, _Stack, _Opts, Context) ->
    sanitize_object(Props, Context);
sanitize_element_1({<<"object">>, _Props, Inner}, _Stack, _Opts, _Context) ->
    Inner;
sanitize_element_1({<<"script">>, Props, _Inner}, _Stack, _Opts, Context) ->
    sanitize_script(Props, Context);
sanitize_element_1(Element, Stack, Opts, Context) ->
    sanitize_element_opts(Element, Stack, Opts, Context).

sanitize_element_opts(Element, Stack, Opts) ->
    sanitize_element_opts(Element, Stack, Opts, undefined).

sanitize_element_opts({<<"a">>, Attrs, Inner} = Element, _Stack, _Opts, _Context) ->
    case proplists:is_defined(<<"target">>, Attrs) of
        true ->
            Attrs1 = [ Attr || Attr = {K,_} <- Attrs, K =/= <<"rel">> ],
            Attrs2 = [ {<<"rel">>, <<"noopener noreferrer">>} | Attrs1 ],
            {<<"a">>, Attrs2, Inner};
        false ->
            Element
    end;
sanitize_element_opts({comment, <<" [", _/binary>> = Comment} = Element, _Stack, _Opts, _Context) ->
    % Conditionals by Microsoft Word: <!-- [if (..)] (..) [endif]-->
    case binary:last(Comment) of
        $] -> <<>>;
        _ -> Element
    end;
sanitize_element_opts({comment, <<"[", _/binary>>}, _Stack, _Opts, _Context) ->
    % Conditional comment, as used for Outlook <!--[if mso]>..<![endif]-->
    <<>>;
sanitize_element_opts({comment, <<"<![">>}, _Stack, _Opts, _Context) ->
    % End of conditional comment, as used for Outlook <!--[if !mso]><!-->...<!--<![endif]-->
    <<>>;
sanitize_element_opts({comment, <<"StartFragment">>}, _Stack, _Opts, _Context) ->
    % Inserted by Microsoft Word: <!--StartFragment-->
    <<>>;
sanitize_element_opts({comment, <<"EndFragment">>}, _Stack, _Opts, _Context) ->
    % Inserted by Microsoft Word: <!--EndFragment-->
    <<>>;
sanitize_element_opts({comment, <<" z-media ", ZMedia/binary>>}, _Stack, _Opts, Context) ->
    % The z-media tag is very strict with spaces
    try
        [Id, Opts] = binary:split(ZMedia, <<" {">>),
        Opts1 = sanitize_z_media(<<${, Opts/binary>>),
        Id1 = z_string:to_name(z_string:trim(Id)),
        case Context =:= undefined orelse z_acl:rsc_visible(Id1, Context) of
            true ->
                {comment, <<" z-media ", Id1/binary, " ", Opts1/binary, " ">>};
            false ->
                ?LOG_NOTICE(#{
                    text => <<"Dropping invisibile media from z-media">>,
                    in => zotonic_core,
                    zmedia => ZMedia,
                    rsc_id => Id1
                }),
                <<" ">>
        end
    catch
        _:_ ->
            ?LOG_NOTICE(#{
                text => <<"Dropping illegal z-media tag">>,
                in => zotonic_core,
                zmedia => ZMedia
            }),
            <<" ">>
    end;
sanitize_element_opts({Tag, Attrs, Inner}, _Stack, _Opts, _Context) ->
    Attrs1 = cleanup_element_attrs(Attrs),
    {Tag, Attrs1, Inner};
sanitize_element_opts(Element, _Stack, _Opts, _Context) ->
    Element.

cleanup_element_attrs(Attrs) ->
    lists:filtermap(fun cleanup_element_attr/1, Attrs).

cleanup_element_attr({<<"class">>, Classes}) ->
    Classes1 = binary:split(Classes, <<" ">>, [global]),
    case lists:filter(fun is_acceptable_classname/1, Classes1) of
        [] -> false;
        Cs -> {true, {<<"class">>, iolist_to_binary(lists:join(32, Cs))}}
    end;
cleanup_element_attr({<<"style">>, <<"mso-", _/binary>>}) ->
    % This might need some extra parsing of the css.
    % For now we just drop styles starting with a "mso-" selector.
    false;
cleanup_element_attr({<<"allow">>, Allow}) ->
    List = binary:split(Allow, <<";">>, Allow),
    List1 = [ z_string:to_lower( z_string:trim(A) ) || A <- List ],
    List2 = lists:filter(fun sanitize_attr_allow/1, List1),
    List3 = lists:join(<<"; ">>, List2),
    iolist_to_binary(List3);
cleanup_element_attr(_Attr) ->
    true.

is_acceptable_classname(<<"Mso", _/binary>>) -> false;
is_acceptable_classname(<<>>) -> false;
is_acceptable_classname(_) -> true.

% Allowed feature policies for the iframe 'allow' attribute.
sanitize_attr_allow(<<"camera", _/binary>>) -> true;
sanitize_attr_allow(<<"microphone", _/binary>>) -> true;
sanitize_attr_allow(<<"midi", _/binary>>) -> true;
sanitize_attr_allow(<<"encrypted-media", _/binary>>) -> true;
sanitize_attr_allow(<<"autoplay", _/binary>>) -> true;
sanitize_attr_allow(<<"fullscreen", _/binary>>) -> true;
sanitize_attr_allow(<<"picture-in-picture", _/binary>>) -> true;
sanitize_attr_allow(<<"geolocation", _/binary>>) -> true;
sanitize_attr_allow(<<"gyroscope", _/binary>>) -> true;
sanitize_attr_allow(<<"accelerometer", _/binary>>) -> true;
sanitize_attr_allow(<<"ambient-light-sensor", _/binary>>) -> true;
sanitize_attr_allow(<<"magnetometer", _/binary>>) -> true;
sanitize_attr_allow(_) -> false.

sanitize_z_media(Data) ->
    Sanitized = maps:fold(
        fun(Key, Value, Acc) ->
            maps:merge(Acc, sanitize_z_media_arg(Key, Value))
        end,
        #{},
        z_json:decode(Data)
    ),
    z_json:encode(Sanitized).

sanitize_z_media_arg(<<"id">>, Id) when is_binary(Id) -> #{<<"id">> => z_string:to_name(Id)};
sanitize_z_media_arg(<<"id">>, Id) when is_integer(Id) -> #{<<"id">> => Id};
sanitize_z_media_arg(<<"size">>, <<"large">>) -> #{<<"size">> => <<"large">>};
sanitize_z_media_arg(<<"size">>, <<"small">>) -> #{<<"size">> => <<"small">>};
sanitize_z_media_arg(<<"size">>, <<"middle">>) -> #{<<"size">> => <<"middle">>};
sanitize_z_media_arg(<<"size">>, _) -> #{<<"size">> => <<"medium">>};
sanitize_z_media_arg(<<"align">>, <<"left">>) -> #{<<"align">> => <<"left">>};
sanitize_z_media_arg(<<"align">>, <<"right">>) -> #{<<"align">> => <<"right">>};
sanitize_z_media_arg(<<"align">>, _) -> #{<<"align">> => <<"block">>};
sanitize_z_media_arg(<<"crop">>, Crop) -> #{<<"crop">> => z_convert:to_bool(Crop)};
sanitize_z_media_arg(<<"link">>, Link) -> #{<<"link">> => z_convert:to_bool(Link)};
sanitize_z_media_arg(<<"link_new">>, LinkNew) -> #{<<"link_new">> => z_convert:to_bool(LinkNew)};
sanitize_z_media_arg(<<"link_url">>, LinkUrl) ->
    #{<<"link_url">> => z_html:sanitize_uri(z_string:trim(LinkUrl))};
sanitize_z_media_arg(<<"caption">>, Caption) ->
    #{<<"caption">> => binary:replace(Caption, <<"-->">>, <<"→"/utf8>>, [global])};
sanitize_z_media_arg(Key, Value) when is_binary(Value) ->
    #{z_string:to_name(Key) => binary:replace(Value, <<"-->">>, <<"→"/utf8>>, [global])};
sanitize_z_media_arg(Key, Value) when is_integer(Value); is_boolean(Value) ->
    #{z_string:to_name(Key) => Value}.

sanitize_script(Props, Context) ->
    Src = proplists:get_value(<<"src">>, Props),
    case to_allowed(Src, Context) of
        {ok, Url} ->
            {<<"script">>, [{<<"src">>,Url} | proplists:delete(<<"src">>, Props)], []};
        false ->
            ?LOG_NOTICE(#{
                text => <<"Dropped script with url">>,
                in => zotonic_core,
                url => Src
            }),
            <<>>
    end.

sanitize_iframe(Props, Context) ->
    Src = proplists:get_value(<<"src">>, Props),
    case to_allowed(Src, Context) of
        {ok, Url} ->
            {<<"iframe">>, [
                {<<"src">>,Url},
                {<<"sandbox">>, default_sandbox_attr(Context)}
                | proplists:delete(<<"src">>,
                    proplists:delete(<<"sandbox">>, Props))], []};
        false ->
            ?LOG_NOTICE(#{
                text => <<"Dropped iframe url">>,
                in => zotonic_core,
                url => Src
            }),
            <<>>
    end.

sanitize_object(Props, Context) ->
    Src = proplists:get_value(<<"data">>, Props),
    case maybe_youtube(Src, Props, Context) of
        {ok, YoutubeIframe} ->
            YoutubeIframe;
        false ->
            case to_allowed(Src, Context) of
                {ok, Url} ->
                    {<<"embed">>, [{<<"src">>,Url} | proplists:delete(<<"data">>, Props)], []};
                false ->
                    ?LOG_NOTICE(#{
                        text => <<"Dropped object url">>,
                        in => zotonic_core,
                        url => Src
                    }),
                    <<>>
            end
    end.

sanitize_embed(Props, Context) ->
    Src = proplists:get_value(<<"src">>, Props),
    case maybe_youtube(Src, Props, Context) of
        {ok, YoutubeIframe} ->
            YoutubeIframe;
        false ->
            case to_allowed(Src, Context) of
                {ok, Url} ->
                    {<<"embed">>, [{<<"src">>,Url} | proplists:delete(<<"src">>, Props)], []};
                false ->
                    ?LOG_NOTICE(#{
                        text => <<"Dropped embed url">>,
                        in => zotonic_core,
                        url => Src
                    }),
                    <<>>
            end
    end.

maybe_youtube(undefined, _Props, _Context) ->
    false;
maybe_youtube(Url, Props, Context) ->
    case binary:split(Url, <<"//">>) of
        [_,Loc] ->
            maybe_youtube_1(Loc, Props, Context);
        _ ->
            false
    end.

maybe_youtube_1(<<"www.youtube.com/v/", Rest/binary>>, Props, Context) ->
    [VideoCode|_] = binary:split(hd(binary:split(Rest, <<"?">>)), <<"&">>),
    make_iframe(<<"https://www.youtube.com/embed/", VideoCode/binary>>, Props, Context);
maybe_youtube_1(<<"www.youtube.com/embed/", _Rest/binary>> = EmbedUrl, Props, Context) ->
    make_iframe(<<"https://",EmbedUrl/binary>>, Props, Context);
maybe_youtube_1(_, _Props, _Context) ->
    false.

make_iframe(Url, Props, Context) ->
    {ok, {<<"iframe">>,
        [
            {<<"width">>, proplists:get_value(<<"width">>, Props, <<"480">>)},
            {<<"height">>, proplists:get_value(<<"height">>, Props, <<"360">>)},
            {<<"allowfullscreen">>, proplists:get_value(<<"allowfullscreen">>, Props, <<"1">>)},
            {<<"frameborder">>, <<"0">>},
            {<<"sandbox">>, default_sandbox_attr(Context)},
            {<<"src">>, Url}
        ],
        []}}.

to_allowed(undefined, _Context) ->
    false;
to_allowed(Url, Context) ->
    to_allowlist_1(binary:split(Url, <<"//">>), Context).

to_allowlist_1([Proto,Loc], Context) when Proto =:= <<>>; Proto =:= <<"http:">>; Proto =:= <<"https:">> ->
    case allowlist(Loc, Context) of
        {ok, Loc1} ->
            % Always use https - http is now defunct
            {ok, <<"https://", Loc1/binary>>};
        false ->
            false
    end;
to_allowlist_1(_, _Context) ->
    false.


allowlist(HostPath, Context) ->
    case z_notifier:first(#sanitize_embed_url{hostpath=HostPath}, Context) of
        undefined ->
            allowlist(HostPath);
        false ->
            false;
        HostPath1 when is_binary(HostPath1) ->
            {ok, HostPath1}
    end.


%% @doc Some allowed domains for embedding.
allowlist(<<"youtu.be/", Rest/binary>>) -> {ok, <<"www.youtube.com/", Rest/binary>>};
allowlist(<<"youtube.com/", Rest/binary>>) -> {ok, <<"www.youtube.com/", Rest/binary>>};
allowlist(<<"www.youtube.com/", _/binary>> = Url) -> {ok, Url};
allowlist(<<"player.vimeo.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"vimeo.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"www.slideshare.net/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"embed.spotify.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"api.soundcloud.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"w.soundcloud.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"cdn.knightlab.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"maps.google.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"www.google.com/maps/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"video.google.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"spreadsheets.google.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"docs.google.com/viewer?",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"instagram.com/embed.js">> = Url) -> {ok, Url};
allowlist(<<"www.instagram.com/embed.js">> = Url) -> {ok, Url};
allowlist(<<"vine.co/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"platform.instagram.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"www.hulu.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"www.metacafe.com/fplayer/", _/binary>> = Url) -> {ok, Url};
allowlist(<<"www.flickr.com/", _/binary>> = Url) -> {ok, Url};
allowlist(<<"flickrit.com/slideshowholder.php?", _/binary>> = Url) -> {ok, Url};
allowlist(<<"flv.video.yandex.ru/", _/binary>> = Url) -> {ok, Url};
allowlist(<<"www.tumblr.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"assets.tumblr.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"static.issuu.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"e.issuu.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"cdn.embedly.com/", _/binary>> = Url) -> {ok, Url};
allowlist(<<"vk.com/video_ext",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"platform.twitter.com/",  _/binary>> = Url) -> {ok, Url};
allowlist(<<"prezi.com/v/", _/binary>> = Url) -> {ok, Url};
allowlist(<<"prezi.com/embed/", _/binary>> = Url) -> {ok, Url};
allowlist(<<"open.spotify.com/embed/", _/binary>> = Url) -> {ok, Url};
allowlist(Url) ->
    case lists:dropwhile(fun(Re) ->
                            re:run(Url, Re) =:= nomatch
                         end,
                         allowlist_res())
    of
        [] -> false;
        [_|_] -> {ok, Url}
    end.

allowlist_res() ->
    [
        <<"^[a-z0-9\\-]+\\.tumblr.com/post/[0-9]+/audio_player_iframe/.*">>,
        <<"cdn.embedly.com/widgets/media.html\\?src=http%3A%2F%2F[a-z0-9-]+\\.ak\\.instagram.com%2F">>
    ].

