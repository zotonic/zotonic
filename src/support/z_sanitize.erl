%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Interface to z_html sanitizers, sets options and adds embed sanitization.

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

-module(z_sanitize).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    uri/1,
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

uri(Uri) ->
    z_html:sanitize_uri(Uri).

escape_props(Props) ->
    z_html:escape_props(Props, default_options()).

escape_props(Props, Context) ->
    z_html:escape_props(Props, context_options(Context)).

escape_props_check(Props) ->
    z_html:escape_props_check(Props, default_options()).

escape_props_check(Props, Context) ->
    z_html:escape_props_check(Props, context_options(Context)).

escape_link({trans, Tr}) ->
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
        {attr_extra, m_config:get_value(site, html_attr_extra, <<"data,allowfullscreen,flashvars,frameborder,scrolling,async,defer">>, Context)},
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
sanitize_element_1(Element, Stack, Opts, _Context) ->
    sanitize_element_opts(Element, Stack, Opts).


sanitize_element_opts({<<"a">>, Attrs, Inner} = Element, _Stack, _Opts) ->
    case proplists:is_defined(<<"target">>, Attrs) of
        true ->
            Attrs1 = [ Attr || Attr = {K,_} <- Attrs, K =/= <<"rel">> ],
            Attrs2 = [ {<<"rel">>, <<"noopener noreferrer">>} | Attrs1 ],
            {<<"a">>, Attrs2, Inner};
        false ->
            Element
    end;
sanitize_element_opts({comment, <<" [", _/binary>> = Comment} = Element, _Stack, _Opts) ->
    % Conditionals by Microsoft Word: <!-- [if (..)] (..) [endif]-->
    case binary:last(Comment) of
        $] -> <<>>;
        _ -> Element
    end;
sanitize_element_opts({comment, <<"StartFragment">>}, _Stack, _Opts) ->
    % Inserted by Microsoft Word: <!--StartFragment-->
    <<>>;
sanitize_element_opts({comment, <<"EndFragment">>}, _Stack, _Opts) ->
    % Inserted by Microsoft Word: <!--EndFragment-->
    <<>>;
sanitize_element_opts({Tag, Attrs, Inner}, _Stack, _Opts) ->
    Attrs1 = cleanup_element_attrs(Attrs),
    {Tag, Attrs1, Inner};
sanitize_element_opts(Element, _Stack, _Opts) ->
    Element.

cleanup_element_attrs(Attrs) ->
    lists:filtermap(fun cleanup_element_attr/1, Attrs).

cleanup_element_attr({<<"class">>, Classes}) ->
    Classes1 = binary:split(Classes, <<" ">>, [global]),
    case lists:filter(fun is_acceptable_classname/1, Classes1) of
        [] -> false;
        Cs -> {true, {<<"class">>, iolist_to_binary(z_utils:combine(32, Cs))}}
    end;
cleanup_element_attr({<<"style">>, <<"mso-", _/binary>>}) ->
    % This might need some extra parsing of the css.
    % For now we just drop styles starting with a "mso-" selector.
    false;
cleanup_element_attr(_Attr) ->
    true.

is_acceptable_classname(<<"Mso", _/binary>>) -> false;
is_acceptable_classname(<<>>) -> false;
is_acceptable_classname(_) -> true.


sanitize_script(Props, Context) ->
    Src = proplists:get_value(<<"src">>, Props),
    case to_whitelisted(Src, Context) of
        {ok, Url} ->
            {<<"script">>, [{<<"src">>,Url} | proplists:delete(<<"src">>, Props)], []};
        false ->
            lager:info("Dropped script with url ~p", [Src]),
            <<>>
    end.

sanitize_iframe(Props, Context) ->
    Src = proplists:get_value(<<"src">>, Props),
    case to_whitelisted(Src, Context) of
        {ok, Url} ->
            {<<"iframe">>, [{<<"src">>,Url} | proplists:delete(<<"src">>, Props)], []};
        false ->
            lager:info("Dropped iframe url ~p", [Src]),
            <<>>
    end.

sanitize_object(Props, Context) ->
    Src = proplists:get_value(<<"data">>, Props),
    case maybe_embed2iframe(Src, Props) of
        {ok, NewElement} ->
            NewElement;
        false ->
            case to_whitelisted(Src, Context) of
                {ok, Url} ->
                    {<<"embed">>, [{<<"src">>,Url} | proplists:delete(<<"data">>, Props)], []};
                false ->
                    lager:info("Dropped object url ~p", [Src]),
                    <<>>
            end
    end.

sanitize_embed(Props, Context) ->
    Src = proplists:get_value(<<"src">>, Props),
    case maybe_embed2iframe(Src, Props) of
        {ok, NewElement} ->
            NewElement;
        false ->
            case to_whitelisted(Src, Context) of
                {ok, Url} ->
                    {<<"embed">>, [{<<"src">>,Url} | proplists:delete(<<"src">>, Props)], []};
                false ->
                    lager:info("Dropped embed url ~p", [Src]),
                    <<>>
            end
    end.

maybe_embed2iframe(undefined, _Props) ->
    false;
maybe_embed2iframe(Url, Props) ->
    case binary:split(Url, <<"//">>) of
        [_,Loc] ->
            maybe_embed2iframe_1(Loc, Props);
        _ ->
            false
    end.

maybe_embed2iframe_1(<<"www.youtube.com/v/", Rest/binary>>, Props) ->
    [VideoCode|_] = binary:split(hd(binary:split(Rest, <<"?">>)), <<"&">>),
    make_iframe(<<"https://www.youtube.com/embed/", VideoCode/binary>>, Props);
maybe_embed2iframe_1(<<"www.youtube.com/embed/", _Rest/binary>> = EmbedUrl, Props) ->
    make_iframe(<<"https://",EmbedUrl/binary>>, Props);
maybe_embed2iframe_1(_, _Props) ->
    false.

make_iframe(Url, Props) ->
    {ok, {<<"iframe">>,
        [
            {<<"width">>, proplists:get_value(<<"width">>, Props, <<"480">>)},
            {<<"height">>, proplists:get_value(<<"height">>, Props, <<"360">>)},
            {<<"allowfullscreen">>, proplists:get_value(<<"allowfullscreen">>, Props, <<"1">>)},
            {<<"frameborder">>, <<"0">>},
            {<<"src">>, maybe_append_flashvars(Url, proplists:get_value(<<"flashvars">>, Props) )}
        ],
        []}}.

maybe_append_flashvars(Url, undefined) ->
    Url;
maybe_append_flashvars(Url, <<>>) ->
    Url;
maybe_append_flashvars(Url, FlashVars) ->
    iolist_to_binary([ Url, $?, z_convert:to_binary(z_url:url_path_encode(FlashVars)) ]).

to_whitelisted(undefined, _Context) ->
    false;
to_whitelisted(Url, Context) ->
    to_whitelist_1(binary:split(Url, <<"//">>), Context).

to_whitelist_1([Proto,Loc], Context) when Proto =:= <<>>; Proto =:= <<"http:">>; Proto =:= <<"https:">> ->
    case wl(Loc, Context) of
        {ok, Loc1} ->
            Proto1 = case preferred_protocol(Loc1) of
                        undefined -> Proto;
                        P -> P
                     end,
            {ok, <<Proto1/binary, "//", Loc1/binary>>};
        false ->
            false
    end;
to_whitelist_1(_, _Context) ->
    false.

preferred_protocol(<<"www.youtube.", _/binary>>) -> <<>>;
preferred_protocol(<<"vimeo.", _/binary>>) -> <<>>;
preferred_protocol(<<"www.vimeo.", _/binary>>) -> <<>>;
preferred_protocol(<<"platform.instagram.com", _/binary>>) -> <<>>;
preferred_protocol(<<"www.flickr.", _/binary>>) -> <<"https:">>;
preferred_protocol(_) -> undefined.


wl(HostPath, Context) ->
    case z_notifier:first(#sanitize_embed_url{hostpath=HostPath}, Context) of
        undefined ->
            wl(HostPath);
        false ->
            false;
        HostPath1 ->
            {ok, HostPath1}
    end.


%% @doc Some whitelisted domains for embedding.
wl(<<"youtu.be/", Rest/binary>>) -> {ok, <<"www.youtube.com/", Rest/binary>>};
wl(<<"youtube.com/", Rest/binary>>) -> {ok, <<"www.youtube.com/", Rest/binary>>};
wl(<<"www.youtube.com/", _/binary>> = Url) -> {ok, Url};
wl(<<"player.vimeo.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"vimeo.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"www.slideshare.net/",  _/binary>> = Url) -> {ok, Url};
wl(<<"embed.spotify.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"api.soundcloud.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"w.soundcloud.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"cdn.knightlab.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"maps.google.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"www.google.com/maps/",  _/binary>> = Url) -> {ok, Url};
wl(<<"video.google.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"spreadsheets.google.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"docs.google.com/viewer?",  _/binary>> = Url) -> {ok, Url};
wl(<<"vine.co/",  _/binary>> = Url) -> {ok, Url};
wl(<<"instagram.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"platform.instagram.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"www.hulu.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"www.metacafe.com/fplayer/", _/binary>> = Url) -> {ok, Url};
wl(<<"www.flickr.com/", _/binary>> = Url) -> {ok, Url};
wl(<<"flickrit.com/slideshowholder.php?", _/binary>> = Url) -> {ok, Url};
wl(<<"flv.video.yandex.ru/", _/binary>> = Url) -> {ok, Url};
wl(<<"www.tumblr.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"assets.tumblr.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"static.issuu.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"e.issuu.com/",  _/binary>> = Url) -> {ok, Url};
wl(<<"cdn.embedly.com/", _/binary>> = Url) -> {ok, Url};
wl(Url) ->
    case lists:dropwhile(fun(Re) ->
                            re:run(Url, Re) =:= nomatch
                         end,
                         wl_res())
    of
        [] -> false;
        [_|_] -> {ok, Url}
    end.

wl_res() ->
    [
        <<"^[a-z0-9\\-]+\\.tumblr.com/post/[0-9]+/audio_player_iframe/.*">>,
        <<"cdn.embedly.com/widgets/media.html\\?src=http%3A%2F%2F[a-z0-9-]+\\.ak\\.instagram.com%2F">>
    ].

