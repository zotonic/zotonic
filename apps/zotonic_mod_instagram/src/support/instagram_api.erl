%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Some Instagram API calls
%% See: http://instagram.com/developer/authentication/

%% Copyright 2015 Marc Worrell
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

-module(instagram_api).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    subscriptions/1,
    subscribe_tag/2,
    unsubscribe/2,
    unsubscribe_tags/1,
    tagged/2,
    tagged_nexturl/2,
    verify_token/1
]).


-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Return all subscriptions
-spec subscriptions(#context{}) -> {ok, list(list())} | {error, term()}.
subscriptions(Context) ->
    {ConsumerKey, ConsumerSecret, _Scope} = mod_instagram:get_config(Context),
    Url = "https://api.instagram.com/v1/subscriptions"
            ++ "?client_secret=" ++ z_url:url_encode(ConsumerSecret)
            ++ "&client_id=" ++ z_url:url_encode(ConsumerKey),
    case httpc:request(get, {Url, []}, httpc_http_options(), httpc_options()) of
        {ok, {{_Version, 200, _OK}, _Hs, Data}} ->
            {struct, MD} = mochijson:binary_decode(Data),
            {ok, [
                [
                    {id, v(<<"id">>, D)},
                    {object, v(<<"object">>, D)},
                    {object_id, v(<<"object_id">>, D)},
                    {aspect, v(<<"aspect">>, D)},
                    {callback_url, v(<<"callback_url">>, D)},
                    {type, v(<<"type">>, D)}
                ]
                || {struct, D} <- proplists:get_value(<<"data">>, MD)
            ]};
        {ok, Other} ->
            {error, Other};
        {error, _} = Error ->
            Error
    end.

v(K, Props) ->
    case proplists:get_value(K, Props) of
        null -> undefined;
        V -> V
    end.

%% @doc Subscribe to a tag on Instagram, new posted photos will be pushed.
subscribe_tag(Tag, Context) ->
    {ConsumerKey, ConsumerSecret, _Scope} = mod_instagram:get_config(Context),
    CallbackUrl = z_context:abs_url(
                    z_dispatcher:url_for(instagram_push, Context),
                    Context),
    Args = [
        {"client_id", ConsumerKey},
        {"client_secret", ConsumerSecret},
        {"object", "tag"},
        {"aspect", "media"},
        {"object_id", Tag},
        {"callback_url", CallbackUrl},
        {"verify_token", verify_token(Context)}
    ],
    Body = z_convert:to_binary(mochiweb_util:urlencode(Args)),
    httpc:request(post, {"https://api.instagram.com/v1/subscriptions/", [], "application/x-www-form-urlencoded", Body},
                  httpc_http_options(), httpc_options()).

%% @doc Delete a subscription id
unsubscribe(SubscriptionId, Context) ->
    {ConsumerKey, ConsumerSecret, _Scope} = mod_instagram:get_config(Context),
    Url = "https://api.instagram.com/v1/subscriptions"
            ++ "?client_secret=" ++ z_url:url_encode(ConsumerSecret)
            ++ "&client_id=" ++ z_url:url_encode(ConsumerKey)
            ++ "&id=" ++ z_url:url_encode(z_convert:to_binary(SubscriptionId)),
    httpc:request(delete, {Url, []}, httpc_http_options(), httpc_options()).

%% @doc Delete al tag subscriptions
unsubscribe_tags(Context) ->
    {ConsumerKey, ConsumerSecret, _Scope} = mod_instagram:get_config(Context),
    Url = "https://api.instagram.com/v1/subscriptions"
            ++ "?client_secret=" ++ z_url:url_encode(ConsumerSecret)
            ++ "&client_id=" ++ z_url:url_encode(ConsumerKey)
            ++ "&object=tag",
    httpc:request(delete, {Url, []}, httpc_http_options(), httpc_options()).


%% @doc Fetch the first page of media with a certain tag
tagged(Tag, Context) ->
    case z_convert:to_list(m_config:get_value(mod_instagram, access_token, Context)) of
        [] ->
            {error, access_token};
        AccessToken ->
            Url = iolist_to_binary([
                        "https://api.instagram.com/v1/tags/",
                        z_url:url_encode(Tag),
                        "/media/recent",
                        "?access_token=", z_url:url_encode(AccessToken)
                    ]),
            tagged_1(httpc:request(get, {z_convert:to_list(Url), []}, httpc_http_options(), httpc_options()))
    end.

tagged_nexturl(NextUrl, _Context) ->
    tagged_1(httpc:request(get, {z_convert:to_list(NextUrl), []}, httpc_http_options(), httpc_options())).


tagged_1({ok, {{_, 200, _}, _Hs, Body}}) ->
    {struct, Props} = mochijson:binary_decode(Body),
    {struct, Pagination} = proplists:get_value(<<"pagination">>, Props),
    NextUrl = proplists:get_value(<<"next_url">>, Pagination),
    Data = proplists:get_value(<<"data">>, Props),
    {ok, {NextUrl, Data}};
tagged_1({ok, _} = Result) ->
    {error, Result};
tagged_1({error, _} = Error) ->
    Error.



%% @doc Return the verification token to be used for subscription requests
verify_token(Context) ->
    case m_config:get_value(mod_instagram, verify_token, Context) of
        None when None =:= undefined; None =:= <<>> ->
            Tk = z_ids:id(),
            m_config:set_value(mod_instagram, verify_token, Tk, Context),
            Tk;
        Tk ->
            Tk
    end.

httpc_options() ->
    [
        {sync, true},
        {body_format, binary}
    ].

httpc_http_options() ->
    [
        {timeout, 30000},
        {connect_timeout, 10000},
        {autoredirect, true},
        {relaxed, true}
    ].

