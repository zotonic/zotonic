%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009-2014 Arjan Scherpenisse
%% @doc Import tweet data as a resource, extract media.

%% Copyright 2009-2014 Arjan Scherpenisse
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

-module(twitter_import_tweet).


%% 1. Map tweet to resource properties, URLs, original tweet, and possible author-ids
%% 2. Send notification with properties.
%% 3. If not handled, import iff author is known and subscribed

-export([
    import_tweet/2,
    extract_test/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_stdlib/include/z_url_metadata.hrl").

import_tweet(Tweet, Context) ->
    import_tweet_1(proplists:get_value(<<"user">>, Tweet), Tweet, Context).

import_tweet_1({User}, Tweet, Context) ->
    TweetId = proplists:get_value(<<"id">>, Tweet),
    UniqueName = <<"tweet_", (z_convert:to_binary(TweetId))/binary>>,
    import_rsc(m_rsc:rid(UniqueName, Context), TweetId, UniqueName, User, Tweet, Context);
import_tweet_1(_NoUser, Tweet, _Context) ->
    lager:info("Twitter: received unknown tweet data: ~p", [Tweet]).

import_rsc(_RscId, 0, _UniqueName, _User, _Tweet, _Context) ->
    % 2016-02-12: Twitter keeps pushing a tweet with id 0, drop it here.
    ok;
import_rsc(undefined, TweetId, UniqueName, User, Tweet, Context) ->
    {ok, ImportRsc, IsAutoImport} = extract_import_rsc(TweetId, UniqueName, User, Tweet, Context),
    case z_notifier:first(ImportRsc, Context) of
        undefined when IsAutoImport, is_integer(ImportRsc#import_resource.user_id) ->
            do_import_rsc(TweetId, ImportRsc, Context);
        undefined ->
            ScreenName = proplists:get_value(<<"screen_name">>, User),
            lager:debug("Twitter: ignored tweet ~p by @~s", [TweetId, ScreenName]),
            ok;
        _Handled ->
            ok
    end;
import_rsc(_RscId, TweetId, _UniqueName, _User, _Tweet, _Context) ->
    lager:debug("Twitter: ignored duplicate tweet ~p", [TweetId]).


do_import_rsc(TweetId, ImportRsc, Context) ->
    AdminContext = z_acl:sudo(Context),
    RscProps = [
        {category, tweet},
        {is_published, true}
        | ImportRsc#import_resource.props
    ],
    Result = case first_media_props(ImportRsc#import_resource.media_urls ++ ImportRsc#import_resource.urls, Context) of
                {ok, MI} ->
                    z_media_import:insert(MI#media_import_props{rsc_props=RscProps}, AdminContext);
                {error, _} ->
                    m_rsc:insert(RscProps, AdminContext)
             end,
    UserId = ImportRsc#import_resource.user_id,
    maybe_author(Result, UserId, AdminContext),
    lager:debug("Twitter: imported tweet ~p for user_id ~p as ~p", [TweetId, UserId, Result]),
    Result.

maybe_author({ok, RscId}, UserId, Context) ->
    {ok, _} = m_edge:insert(RscId, author, UserId, Context).


extract_import_rsc(TweetId, UniqueName, User, Tweet, Context) ->
    ScreenName = proplists:get_value(<<"screen_name">>, User),
    TweetText = proplists:get_value(<<"text">>, Tweet),
    LinkUrls = extract_urls(Tweet),
    MediaUrls = extract_media_urls(Tweet),
    TweetUrl = iolist_to_binary([
            "https://twitter.com/", ScreenName, "/status/", z_convert:to_binary(TweetId)
        ]),
    Body = iolist_to_binary([
                <<"<p>">>,
                filter_twitter:twitter(TweetText, [url_location], Context),
                <<"</p>">>
            ]),
    Language = extract_language(Tweet, Context),
    {Long, Lat} = extract_coordinates(Tweet),
    Props = [
        {name, UniqueName},
        {language, [Language]},
        {title, {trans, [
                    {Language, iolist_to_binary([
                                ScreenName, ": ",
                                z_string:truncate(z_html:unescape(z_html:strip(Body)), 50)])}
                 ]}},
        {body, {trans, [{Language, Body}]}},
        {location_lng, Long},
        {location_lat, Lat},
        {website, first_link(LinkUrls, TweetUrl)}
    ],
    TweeterId = proplists:get_value(<<"id">>, User),
    {UserId, IsAutoImport} = find_user(TweeterId, Context),
    {ok, #import_resource{
        source = twitter,
        source_id = TweetId,
        source_url = TweetUrl,
        source_user_id = TweeterId,
        user_id = UserId,
        name = UniqueName,
        props = Props,
        media_urls = MediaUrls,
        urls = LinkUrls,
        data = Tweet
    }, IsAutoImport}.

first_link([Url|_], _) -> Url;
first_link(_, Url) -> Url.

-spec first_media_props([#url_metadata{} | string() | binary()], #context{}) -> {error, nomedia} | {ok, pos_integer()}.
first_media_props([], _Context) ->
    {error, nomedia};
first_media_props([Url|Urls], Context) ->
    case z_media_import:url_import_props(Url, Context) of
        {ok, []} ->
            first_media_props(Urls, Context);
        {ok, [MI|_]} ->
            {ok, MI};
        {error, _} ->
            first_media_props(Urls, Context)
    end.

extract_language(Tweet, Context) ->
    case proplists:get_value(<<"lang">>, Tweet) of
        undefined ->
            z_context:language(Context);
        Lang ->
            [LanguageCode|_] = binary:split(Lang, <<"-">>),
            case z_language:to_language_atom(LanguageCode) of
                {ok, Code} ->
                    Enabled = m_translation:language_list_enabled(Context),
                    case proplists:is_defined(LanguageCode, Enabled) of
                        true -> Code;
                        false -> z_context:language(Context)
                    end;
                {error, _} ->
                    z_context:language(Context)
            end
    end.

extract_coordinates(Tweet) ->
    case proplists:get_value(<<"">>, Tweet) of
        {Coordinates} ->
            case proplists:get_value(<<"type">>, Coordinates) of
                <<"Point">> ->
                    [Longitude, Latitude] = proplists:get_value(<<"coordinates">>, Coordinates),
                    {Longitude, Latitude};
                _Other ->
                    {undefined, undefined}
            end;
        undefined ->
            {undefined, undefined}
    end.

find_user(TweeterId, Context) ->
    TwId = z_convert:to_binary(TweeterId),
    case m_identity:lookup_by_type_and_key(twitter_id, TwId, Context) of
        undefined ->
            case m_identity:lookup_by_type_and_key(twitter, TwId, Context) of
                undefined ->
                    {undefined, false};
                R ->
                    {proplists:get_value(rsc_id, R), false}
            end;
        R ->
            {proplists:get_value(rsc_id, R), true}
    end.


%% @doc Fetch urls and other links from the tweet, see: https://dev.twitter.com/overview/api/entities
extract_urls(Tweet) ->
    case proplists:get_value(<<"entities">>, Tweet) of
        {Entitites} ->
            case proplists:get_value(<<"urls">>, Entitites) of
                undefined -> [];
                Urls -> [ url(Url) || Url <- Urls ]
            end;
        undefined ->
            []
    end.

extract_media_urls(Tweet) ->
    case proplists:get_value(<<"entities">>, Tweet) of
        {Entitites} ->
            extract_media_1(proplists:get_value(<<"media">>, Entitites), []);
        undefined ->
            []
    end.

extract_media_1(undefined, Acc) ->
    lists:reverse(Acc);
extract_media_1([], Acc) ->
    lists:reverse(Acc);
extract_media_1([{M}|Ms], Acc) ->
    case proplists:get_value(<<"type">>, M) of
        <<"photo">> ->
            case proplists:get_value(<<"media_url_https">>, M) of
                undefined -> extract_media_1(Ms, Acc);
                Url -> extract_media_1(Ms, [Url|Acc])
            end;
        _Other ->
            extract_media_1(Ms, Acc)
    end.

url({UrlProps}) ->
    proplists:get_value(<<"expanded_url">>, UrlProps).


%% Interactive test code
extract_test(Filename, Context) ->
    {ok, Data} = file:read_file("modules/mod_twitter/testdata/"++Filename),
    [{Decoded}] = jiffy:decode(Data),
    extract_test_tweet(Decoded, Context).

extract_test_tweet(Tweet, Context) ->
    {User} = proplists:get_value(<<"user">>, Tweet),
    TweetId = proplists:get_value(<<"id">>, Tweet),
    UniqueName = <<"tweet_", (z_convert:to_binary(TweetId))/binary>>,
    extract_import_rsc(TweetId, UniqueName, User, Tweet, Context).

