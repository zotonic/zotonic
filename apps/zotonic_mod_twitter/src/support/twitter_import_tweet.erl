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

%% @TODO: check the identity table for possible twitter feed ownership (and by extension author-edge)

-export([
    import_tweet/3,
    test/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_stdlib/include/z_url_metadata.hrl").

import_tweet(#{ <<"id">> := TweetId } = Tweet, AuthorId, Context) ->
    UniqueName = <<"tweet_", (z_convert:to_binary(TweetId))/binary>>,
    import_rsc(m_rsc:rid(UniqueName, Context), TweetId, UniqueName, AuthorId, Tweet, Context);
import_tweet(Tweet, _AuthorId, _Context) ->
    lager:info("Twitter: received unknown tweet data: ~p", [Tweet]),
    {error, tweet_format}.

import_rsc(undefined, TweetId, UniqueName, AuthorId, Tweet, Context) ->
    {ok, ImportRsc} = extract_import_rsc(TweetId, UniqueName, Tweet, Context),
    case z_notifier:first(ImportRsc, Context) of
        undefined ->
            do_import_rsc(TweetId, ImportRsc, AuthorId, Tweet, Context);
        {ok, _} = OK ->
            OK;
        {error, _} = Error ->
            Error;
        Handled ->
            {ok, Handled}
    end;
import_rsc(_RscId, TweetId, _UniqueName, _AuthorId, _Tweet, _Context) ->
    lager:debug("Twitter: ignored duplicate tweet ~p", [TweetId]),
    {error, duplicate}.

do_import_rsc(TweetId, ImportRsc, AuthorId, Tweet, Context) ->
    AdminContext = z_acl:sudo(Context),
    RscProps = [
        {category, tweet},
        {is_published, true}
        | ImportRsc#import_resource.props
    ],
    Result = case first_media_props(ImportRsc#import_resource.media_urls, Context) of
                {ok, MI} ->
                    z_media_import:insert(MI#media_import_props{rsc_props=RscProps}, AdminContext);
                {error, _} ->
                    m_rsc:insert(RscProps, AdminContext)
             end,
    % Find the author for the author edge
    maybe_author(Result, AuthorId, Tweet, AdminContext),
    lager:info("Twitter: imported tweet ~p for user_id ~p as ~p", [TweetId, z_acl:user(Context), Result]),
    Result.

%% @doc If the importing user_id is defined, or if the Twitter user is coupled to an identity
%%      then add an author edge.
maybe_author({ok, RscId}, undefined, #{ <<"user">> := User }, Context) ->
    ScreenName = maps:get(<<"screen_name">>, User, undefined),
    #{ <<"id">> := TwitterUserId } = User,
    UIdBin = z_convert:to_binary(TwitterUserId),
    Idns = [
        <<"@", ScreenName/binary>>,
        UIdBin,
        <<"@#", UIdBin/binary>>
    ],
    AuthorId = lists:foldl(
        fun
            (Key, undefined) ->
                case m_identity:lookup_by_type_and_key(twitter_id, Key, Context) of
                    undefined -> undefined;
                    Idn -> proplists:get_value(rsc_id, Idn)
                end;
            (_Key, UserId) ->
                UserId
        end,
        undefined,
        Idns),
    case AuthorId of
        undefined -> ok;
        _ -> m_edge:insert(RscId, author, AuthorId, Context)
    end;
maybe_author({ok, RscId}, UserId, _Tweet, Context) ->
    m_edge:insert(RscId, author, UserId, Context);
maybe_author(_NotOk, _UserId, _Tweet, _Context) ->
    ok.

get_value(K, Props) ->
    case maps:get(K, Props, null) of
        null -> undefined;
        V -> V
    end.


extract_import_rsc(TweetId, UniqueName, #{ <<"user">> := User } = Tweet, Context) ->
    ScreenName = get_value(<<"screen_name">>, User),
    #{ <<"id">> := TwitterUserId } = User,
    TweetText = case get_value(<<"full_text">>, Tweet) of
        undefined -> get_value(<<"text">>, Tweet);
        Txt -> Txt
    end,
    LinkUrls = extract_urls(Tweet),
    {MediaUrls, ShortMediaUrls} = extract_media_urls(Tweet),
    TweetUrl = iolist_to_binary([
            "https://twitter.com/", ScreenName, "/status/", z_convert:to_binary(TweetId)
        ]),
    Body = iolist_to_binary([
                <<"<p>">>,
                filter_twitter:twitter(
                    TweetText,
                    [ url_location, {drop_urls, ShortMediaUrls} ],
                    Context),
                <<"</p>">>
            ]),
    Language = extract_language(Tweet, Context),
    {Long, Lat} = extract_coordinates(Tweet),
    Created = qdate:parse(maps:get(<<"created_at">>, Tweet)),
    TweetProps = [
        {tweet_id, TweetId},
        {tweet_url, TweetUrl},
        {user, [
            {is_verified, z_convert:to_bool(get_value(<<"verified">>, User))},
            {id, TwitterUserId},
            {screen_name, ScreenName},
            {name, get_value(<<"name">>, User)},
            {image_url, get_value(<<"profile_image_url_https">>, User)},
            {url, <<"https://twitter.com/", ScreenName/binary>>}
        ]},
        {in_reply_to_user_id, get_value(<<"in_reply_to_user_id">>, Tweet)},
        {in_reply_to_screen_name, get_value(<<"in_reply_to_screen_name">>, Tweet)},
        {in_reply_to_status_id, get_value(<<"in_reply_to_status_id">>, Tweet)},
        {is_retweet_status, get_value(<<"retweeted_status">>, Tweet) =/= undefined},
        {retweeted_status_url, retweeted_status_url(Tweet)},
        {retweeted_status_id, retweeted_status_id(Tweet)},
        {is_quote_status, z_convert:to_bool(get_value(<<"is_quote_status">>, Tweet))},
        {quoted_status_url, quoted_status_url(Tweet)}
    ],
    TweetProps1 = [ {K, V} || {K, V} <- TweetProps, V =/= undefined ],
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
        {website, first_link(LinkUrls, TweetUrl)},
        {org_pubdate, Created},
        {publication_start, Created},
        {tweet, TweetProps1}
    ],
    {ok, #import_resource{
        source = twitter,
        source_id = TweetId,
        source_url = TweetUrl,
        source_user_id = TwitterUserId,
        user_id = z_acl:user(Context),
        name = UniqueName,
        props = Props,
        media_urls = MediaUrls,
        urls = LinkUrls,
        data = Tweet
    }}.

first_link([Url|_], _) -> Url;
first_link(_, Url) -> Url.

-spec first_media_props([#url_metadata{} | string() | binary()], z:context()) -> {error, nomedia} | {ok, pos_integer()}.
first_media_props([], _Context) ->
    {error, nomedia};
first_media_props([ Url | Urls ], Context) ->
    case z_media_import:url_import_props(Url, Context) of
        {ok, []} ->
            first_media_props(Urls, Context);
        {ok, [ MI | _ ]} ->
            {ok, MI};
        {error, _} ->
            first_media_props(Urls, Context)
    end.

quoted_status_url(#{ <<"quoted_status_permalink">> := Qs }) ->
    get_value(<<"expanded">>, Qs);
quoted_status_url(_Tweet) ->
    undefined.

retweeted_status_url(#{ <<"retweeted_status">> := RT }) ->
    case get_value(<<"id">>, RT) of
        undefined -> undefined;
        Id ->
            #{ <<"user">> := RTUser } = RT,
            #{ <<"screen_name">> := RTScreenname } = RTUser,
            <<"https://twitter.com/", RTScreenname/binary,
              "/status/", (z_convert:to_binary(Id))/binary>>
    end;
retweeted_status_url(_Tweet) ->
    undefined.

retweeted_status_id(#{ <<"retweeted_status">> := RT }) ->
    maps:get(<<"id">>, RT);
retweeted_status_id(_Tweet) ->
    undefined.

extract_language(#{ <<"lang">> := Lang }, Context) when is_binary(Lang) ->
    [LanguageCode|_] = binary:split(Lang, <<"-">>),
    case z_language:to_language_atom(LanguageCode) of
        {ok, Code} ->
            case z_language:is_language_enabled(Code, Context) of
                true -> Code;
                false -> z_context:language(Context)
            end;
        {error, _} ->
            z_context:language(Context)
    end;
extract_language(_Tweet, Context) ->
    z_context:language(Context).

extract_coordinates(#{ <<"coordinates">> := Coordinates }) when is_map(Coordinates) ->
    case maps:get(<<"type">>, Coordinates) of
        <<"Point">> ->
            [Longitude, Latitude] = maps:get(<<"coordinates">>, Coordinates),
            {Longitude, Latitude};
        _Other ->
            {undefined, undefined}
    end;
extract_coordinates(_Tweet) ->
    {undefined, undefined}.

%% @doc Fetch urls and other links from the tweet, see: https://dev.twitter.com/overview/api/entities
extract_urls(#{ <<"entities">> := Entitites }) when is_map(Entitites) ->
    Urls = maps:get(<<"urls">>, Entitites, []),
    [ url(Url) || Url <- Urls ];
extract_urls(_Tweet) ->
    [].

extract_media_urls(#{ <<"entities">> := Entitites }) when is_map(Entitites) ->
    Urls = extract_media_1(maps:get(<<"media">>, Entitites, null), []),
    lists:unzip(Urls);
extract_media_urls(_Tweet) ->
    [].

extract_media_1(null, Acc) ->
    lists:reverse(Acc);
extract_media_1([], Acc) ->
    lists:reverse(Acc);
extract_media_1([ #{ <<"type">> := <<"photo">>, <<"media_url_https">> := MediaUrl, <<"url">> := Url } | Ms ], Acc) ->
    extract_media_1(Ms, [ {MediaUrl, Url} |Acc]);
extract_media_1([ _ | Ms ], Acc) ->
    extract_media_1(Ms, Acc).

url(#{ <<"expanded_url">> := Url }) ->
    Url;
url(_) ->
    undefined.


%% Interactive test code
test(Context) ->
        Dir = "apps/mod_twitter/testdata/",
        {ok, Fs} = file:list_dir(Dir),
        lists:map(
            fun(F) ->
                case filename:extension(F) of
                    ".txt" ->
                        {ok, [Tweet]} = file:consult(Dir++F),
                        extract_test_tweet(Tweet, Context);
                    ".json" ->
                        {ok, Data} = file:read_file(Dir++F),
                        Tweet = z_json:decode(Data),
                        extract_test_tweet(Tweet, Context);
                    _ ->
                        {ignored, F}
                end
            end,
            Fs).

extract_test_tweet(Tweet, Context) ->
    TweetId = proplists:get_value(<<"id">>, Tweet),
    UniqueName = <<"tweet_", (z_convert:to_binary(TweetId))/binary>>,
    extract_import_rsc(TweetId, UniqueName, Tweet, z_acl:logon(1, Context)).
    % import_tweet(Tweet, <<"test">>, Context).
