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

-include("zotonic.hrl").

import_tweet({Tweet}, DefaultScreenName, Context) when is_list(Tweet) ->
    {<<"id">>, TweetId} = proplists:lookup(<<"id">>, Tweet),
    UniqueName = <<"tweet_", (z_convert:to_binary(TweetId))/binary>>,
    import_rsc(m_rsc:rid(UniqueName, Context), TweetId, UniqueName, DefaultScreenName, Tweet, Context);
import_tweet(Tweet, _DefaultScreenName, _Context) ->
    lager:info("Twitter: received unknown tweet data: ~p", [Tweet]),
    {error, tweet_format}.

import_rsc(undefined, TweetId, UniqueName, DefaultScreenName, Tweet, Context) ->
    {ok, ImportRsc} = extract_import_rsc(TweetId, UniqueName, Tweet, DefaultScreenName, Context),
    case z_notifier:first(ImportRsc, Context) of
        undefined ->
            do_import_rsc(TweetId, ImportRsc, Context);
        {ok, _} = OK ->
            OK;
        {error, _} = Error ->
            Error;
        Handled ->
            {ok, Handled}
    end;
import_rsc(_RscId, TweetId, _UniqueName, _User, _Tweet, _Context) ->
    lager:debug("Twitter: ignored duplicate tweet ~p", [TweetId]),
    {error, duplicate}.

do_import_rsc(TweetId, ImportRsc, Context) ->
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
    UserId = ImportRsc#import_resource.user_id,
    maybe_author(Result, UserId, AdminContext),
    lager:info("Twitter: imported tweet ~p for user_id ~p as ~p", [TweetId, UserId, Result]),
    Result.

maybe_author({ok, _RscId}, 1, _Context) ->
    ok;
maybe_author({ok, _RscId}, undefined, _Context) ->
    ok;
maybe_author({ok, RscId}, UserId, Context) ->
    _ = m_edge:insert(RscId, author, UserId, Context);
maybe_author(_NotOk, _UserId, _Context) ->
    ok.


extract_import_rsc(TweetId, UniqueName, Tweet, DefaultScreenName, Context) ->
    {User} = proplists:get_value(<<"user">>, Tweet),
    ScreenName = proplists:get_value(<<"screen_name">>, User, DefaultScreenName),
    TweetText = case proplists:get_value(<<"full_text">>, Tweet) of
        undefined -> proplists:get_value(<<"text">>, Tweet);
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
    Created = qdate:parse(proplists:get_value(<<"created_at">>, Tweet)),
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
        {publication_start, Created}
    ],
    TwitterUserId = proplists:get_value(id, User),
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
    case proplists:get_value(<<"lang">>, Tweet, null) of
        null ->
            z_context:language(Context);
        Lang ->
            [Iso|_] = binary:split(Lang, <<"-">>),
            case z_trans:to_language_atom(Iso) of
                {ok, Code} ->
                    Enabled = m_translation:language_list_enabled(Context),
                    case proplists:is_defined(Code, Enabled) of
                        true -> Code;
                        false -> z_context:language(Context)
                    end;
                {error, _} ->
                    z_context:language(Context)
            end
    end.

extract_coordinates(Tweet) ->
    case proplists:get_value(<<"coordinates">>, Tweet, null) of
        null ->
            {undefined, undefined};
        {Coordinates} ->
            case proplists:get_value(type, Coordinates) of
                <<"Point">> ->
                    [Longitude, Latitude] = proplists:get_value(<<"coordinates">>, Coordinates),
                    {Longitude, Latitude};
                _Other ->
                    {undefined, undefined}
            end
    end.

%% @doc Fetch urls and other links from the tweet, see: https://dev.twitter.com/overview/api/entities
extract_urls(Tweet) ->
    case proplists:get_value(<<"entities">>, Tweet, null) of
        null ->
            [];
        {Entitites} ->
            case proplists:get_value(<<"urls">>, Entitites) of
                undefined -> [];
                Urls -> [ url(Url) || Url <- Urls ]
            end
    end.

extract_media_urls(Tweet) ->
    case proplists:get_value(<<"entities">>, Tweet, null) of
        null ->
            [];
        {Entitites} ->
            Urls = extract_media_1(proplists:get_value(<<"media">>, Entitites, null), []),
            lists:unzip(Urls)
    end.

extract_media_1(null, Acc) ->
    lists:reverse(Acc);
extract_media_1([], Acc) ->
    lists:reverse(Acc);
extract_media_1([{M}|Ms], Acc) ->
    case proplists:get_value(<<"type">>, M) of
        <<"photo">> ->
            case proplists:get_value(<<"media_url_https">>, M) of
                undefined -> extract_media_1(Ms, Acc);
                MediaUrl ->
                    Url = proplists:get_value(<<"url">>, M),
                    extract_media_1(Ms, [ {MediaUrl, Url} |Acc])
            end;
        _Other ->
            extract_media_1(Ms, Acc)
    end.

url({UrlProps}) when is_list(UrlProps) ->
    proplists:get_value(<<"expanded_url">>, UrlProps).


%% Interactive test code
test(Context) ->
    Dir = "modules/mod_twitter/testdata/",
    {ok, Fs} = file:list_dir(Dir),
    lists:map(
        fun(F) ->
            case filename:extension(F) of
                ".txt" ->
                    {ok, [Tweet]} = file:consult(Dir++F),
                    extract_test_tweet(Tweet, Context);
                ".json" ->
                    {ok, Data} = file:read_file(Dir++F),
                    [Tweet] = jiffy:decode(Data),
                    extract_test_tweet(Tweet, Context);
                _ ->
                    {ignored, F}
            end
        end,
        Fs).

extract_test_tweet({Tweet}, Context) ->
    TweetId = proplists:get_value(<<"id">>, Tweet),
    UniqueName = <<"tweet_", (z_convert:to_binary(TweetId))/binary>>,
    extract_import_rsc(TweetId, UniqueName, Tweet, <<"test">>, z_acl:logon(1, Context)).
    % import_tweet(Tweet, <<"test">>, Context).
