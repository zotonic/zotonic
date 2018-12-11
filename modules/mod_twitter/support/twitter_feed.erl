%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Driebit BV
%% @doc Fetch feed for twitter username of tag.

%% Copyright 2018 Driebit BV
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

-module(twitter_feed).

-export([
    poll/3,
    poll_next/2,

    get_tweet/2
]).

%% @doc Fetch the timeline of an user or query for a phrase or tag.
%%      Returns a list with the 'next' args for poll_next/2, the tweets found
%%      and the max-id of all tweets. Retweets are filtered from the query but
%%      not from the timeline.
-spec poll( binary(), integer() | undefined, z:context() ) -> {ok, list()} | {error, term()}.
poll(<<"@", Username/binary>>, SinceId, Context) ->
    % Use <<"@username">>, or <<"@#userid">> for the timeline poll.
    Args = [
        {"count", "200"},
        {"trim_user", "false"},         % Minimal 'user' info in Tweet?
        {"include_rts", "true"},        % Retweets?
        {"exclude_replies", "true"},    % Exclude replies to other users?
        {"tweet_mode", "extended"}
    ],
    Args1 = case Username of
        <<"#", TwUserId/binary>> ->
            [ {"user_id", z_convert:to_list(TwUserId)} | Args ];
        _ ->
            [ {"screen_name", z_convert:to_list(Username)} | Args ]
    end,
    Args2 = case SinceId of
        0 -> Args;
        undefined -> Args;
        _ -> [ {"since_id", z_convert:to_list(SinceId)} | Args1 ]
    end,
    case fetch("statuses/user_timeline", Args2, Context) of
        {ok, Tweets} when is_list(Tweets) ->
            {ok, [
                {next, undefined},
                {max_id, max_id(Tweets)},
                {tweets, Tweets}
            ]};
        {error, _} = Error ->
            Error
    end;
poll(TagOrPhrase, SinceId, Context) ->
    Args = [
        {"q", z_convert:to_list(TagOrPhrase)},
        {"count", "100"},
        {"result_type", "recent"},
        {"include_entities", "true"},
        {"tweet_mode", "extended"}
    ],
    Args1 = case SinceId of
        0 -> Args;
        undefined -> Args;
        _ -> [ {"since_id", z_convert:to_list(SinceId)} | Args ]
    end,
    case fetch("search/tweets", Args1, Context) of
        {ok, { Result }} ->
            {<<"statuses">>, Tweets} = proplists:lookup(<<"statuses">>, Result),
            {<<"search_metadata">>, {Meta}} = proplists:lookup(<<"search_metadata">>, Result),
            {<<"max_id">>, MaxId} = proplists:lookup(<<"max_id">>, Meta),
            Next = proplists:get_value(<<"next_results">>, Meta, null),
            {ok, [
                {next, {"search/tweets", parse_qs(Next)}},
                {max_id, MaxId},
                {tweets, filter_retweets(Tweets)}
            ]};
        {error, _} = Error ->
            Error
    end.

poll_next({_API, undefined}, _Context) ->
    {ok, [
        {next, []},
        {tweets, []}
    ]};
poll_next({API, Args}, Context) ->
    case fetch(API, Args, Context) of
        {ok, { Result }} ->
            {<<"statuses">>, Tweets} = proplists:lookup(<<"statuses">>, Result),
            {<<"search_metadata">>, {Meta}} = proplists:lookup(<<"search_metadata">>, Result),
            {<<"max_id">>, MaxId} = proplists:lookup(<<"max_id">>, Meta),
            Next = proplists:get_value(<<"next_results">>, Meta, null),
            {ok, [
                {next, {API, parse_qs(Next)}},
                {max_id, MaxId},
                {tweets, filter_retweets(Tweets)}
            ]};
        {error, _} = Error ->
            Error
    end.

parse_qs(<<"?", Qs/binary>>) ->
    [ {"tweet_mode", "extended"} | mochiweb_util:parse_qs(Qs) ];
parse_qs(null) ->
    undefined.

max_id(undefined) ->
    undefined;
max_id([]) ->
    undefined;
max_id(Tweets) ->
    lists:max( [ proplists:get_value(<<"id">>, T) || {T} <- Tweets ] ).


%% @doc Remove all retweets from the list of tweets
filter_retweets(Tweets) ->
    lists:filter(
        fun( {Tweet} ) ->
            TweetText = case proplists:get_value(<<"full_text">>, Tweet) of
                undefined -> proplists:get_value(<<"text">>, Tweet);
                Txt -> Txt
            end,
            case TweetText of
                <<"RT ", _/binary>> -> false;
                _ -> true
            end
        end,
        Tweets).


get_tweet(TweetId, Context) when is_integer(TweetId) ->
    Args = [
        {"id", z_convert:to_list(TweetId)},
        {"trim_user", "false"},           % Minimal 'user' info in Tweet
        {"include_entities", "true"},
        {"tweet_mode", "extended"}
    ],
    fetch("statuses/show", Args, Context).


fetch(API, Args, Context) ->
    lager:debug("Twitter: fetching \"~s\" for ~p", [ API, Args ]),
    Access = {
        z_convert:to_list(m_config:get_value(mod_twitter, access_token, Context)),
        z_convert:to_list(m_config:get_value(mod_twitter, access_token_secret, Context))
    },
    oauth_twitter_client:request(get, API, Args, Access, Context).

