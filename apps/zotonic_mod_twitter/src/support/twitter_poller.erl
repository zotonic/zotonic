%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Driebit BV
%% @doc Task queue handler for Twitter polls
%% @todo Split all poll actions into separate jobs

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

-module(twitter_poller).

-export([
    poll/1,
    poll_next/3
]).

% Delays in seconds
-define(DELAY_MINIMUM, 5).
-define(DELAY_ACTIVE, 10).
-define(DELAY_RATE_LIMIT, 15*60).
-define(DELAY_5XX_ERROR, 10*60).
-define(DELAY_EXCEPTION, 3600).
-define(DELAY_MAXIMUM, 3600).

% Delay between pages of the same feed
-define(DELAY_NEXT_PAGE, 10).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Periodic poll of due Twitter subscriptions
poll(Context) ->
    try
        Subscriptions = m_twitter:due(calendar:universal_time(), Context),
        Status = lists:foldl(
            fun
                (Sub, ok) ->
                    case poll_feed(Sub, Context) of
                        {ok, Result} ->
                            ImportCount = import_result(Sub, Result, Context),
                            set_due(Sub, Result, ImportCount, Context),
                            maybe_poll_next(Sub, Result, Context),
                            lager:debug("Twitter: imported ~p tweets", [ ImportCount ]),
                            ok;
                        ok ->
                            ok;
                        {error, _} = Error ->
                            lager:error("Twitter poller error for ~p: ~p", [ Sub, Error ]),
                            Error;
                        {delay, _} = D ->
                            D
                    end;
                (_Sub, Other) ->
                    Other
            end,
            ok,
            Subscriptions),
        determine_next_delay(Status, Context)
    catch
        Type:E ->
            Trace = erlang:get_stacktrace(),
            lager:error("Twitter poller error: ~p:~p at ~p", [ Type, E, Trace ]),
            {delay, ?DELAY_EXCEPTION}
    end.

%% @doc Poll the next page of a feed
-spec poll_next( integer(), {string, list()}, z:context() ) -> ok | {delay, integer()}.
poll_next(SubId, Next, Context) ->
    case m_twitter:get(SubId, Context) of
        {ok, Sub} ->
            try
                case poll_feed_next(Sub, Next, Context) of
                    {ok, Result} ->
                        ImportCount = import_result(Sub, Result, Context),
                        m_twitter:set_import_count(SubId, ImportCount, Context),
                        maybe_poll_next(Sub, Result, Context),
                        ok;
                    ok ->
                        ok;
                    {error, _} ->
                        ok;
                    {delay, _} = D ->
                        D
                end
            catch
                Type:E ->
                    Trace = erlang:get_stacktrace(),
                    lager:error("Twitter (next) poller error: ~p:~p at ~p", [ Type, E, Trace ]),
                    {delay, ?DELAY_EXCEPTION}
            end;
        {error, not_found} ->
            ok
    end.


%% @doc Import all feed entries and set new due for the subscription
import_result(Sub,  #{ tweets := Tweets }, Context) ->
    AuthorId = proplists:get_value(user_id, Sub),
    ContextUser = case AuthorId of
        undefined -> z_acl:sudo(Context);
        UserId -> z_acl:logon(UserId, Context)
    end,
    lists:foldl(
        fun(Tweet, Acc) ->
            case twitter_import_tweet:import_tweet(Tweet, AuthorId, ContextUser) of
                {ok, _} ->
                    Acc + 1;
                {error, _} ->
                    Acc
            end
        end,
        0,
        Tweets).

%% @doc Set the next due for the feed, backoff if no tweets fetched
set_due(Sub, #{ max_id := MaxId}, ImportCount, Context) ->
    {id, SubId} = proplists:lookup(id, Sub),
    Now = z_datetime:timestamp(),
    Delay = case MaxId of
        undefined ->
            % Check when the last import was
            {last_import, LastImport} = proplists:lookup(last_import, Sub),
            Last = z_datetime:datetime_to_timestamp(LastImport),
            backoff(Now - Last);
        _MaxId ->
            ?DELAY_ACTIVE
    end,
    Due = z_datetime:timestamp_to_datetime(Now + Delay),
    m_twitter:set_due(SubId, Due, MaxId, ImportCount, Context).

%% @doc Fetch the next page of the poll, this will be scheduled as separate task
%%      so that the current task is not taking too long.
maybe_poll_next(_Sub, #{ next := undefined }, _Context) ->
    ok;
maybe_poll_next(_Sub, #{ next := {_API, undefined} }, _Context) ->
    ok;
maybe_poll_next(Sub, #{ next := {_API, _Args} = Next }, Context) ->
    {id, SubId} = proplists:lookup(id, Sub),
    Args = [
        SubId,
        Next
    ],
    z_pivot_rsc:insert_task_after(?DELAY_NEXT_PAGE, ?MODULE, poll_next, undefined, Args, Context).



backoff(N) when N > 24*3600 ->     % After a day, once per 12 hours
    12 * 3600;
backoff(N) when N > 7200 ->        % After two hours, once per hour
    60 * 60;
backoff(N) when N > 900 ->         % After 15 minutes once per 15 minutes
    15 * 60;
backoff(N) when N > 300 ->         % After 5 minutes once per 5 minutes
    5 * 60;
backoff(N) when N > 60 ->          % After 1 minute once per minute
    1 * 60;
backoff(_N) ->                     % First minute every 10 seconds
    ?DELAY_ACTIVE.


poll_feed(Sub, Context) ->
    {id, SubId} = proplists:lookup(id, Sub),
    case is_allowed_insert_tweets(Sub, Context) of
        true ->
            {key, FeedKey} = proplists:lookup(key, Sub),
            {last_id, LastId} = proplists:lookup(last_id, Sub),
            case twitter_feed:poll(FeedKey, LastId, Context) of
                {ok, Result} ->
                    {ok, Result};
                {error, unauthorized} ->
                    % TODO: check code returned if tokens are invalid vs access to user denied
                    m_twitter:disable(SubId, <<"noauth">>, Context),
                    ok;
                {error, not_found} ->
                    m_twitter:disable(SubId, <<"not_found">>, Context),
                    ok;
                {error, rate_limit} ->
                    m_twitter:disable(SubId, <<"not_found">>, Context),
                    {delay, ?DELAY_RATE_LIMIT};
                {error, connection_limit} ->
                    {delay, ?DELAY_RATE_LIMIT};
                {error, {code, HttpCode}} when HttpCode >= 500, HttpCode =< 600 ->
                    {delay, ?DELAY_5XX_ERROR};
                {error, _} = Error ->
                    Error
            end;
        false ->
            lager:info("twitter_poller: disable subscription because not allowd to insert tweets for ~p", [ Sub ]),
            m_twitter:disable(SubId, <<"acl">>, Context),
            {error, eacces}
    end.

poll_feed_next(Sub, Next, Context) ->
    {id, SubId} = proplists:lookup(id, Sub),
    case twitter_feed:poll_next(Next, Context) of
        {ok, Result} ->
            {ok, Result};
        {error, unauthorized} ->
            % TODO: check code returned if tokens are invalid vs access to user denied
            m_twitter:disable(SubId, <<"noauth">>, Context),
            ok;
        {error, not_found} ->
            m_twitter:disable(SubId, <<"not_found">>, Context),
            ok;
        {error, rate_limit} ->
            m_twitter:disable(SubId, <<"not_found">>, Context),
            {delay, ?DELAY_RATE_LIMIT};
        {error, connection_limit} ->
            {delay, ?DELAY_RATE_LIMIT};
        {error, {code, HttpCode}} when HttpCode >= 500, HttpCode =< 600 ->
            {delay, ?DELAY_5XX_ERROR};
        {error, _} = Error ->
            Error
    end.


is_allowed_insert_tweets(Sub, Context) ->
    case proplists:lookup(user_id, Sub) of
        {user_id, undefined} ->
            true;
        {user_id, UserId} ->
            z_acl:is_allowed(insert, tweet, z_acl:logon(UserId, Context))
    end.


%% @doc Fetch the next delay from the subscriptions table
determine_next_delay(ok, Context) ->
    case m_twitter:next_due(Context) of
        undefined ->
            {delay, ?DELAY_MAXIMUM};
        DT ->
            Secs = z_datetime:datetime_to_timestamp(DT),
            Now = z_datetime:timestamp(),
            {delay, max( min(?DELAY_MINIMUM, Secs - Now), ?DELAY_MAXIMUM )}
    end;
determine_next_delay({delay, Secs}, _Context) ->
    {delay, max( Secs, ?DELAY_MAXIMUM )}.
