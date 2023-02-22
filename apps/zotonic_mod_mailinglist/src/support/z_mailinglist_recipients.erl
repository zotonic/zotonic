%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020-2023 Marc Worrell <marc@worrell.nl>
%% @doc Fetch all recipients for a mailinglist.

%% Copyright 2020-2023 Marc Worrell
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

-module(z_mailinglist_recipients).

-export([
    recipient_key_encode/2,
    recipient_key_decode/2,

    count_recipients/2,
    list_recipients/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(MAX_ROWS, 100000).
-define(RECIPIENT_SECRET_LENGTH, 32).

%% @doc Encode a recipient to a key used for the manage subscriptions page.
-spec recipient_key_encode(Recipient, Context) -> {ok, Key} when
    Recipient :: Email | RscId,
    RscId :: m_rsc:resource_id(),
    Email :: binary(),
    Context :: z:context(),
    Key :: binary().
recipient_key_encode(Recipient, Context) ->
    Term = {r0, Recipient, z_datetime:timestamp()},
    {ok, termit:encode_base64(Term, recipient_secret(Context))}.

%% @doc Decode a recipient to a key used for the manage subscriptions page.
-spec recipient_key_decode(Key, Context) -> {ok, Recipient} | {error, Reason} when
    Key :: binary(),
    Context :: z:context(),
    Recipient :: Email | RscId,
    RscId :: m_rsc:resource_id(),
    Email :: binary(),
    Reason :: expired | term().
recipient_key_decode(Key, Context) ->
    case termit:decode_base64(Key, recipient_secret(Context)) of
        {ok, {r0, Recipient, Timestamp}} when is_integer(Timestamp) ->
            % TODO: check timestamp
            {ok, Recipient};
        {ok, Decoded} ->
            ?LOG_ERROR(#{
                in => mod_mailinglist,
                text => <<"Mailinglist recipient key with unknown decoded value">>,
                decoded => Decoded,
                result => error,
                reason => payload
            }),
            {error, payload};
        {error, Reason} ->
            ?LOG_ERROR(#{
                in => mod_mailinglist,
                text => <<"Mailinglist recipient key could not be decoded">>,
                result => error,
                reason => Reason,
                key => Key
            }),
            {error, Reason}
    end.

-spec recipient_secret( z:context() ) -> binary().
recipient_secret(Context) ->
    case m_config:get_value(mod_mailinglist, recipient_secret, Context) of
        <<>> -> generate_recipient_secret(Context);
        undefined -> generate_recipient_secret(Context);
        Secret -> Secret
    end.

-spec generate_recipient_secret( z:context() ) -> binary().
generate_recipient_secret(Context) ->
    Secret = z_ids:id(?RECIPIENT_SECRET_LENGTH),
    m_config:set_value(mod_mailinglist, recipient_secret, Secret, Context),
    Secret.


-spec count_recipients( m_rsc:resource(), z:context() ) -> map().
count_recipients(ListId, Context) ->
    Count = m_mailinglist:count_recipients(ListId, Context),
    SubIds = m_edge:subjects(ListId, subscriberof, Context),
    QueryIds = case m_rsc:p(ListId, 'query', Context) of
        undefined -> [];
        <<>> -> [];
        _ ->
            Q = #{
                <<"query_id">> => ListId
            },
            #search_result{ result = Result } = z_search:search(<<"query">>, Q, 1, ?MAX_ROWS, Context),
            Result
    end,
    #{
        total => Count + length(SubIds) + length(QueryIds),
        recipients => Count,
        subscriberof => length(SubIds),
        query_text => length(QueryIds)
    }.

%% @doc Fetch all (enabled) recipients of a mailinglist.
-spec list_recipients( m_rsc:resource(), z:context() ) -> map().
list_recipients(List, Context) ->
    ListId = m_rsc:rid(List, Context),
    {ok, Recipients} = m_mailinglist:list_recipients(ListId, Context),
    Rs = lists:foldl(
        fun
            (#{ <<"is_enabled">> := true, <<"email">> := Email } = R, Acc) ->
                Acc#{ Email => R };
            (_, Acc) ->
                Acc
        end,
        #{},
        Recipients),
    SubIds = m_edge:subjects(ListId, subscriberof, Context),
    QueryIds = case m_rsc:p(ListId, 'query', Context) of
        undefined -> [];
        <<>> -> [];
        _ ->
            Q = #{
                <<"query_id">> => ListId
            },
            #search_result{ result = Result } = z_search:search(<<"query">>, Q, 1, ?MAX_ROWS, Context),
            Result
    end,
    AllIds = lists:usort(SubIds ++ QueryIds),
    lists:foldl(
        fun(Id, Acc) ->
            case z_acl:rsc_visible(Id, Context)
                andalso m_rsc:p(Id, is_published_date, Context)
                andalso not z_convert:to_bool( m_rsc:p_no_acl(Id, <<"is_mailing_opt_out">>, Context) )
            of
                true ->
                    Email = m_rsc:p_no_acl(Id, <<"email_raw">>, Context),
                    Acc#{
                        Email => Id
                    };
                false ->
                    Acc
            end
        end,
        Rs,
        AllIds).

