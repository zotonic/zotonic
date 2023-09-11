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
    recipient_key_encode/3,
    recipient_key_decode/2,

    count_recipients/2,
    list_recipients/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(MAX_ROWS, 100000).
-define(RECIPIENT_SECRET_LENGTH, 32).

%% @doc Encode a recipient to a key used for the manage subscriptions page.
-spec recipient_key_encode(Recipient, ListId, Context) -> {ok, Key} when
    Recipient :: Email | RscId,
    RscId :: m_rsc:resource_id(),
    Email :: binary(),
    ListId :: m_rsc:resource_id() | undefined,
    Context :: z:context(),
    Key :: binary().
recipient_key_encode(Recipient, ListId, Context) ->
    Term = {r1, Recipient, ListId, z_datetime:timestamp()},
    {ok, termit:encode_base64(Term, recipient_secret(Context))}.

%% @doc Decode a recipient to a key used for the manage subscriptions page.
-spec recipient_key_decode(Key, Context) -> {ok, Decoded} | {error, Reason} when
    Key :: binary(),
    Context :: z:context(),
    Decoded :: #{
        recipient => Recipient,
        list_id => MailinglistId
    },
    Recipient :: RscId | Email,
    RscId :: m_rsc:resource_id(),
    Email :: binary(),
    MailinglistId :: m_rsc:resource_id() | undefined,
    Reason :: expired | term().
recipient_key_decode(Key, Context) ->
    case termit:decode_base64(Key, recipient_secret(Context)) of
        {ok, {r0, Recipient, Timestamp}} when is_integer(Timestamp) ->
            % TODO: check timestamp
            {ok, #{
                recipient => Recipient,
                list_id => undefined
            }};
        {ok, {r1, Recipient, ListId, Timestamp}} when is_integer(Timestamp) ->
            % TODO: check timestamp
            {ok, #{
                recipient => Recipient,
                list_id => ListId
            }};
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
    QueryTotal = case m_rsc:p(ListId, <<"query">>, Context) of
        undefined -> 0;
        <<>> -> 0;
        _ ->
            Q = #{
                <<"query_id">> => ListId
            },
            try
                #search_result{ total = QTotal } = z_search:search(<<"query">>, Q, 1, ?MAX_ROWS, Context),
                QTotal
            catch
                Error:Reason:Stack ->
                    ?LOG_ERROR(#{
                        in => zotonic_mod_mailinglist,
                        text => <<"Error performing query for mailinglist recipients">>,
                        rsc_id => ListId,
                        query => Q,
                        result => Error,
                        reason => Reason,
                        stack => Stack
                    }),
                    0
            end
    end,
    #{
        total => Count + length(SubIds) + QueryTotal,
        recipients => Count,
        subscriberof => length(SubIds),
        query_text => QueryTotal
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
    QueryIds = case m_rsc:p(ListId, <<"query">>, Context) of
        undefined -> [];
        <<>> -> [];
        _ ->
            Q = #{
                <<"query_id">> => ListId
            },
            try
                #search_result{ result = Result } = z_search:search(<<"query">>, Q, 1, ?MAX_ROWS, Context),
                Result
            catch
                Error:Reason:Stack ->
                    ?LOG_ERROR(#{
                        in => zotonic_mod_mailinglist,
                        text => <<"Error performing query for mailinglist recipients">>,
                        rsc_id => ListId,
                        query => Q,
                        result => Error,
                        reason => Reason,
                        stack => Stack
                    }),
                    []
            end
    end,
    AllIds = lists:usort(SubIds ++ QueryIds),
    lists:foldl(
        fun(Id, Acc) ->
            case z_acl:rsc_visible(Id, Context)
                andalso m_rsc:p(Id, <<"is_published_date">>, Context)
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

