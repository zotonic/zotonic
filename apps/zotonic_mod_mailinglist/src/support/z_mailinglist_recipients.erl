%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell <marc@worrell.nl>
%% @doc Fetch all recipients for a mailinglist.

%% Copyright 2020 Marc Worrell
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
    count_recipients/2,
    list_recipients/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(MAX_ROWS, 100000).

-spec count_recipients( m_rsc:resource(), z:context() ) -> map().
count_recipients(ListId, Context) ->
    Count = m_mailinglist:count_recipients(ListId, Context),
    SubIds = m_edge:subjects(ListId, subscriberof, Context),
    QueryIds = case m_rsc:p(ListId, 'query', Context) of
        undefined -> [];
        <<>> -> [];
        _ ->
            Q = [
                {query_id, ListId}
            ],
            #search_result{ result = Result } = z_search:search({'query', Q}, ?MAX_ROWS, Context),
            Result
    end,
    #{
        total => Count + length(SubIds) + length(QueryIds),
        recipients => Count,
        subscriberof => length(SubIds),
        query_text => length(QueryIds)
    }.

%% @doc Fetch all (enabled) recipients of a mailinglist.
-spec list_recipients( m_rsc:resource(), z:context() ) -> {ok, map()}.
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
            Q = [
                {query_id, ListId}
            ],
            #search_result{ result = Result } = z_search:search({'query', Q}, ?MAX_ROWS, Context),
            Result
    end,
    AllIds = lists:usort(SubIds ++ QueryIds),
    lists:foldl(
        fun(Id, Acc) ->
            case z_acl:rsc_visible(Id, Context)
                andalso m_rsc:p(Id, is_published_date, Context)
            of
                true ->
                    Email = m_rsc:p_no_acl(Id, email_raw, Context),
                    Acc#{
                        Email => Id
                    };
                false ->
                    Acc
            end
        end,
        Rs,
        AllIds).

