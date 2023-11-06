%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009-2023 Arjan Scherpenisse
%% @doc Notifications for 'query' resources when items are added to them.
%% @end

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

-module(search_query_notify).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

%% interface functions
-export([
         init/1,
         watches_update/3,
         watches_remove/3,
         check_rsc/3,
         send_notifications/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").


%% Get all 'query' types and add them to the watches.
init(Context) ->
    case m_category:get_by_name('query', Context) of
        undefined ->
            [];
        _ ->
            Ids = z_search:query_([{cat, 'query'}], Context),
            lists:foldr(fun (Id, Acc) -> watches_update(Id, Acc, Context) end, [], Ids)
    end.


watches_update(Id, Watches, Context) ->
    case m_rsc:p(Id, 'query', Context) of
        undefined ->
            proplists:delete(Id, Watches);
        Q ->
            case z_convert:to_bool(m_rsc:p_no_acl(Id, is_query_live, Context)) of
                true ->
                    try
                        Query = z_search_props:from_text(z_html:unescape(Q)),
                        [{Id, Query} | proplists:delete(Id, Watches)]
                    catch
                        throw:{error,{unknown_query_term, _Term}} ->
                            proplists:delete(Id, Watches)
                    end;
                false ->
                    proplists:delete(Id, Watches)
            end
    end.

watches_remove(Id, Watches, _Context) ->
    proplists:delete(Id, Watches).

%% @doc Check whether the given resource matches to the queries. Returns list of matching query resource ids.
check_rsc(Id, Watches, Context) ->
    IsA = m_rsc:p_no_acl(Id, is_a, Context),
    Cats = [
        #{
            <<"term">> => <<"cat">>,
            <<"value">> => z_convert:to_binary(A)
        } || A <- IsA ],
    CatsEx = [
        #{
            <<"term">> => <<"cat_exclude">>,
            <<"value">> => z_convert:to_binary(A)
        } || A <- IsA ],
    %% Pre-filter the list of queries according to category check
    W = lists:filter(
        fun({_Id, Query}) ->
            cat_matches(Cats, Query) andalso not(cat_matches(CatsEx, Query))
        end, Watches),
    %% Filter the list by executing the query
    W2 = lists:filter(
        fun({_Id, Query}) ->
            execute_query_check(Id, Query, Context)
        end, W),
    [QueryId || {QueryId, _} <- W2].


cat_matches([], _Props) -> false;
cat_matches([Cat|Rest], #{ <<"q">> := QueryTerms } = Query) ->
    case lists:member(Cat, QueryTerms) of
        true -> true;
        false -> cat_matches(Rest, Query)
    end.

send_notifications(_Id, [], _Context) ->
    ok;
send_notifications(Id, [QueryId|Rest], Context) ->
    z_notifier:notify(#rsc_query_item{query_id=QueryId, match_id=Id}, Context),
    send_notifications(Id, Rest, Context).


%% @doc Check the given resource ID against the props of the query. Returns true if matches.
execute_query_check(CheckId, #{ <<"q">> := Terms } = Query, Context) ->
    Query1 = Query#{
        <<"q">> => [
            #{
                <<"term">> => <<"id">>,
                <<"value">> => CheckId
            }
            | Terms
        ]
    },
    case z_search:query_(Query1, Context) of
        [CheckId] -> true;
        [] -> false
    end.

