%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009-2010 Arjan Scherpenisse
%% Date: 2009-04-12
%% @doc Notifications for 'query' resources when items are added to them.

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

-include_lib("zotonic.hrl").


%% Get all 'query' types and add them to the watches.
init(Context) ->
    case m_category:get_by_name('query', Context) of
        undefined -> [];
        _ ->
            Ids = z_search:query_([{cat, 'query'}], Context),
            lists:foldr(fun (Id, Acc) -> watches_update(Id, Acc, Context) end, [], Ids)
    end.


watches_update(Id, Watches, Context) ->
    case m_rsc:p(Id, 'query', Context) of
        undefined ->
            proplists:delete(Id, Watches);
        Q ->
            case z_convert:to_bool(m_rsc:p(Id, is_query_live, Context)) of
                true ->
                    Props = search_query:parse_query_text(Q),
                    [{Id, Props} | proplists:delete(Id, Watches)];
                false ->
                    proplists:delete(Id, Watches)
            end
    end.

watches_remove(Id, Watches, _Context) ->
    proplists:delete(Id, Watches).

%% @doc Check whether the given resource matches to the queries. Returns list of matching query resource ids.
%% @spec check_rsc(Id, Watches, Context) -> list()
check_rsc(Id, Watches, Context) ->
    IsA = m_rsc:p(Id, is_a, Context),
    Cats   = [ {cat, atom_to_list(A)} || {A,_} <- IsA ],
    CatsEx = [ {cat_exclude, atom_to_list(A)} || {A,_} <- IsA ],
    %% Pre-filter the list of queries according to category check
    W = lists:filter(fun({_, Props}) -> cat_matches(Cats, Props) andalso not(cat_matches(CatsEx, Props)) end, Watches),
    %% Filter the list by executing the query
    W2 = lists:filter(fun({_, QueryProps}) -> execute_query_check(Id, QueryProps, Context) end, W),
    [QueryId || {QueryId, _} <- W2].


cat_matches([], _Props) -> false;
cat_matches([Cat|Rest], Props) ->
    case lists:member(Cat, Props) of
        true -> true;
        false -> cat_matches(Rest, Props)
    end.

send_notifications(_Id, [], _Context) ->
    ok;
send_notifications(Id, [QueryId|Rest], Context) ->
    z_notifier:notify(#rsc_query_item{query_id=QueryId, match_id=Id}, Context),
    send_notifications(Id, Rest, Context).


%% @doc Check the given resource ID against the props of the query. Returns true if matches.
execute_query_check(CheckId, QueryProps, Context) ->
    Query = [{rsc_id, CheckId} | QueryProps],
    case z_search:query_(Query, Context) of
        [CheckId] -> true;
        [] -> false
    end.

