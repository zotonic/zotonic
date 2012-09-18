%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Return a list of locations for the search or id given.

%% Copyright 2012 Marc Worrell
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

-module(service_geomap_locations).
-author("Marc Worrell <marc@worrell.nl>").

-svc_title("List locations of a collection, search_query or search parameters.").
-svc_needauth(false).

-export([process_get/2]).

-define(COUNT, 100).

-include_lib("zotonic.hrl").

process_get(_ReqData, Context) ->
    case get_ids(Context) of
        {error, _, _} = E -> E;
        {ok, Ids} ->
            Ids1 = lists:filter(fun(Id) -> z_acl:rsc_visible(Id, Context) end, Ids),
            Locations = add_locations(Ids1, [], Context),
            {array, Locations}
    end.

add_locations([], Acc, _Context) ->
    lists:reverse(Acc);
add_locations([Id|Ids], Acc, Context) ->
    Lat = m_rsc:p(Id, computed_location_lat, Context),
    Lng = m_rsc:p(Id, computed_location_lng, Context),
    IsManualLoc = not z_utils:is_empty(m_rsc:p(Id, location_lat, Context))
                andalso not z_utils:is_empty(m_rsc:p(Id, location_lng, Context)),
    case {Lat,Lng} of
        {undefined, _} -> add_locations(Ids, Acc, Context);
        {_, undefined} -> add_locations(Ids, Acc, Context);
        _ -> 
            Loc = {struct, [
                {id, z_convert:to_binary(Id)},
                {location_lat, Lat},
                {location_lng, Lng},
                {is_manual, IsManualLoc},
                {category, m_rsc:p(m_rsc:p(Id, category_id, Context), name, Context)},
                {title, ?__(m_rsc:p(Id, title, Context), Context)},
                {page_url, iolist_to_binary(m_rsc:p(Id, page_url, Context))},
                {address_country, m_l10n:country_name(m_rsc:p(Id, address_country, Context), Context)},
                {address_city, m_rsc:p(Id, address_city, Context)},
                {address_state, m_rsc:p(Id, address_state, Context)},
                {address_street_1, m_rsc:p(Id, address_street_1, Context)},
                {address_street_2, m_rsc:p(Id, address_street_2, Context)},
                {address_postcode, m_rsc:p(Id, address_postcode, Context)}
            ]},
            add_locations(Ids, [Loc|Acc], Context)
    end.


get_ids(Context) ->
    QueryArgs = controller_api:get_q_all(Context),
    case QueryArgs of
        [{"id", RscId}] ->
            get_from_id(RscId, Context);
        Q1 ->
            get_from_qs(Q1, Context)
    end.

get_from_qs(Qs, Context) ->
    try
        Qs1 = proplists:delete("count", Qs),
        Count = z_convert:to_integer(proplists:get_value("count", Qs, ?COUNT)),
        Q = search_query:parse_request_args(Qs1),
        S = z_search:search({'query', Q}, {1,Count}, Context),
        {ok, S#search_result.result}
    catch
        _: {error, {unknown_query_term, E}} ->
            {error, unknown_arg, E};
        _: {error, {Message, E}} ->
            {error, Message, E};
        _: {case_clause, {error, {error, error, _, E, _}}} ->
            {error, syntax, binary_to_list(E)}
    end.


get_from_id(RscId, Context) ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, unknown_id, RscId};
        Id ->
            case m_rsc:is_a(Id, search_query, Context) of
                true ->
                    S = z_search:search({'query', [{query_id, Id}]}, Context),
                    {ok, S#search_result.result};
                false ->
                    case m_rsc:is_a(Id, collection, Context) of
                        true -> {ok, m_edge:objects(Id, haspart, Context)};
                        false -> {ok, [Id]}
                    end
            end
    end.
    

