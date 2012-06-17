%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Return the JSON for the country overview.

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

-module(service_geomap_countries).
-author("Marc Worrell <marc@worrell.nl>").

-svc_title("JSON for the country map, colours and country id per country.").
-svc_needauth(false).

-export([process_get/2]).

-export([
    get_data/1,
    get_country_coords/1
]).

-define(COUNT, 100).

-include_lib("zotonic.hrl").

process_get(_ReqData, Context) ->
    {binary_json, get_data(Context)}.

get_data(Context) ->
    Coords = get_country_coords(Context),
    Ids = get_countries(Context),
    Data = lists:foldl(fun(Id, Acc) ->
                            case z_acl:rsc_visible(Id, Context) of
                                true ->
                                    [{m_rsc:p(Id, address_country, Context), {
                                            z_trans:lookup_fallback(m_rsc:p(Id, title, Context), Context),
                                            m_rsc:p(Id, map_color, Context), 
                                            m_rsc:p(Id, map_value, Context)
                                        }
                                    } | Acc];
                                false ->
                                    Acc
                            end
                        end,
                        [],
                        Ids),
    set_values(Coords, Data).


get_countries(Context) ->
    case m_category:name_to_id(country, Context) of
        {error, _} -> 
            [];
        {ok, _} ->
            #search_result{result=R} = z_search:search({latest, [{cat, country}]}, {1,1000}, Context),
            R
    end.

get_country_coords(Context) ->
    {ok, #module_index{} = M} = z_module_indexer:find(lib, "data/internet_users_2005_choropleth_lowres.json", Context),
    {ok, Data} = file:read_file(z_convert:to_list(M#module_index.filepath)),
    mochijson:binary_decode(Data).


set_values({struct, [{<<"type">>,<<"FeatureCollection">>}, {<<"features">>, Cs}]}, Data) ->
    {struct, [
        {<<"type">>,<<"FeatureCollection">>}, 
        {<<"features">>, 
            lists:foldl(fun(C, Acc) -> 
                            {ok, C1} = set_value(C, Data),
                            [C1|Acc]
                        end,
                        [],
                        Cs)
    }]}.

set_value({struct, Fs}, Data) ->
    {struct, Properties} = proplists:get_value(<<"properties">>, Fs),
    Name = proplists:get_value(<<"name">>, Properties),
    case l10n_country2iso:country2iso(Name) of
        undefined ->
            P1 = {<<"properties">>,
               {struct,[{<<"name">>,Name},
                        {<<"value">>,<<"">>},
                        {<<"colour">>,<<"#ccc">>}
                ]}
            };
        Iso ->
            case proplists:get_value(Iso, Data) of
                {Title, Colour, Value} ->
                    P1 = {<<"properties">>,
                       {struct,[{<<"name">>,Title},
                                {<<"value">>,value(Value)},
                                {<<"colour">>, color(Colour)}
                        ]}
                    };
                undefined ->
                    P1 = {<<"properties">>,
                       {struct,[{<<"name">>,l10n_iso2country:iso2country(Iso)},
                                {<<"value">>,<<"">>},
                                {<<"colour">>,<<"#ccc">>}
                        ]}
                    }
            end
    end,
    {ok, {struct, [ P1 | proplists:delete(<<"properties">>, Fs) ]}}.


    color(undefined) -> <<"#ccc">>;
    color(<<>>) -> <<"#ccc">>;
    color(C) -> C.
    
    value(undefined) -> <<"">>;
    value(V) -> V.

