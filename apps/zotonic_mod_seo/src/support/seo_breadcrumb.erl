%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell
%% @doc Find the hierarchies of a page. Returns the known paths of a page.
%% @end

%% Copyright 2023 Marc Worrell
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

-module(seo_breadcrumb).

-export([
    find/2
]).

-define(MAX_HASPART, 3).

%% @doc Find a list of all paths to this resource. Checks the main menu path
%% and the haspart subject edges.
-spec find(Id, Context) -> {ok, BreadcrumbList} when
    Id :: m_rsc:resource_id(),
    Context :: z:context(),
    BreadcrumbList :: [ Breadcrumb ],
    Breadcrumb :: [ m_rsc:resource_id() ].
find(Id, Context) ->
    MenuTrail = menu_trail(Id, Context),
    IsPartOf = visible(m_edge:subjects(Id, haspart, Context), Context),
    IsPartOf1 = lists:sublist(IsPartOf, ?MAX_HASPART),
    CollectionTrails = collection_trail(IsPartOf1, [Id], [], Context),
    CollectionTrails1 = lists:filter(
        fun
            ([_]) -> false;
            ([_|_]) -> true
        end,
        CollectionTrails),
    CollectionTrails2 = lists:sublist(CollectionTrails1, ?MAX_HASPART),
    case MenuTrail of
        [] ->
            {ok, CollectionTrails2};
        _ ->
            {ok, [ MenuTrail | CollectionTrails2 ]}
    end.

menu_trail(Id, Context) ->
    case visible(filter_menu_trail:menu_trail(Id, Context), Context) of
        [] -> [];
        Trail -> [ page_home | Trail ]
    end.

collection_trail([], Trail, Acc, _Context) ->
    [Trail|Acc];
collection_trail(Ids, Trail, Acc, Context) ->
    lists:foldl(
        fun(Id, TAcc) ->
            case lists:member(Id, Trail) of
                true ->
                    % Drop cyclic paths
                    TAcc;
                false ->
                    IsPartOf = visible(m_edge:subjects(Id, haspart, Context), Context),
                    IsPartOf1 = lists:sublist(IsPartOf, ?MAX_HASPART),
                    collection_trail(IsPartOf1, [Id|Trail], TAcc, Context)
            end
        end,
        Acc,
        Ids).

visible(undefined, _Context) ->
    [];
visible(Ids, Context) ->
    lists:filter(fun(Id) -> z_acl:rsc_visible(Id, Context) end, Ids).

