%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell
%% @doc Find the hierarchies of a page. Returns the known paths to a page.
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

-define(MAX_HASPART, 4).

%% @doc Find a list of all paths to this resource. Checks the main menu path
%% and the haspart subject edges.
-spec find(Id, Context) -> {ok, BreadcrumbList} when
    Id :: m_rsc:resource_id(),
    Context :: z:context(),
    BreadcrumbList :: [ Breadcrumb ],
    Breadcrumb :: [ m_rsc:resource_id() ].
find(Id, Context) ->
    case m_rsc:p_no_acl(Id, <<"page_path">>, Context) of
        <<"/">> ->
            % The home page has a single trail with itself.
            {ok, [[Id]]};
        _ ->
            {MTrails, PTrails} = trails(Id, [], [], Context),
            PTrails1 = lists:filter(
                fun
                    ([MId]) when MId =:= Id -> false;
                    (PTrail) -> not lists:member(PTrail, MTrails)
                end,
                PTrails),
            {ok, MTrails ++ lists:sublist(PTrails1, ?MAX_HASPART)}
    end.

%% @doc Collect the menu trails of an id. Preference to trails that are part of a menu.
%% The 'haspart' edges are followed till a cycle or the top of the collection tree has
%% been found.
trails(Id, Trail, PartOfAcc, Context) ->
    case lists:member(Id, Trail) of
        true ->
            {[], PartOfAcc};
        false ->
            trails_1(Id, Trail, PartOfAcc, Context)
    end.

trails_1(Id, Trail, PartOfAcc, Context) ->
    MenuPartOf = m_edge:subjects(Id, hasmenupart, Context),
    MenuTrails = lists:foldl(
        fun(MId, MAcc) ->
            Ts = menu_trail(Id, MId, Trail, Context),
            Ts ++ MAcc
        end,
        [],
        MenuPartOf),
    IdTrail = [ Id | Trail ],
    {MenuTrails1, PartofTrails1} = case visible(m_edge:subjects(Id, haspart, Context), Context) of
        [] when MenuTrails =:= [] ->
            % Do not list path to collection if the collection is part of a menu.
            case is_noindex(Id, Context) of
                true -> {MenuTrails, PartOfAcc};
                false -> {MenuTrails, [ IdTrail | PartOfAcc ]}
            end;
        [] ->
            {MenuTrails, PartOfAcc};
        PartOf ->
            lists:foldl(
                fun(PId, {MTs, PTs} = Acc) ->
                    case is_noindex(PId, Context) of
                        true ->
                            Acc;
                        false ->
                            {MTs1, PTs1} = trails(PId, IdTrail, PTs, Context),
                            {MTs1 ++ MTs, PTs1}
                    end
                end,
                {MenuTrails, PartOfAcc},
                PartOf)
    end,
    {MenuTrails1, PartofTrails1}.

menu_trail(Id, MId, Trail, Context) ->
    case visible(filter_menu_trail:menu_trail(Id, MId, Context), Context) of
        [] ->
            [];
        MenuTrail ->
            case m_rsc:p_no_acl(MId, name, Context) of
                <<"main_menu">> ->
                    % Main menu, used by the home page.
                    case homepage(Context) of
                        undefined -> [ MenuTrail ++ Trail ];
                        HomeId -> [ [ HomeId | MenuTrail ] ++ Trail ]
                    end;
                _ ->
                    % Find pages using this menu, fall back to the menu itself.
                    MenuOf = case visible(m_edge:subjects(Id, hasmenu, Context), Context) of
                        [] -> [ Id ];
                        HMs -> HMs
                    end,
                    lists:filtermap(
                        fun(MenuOfId) ->
                            case is_noindex(MenuOfId, Context) of
                                true -> false;
                                false -> {true, [ MenuOfId | MenuTrail ] ++ Trail}
                            end
                        end,
                        MenuOf)
            end
    end.

homepage(Context) ->
    case m_rsc:rid(page_home, Context) of
        undefined -> m_rsc:rid(home, Context);
        Id -> Id
    end.

visible(undefined, _Context) ->
    [];
visible(Ids, Context) ->
    lists:filter(fun(Id) -> z_acl:rsc_visible(Id, Context) end, Ids).

is_noindex(Id, Context) ->
    z_convert:to_bool(m_rsc:p_no_acl(Id, seo_noindex, Context)).

