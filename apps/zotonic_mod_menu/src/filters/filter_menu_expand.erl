%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Expand a menu by adding all 'haspart' edges to the menu lists.

%% Copyright 2021 Marc Worrell
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

-module(filter_menu_expand).

-export([
    menu_expand/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

menu_expand([], _Contex) ->
    [];
menu_expand([ #rsc_tree{} | _ ] = Menu, Context) ->
    expand(Menu, Context);
menu_expand(MenuId, Context) ->
    case m_rsc:rid(MenuId, Context) of
        undefined ->
            [];
        RscId ->
            z_depcache:memo(
                fun() -> menu_expand_1(RscId, Context) end,
                {menu_expand, RscId},
                ?DAY,
                [
                    RscId,
                    {predicate, m_rsc:rid(haspart, Context)}
                ],
                Context)
    end.

menu_expand_1(RscId, Context) ->
    Menu = case m_rsc:p(RscId, <<"menu">>, Context) of
        [ #rsc_tree{} | _ ] = M -> M;
        _ -> []
    end,
    TopIds = [ Id || #rsc_tree{ id = Id } <- Menu ],
    M1 = lists:filtermap(
        fun(Id) ->
            case lists:member(Id, TopIds) of
                false -> {true, #rsc_tree{ id = Id, tree = [] }};
                true -> false
            end
        end,
        m_edge:objects(RscId, haspart, Context)),
    expand(Menu ++ M1, Context).

expand(Menu, Context) when is_list(Menu) ->
    lists:map(
        fun(M) -> expand(M, Context) end,
        Menu);
expand(#rsc_tree{ id = RscId, tree = Tree } = T, Context) ->
    TopIds = [ Id || #rsc_tree{ id = Id } <- Tree ],
    M1 = lists:filtermap(
        fun(Id) ->
            case lists:member(Id, TopIds) of
                false -> {true, #rsc_tree{ id = Id, tree = [] }};
                true -> false
            end
        end,
        m_edge:objects(RscId, haspart, Context)),
    M2 = expand(M1, Context),
    T#rsc_tree{ tree = M2 }.
