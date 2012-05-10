%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse
%% @doc Get a "trail" of menu parents

%% Copyright 2010 Marc Worrell
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

-module(filter_menu_trail).
-export([menu_trail/2, menu_trail/3, test/0]).

menu_trail(undefined, _Context) ->
    undefined;
menu_trail(Id, Context) ->
    menu_trail(Id, main_menu, Context).

menu_trail(_Id, undefined, _Context) ->
    undefined;
menu_trail(_Id, [], _Context) ->
    undefined;
menu_trail(_Id, <<>>, _Context) ->
    undefined;
menu_trail(undefined, _Menu, _Context) ->
    undefined;
menu_trail(Id, [{MenuId,Sub}|_] = Menu, Context) when is_integer(MenuId), is_list(Sub) ->
    trail(m_rsc:rid(Id, Context), mod_menu:remove_invisible(Menu, Context), Context);
menu_trail(Id, MenuId, Context) ->
    Menu = mod_menu:get_menu(MenuId, Context),
    trail(m_rsc:rid(Id, Context), mod_menu:remove_invisible(Menu, Context), Context).


trail(_Id, [], _Context) ->
    []; %% not found
trail(Id, [{Id, _}|_], _Context) ->
    [Id]; %% found
trail(Id, [{MenuMaybeSymbolic, Children}|Rest], Context) ->
    MenuId = m_rsc:rid(MenuMaybeSymbolic, Context),
    case MenuId of
        Id ->
            [Id];
        _ ->
            case trail(Id, Children, Context) of
                [] ->
                    %% not found
                    trail(Id, Rest, Context);
                Path ->
                    %% Found
                    [MenuId|Path]
            end
    end;
trail(Id, [MenuId|Rest], Context) when is_integer(MenuId)->
    %% legacy menu notation
    trail(Id, [{MenuId, []}|Rest], Context).




test() ->
    [1] = trail(1, [{1, []}], x),
    [] = trail(1, [{33, []}], x),
    [33,66] = trail(66, [{4, []}, {33, [{3, []}, {66, []}]}], x).
