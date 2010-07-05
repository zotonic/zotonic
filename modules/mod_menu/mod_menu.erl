%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-12
%% @doc Menu module.  Supports menus in Zotonic. Adds admin interface to define the menu.

%% Copyright 2009 Marc Worrell
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

-module(mod_menu).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Menus").
-mod_description("Menus in Zotonic, adds amdin interface to define the menu.").

%% interface functions
-export([
    observe_menu_get_rsc_ids/2
]).

observe_menu_get_rsc_ids(menu_get_rsc_ids, Context) ->
    Menu = scomp_menu_menu:get_menu(Context),
    {ok, menu_ids(Menu, [])}.
    
    menu_ids([], Acc) ->
        Acc;
    menu_ids([{Id,SubMenu}|T], Acc) ->
        Acc1 = menu_ids(SubMenu, [Id|Acc]),
        menu_ids(T, Acc1);
    menu_ids([H|T], Acc) ->
        menu_ids(T, [H|Acc]).

