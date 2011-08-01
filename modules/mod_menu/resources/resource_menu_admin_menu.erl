%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Callbacks for editing the menu.

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

-module(resource_menu_admin_menu).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    event/2,
    test/0
]).

-include("zotonic.hrl").

event({drop, {dragdrop, ["new", Id], _, _DragEltId}, {dragdrop, [MenuId, "first"], _, _DropEltId}}, Context) ->
    mod_menu:set_menu(MenuId, [{Id, []}], Context),
    Html = z_template:render("_admin_menu_menu_view.tpl", [{id, MenuId}], Context),
    z_render:update("menu-editor", Html, Context);

event({drop, {dragdrop, DragTag, _, _DragEltId}, {dragdrop, [MenuId, "before", TP], _, _DropEltId}}, Context) ->
    TargetPath = lists:reverse(TP),
    Menu  = mod_menu:get_menu(MenuId, Context),
    Menu2 = case DragTag of
                ["new", NewId] ->
                    insert_menuitem(Menu, {NewId, []}, TargetPath);
                Path ->
                    SourcePath = lists:reverse(Path),
                    MenuItem = get_menuitem(Menu, SourcePath),
                    Menu1 = delete_path(Menu, SourcePath),
                    TargetPath1 = path_after_delete(TargetPath, SourcePath),
                    insert_menuitem(Menu1, MenuItem, TargetPath1)
            end,
    mod_menu:set_menu(MenuId, Menu2, Context),
    Html = z_template:render("_admin_menu_menu_view.tpl", [{id, MenuId}], Context),
    z_render:update("menu-editor", Html, Context);

event({drop, {dragdrop, DragTag, _, _DragEltId}, {dragdrop, [MenuId, "on", TargetPath], _, _DropEltId}}, Context) ->
    event({drop, {dragdrop, DragTag, x, x}, {dragdrop, [MenuId, "before", [1|TargetPath]], x, x}}, Context);

event({postback, {delete, Props}, _TriggerId, _TargetId}, Context) ->
    MenuId = proplists:get_value(menu_id, Props),
    Path = lists:reverse(proplists:get_value(path, Props)),
    Menu = mod_menu:get_menu(MenuId, Context),
    Menu1 = delete_path(Menu, Path),
    mod_menu:set_menu(MenuId, Menu1, Context),
    Html = z_template:render("_admin_menu_menu_view.tpl", [{id, MenuId}], Context),
    z_render:update("menu-editor", Html, Context);

event(_Event, Context) ->
    Context.


delete_path(Menu, Path) ->
    delete_path(Menu, Path, []).
delete_path([_|Rest], [1], Acc) ->
    lists:reverse(Acc) ++ Rest;
delete_path([Item|Rest], [Idx], Acc) ->
    delete_path(Rest, [Idx-1], [Item|Acc]);
delete_path([{Id,SubMenu}|Rest], [1|Path], Acc) ->
    lists:reverse(Acc)
        ++ [{Id, delete_path(SubMenu, Path)} | Rest];
delete_path([Item|Menu], [Idx|Path], Acc) ->
    delete_path(Menu, [Idx-1|Path], [Item|Acc]).


get_menuitem([Item|_Menu], [1]) ->
    Item;
get_menuitem([_Item|Menu], [Idx]) ->
    get_menuitem(Menu, [Idx-1]);
get_menuitem([{_, SubMenu}|_Rest], [1|PathRest]) ->
    get_menuitem(SubMenu, PathRest);
get_menuitem([_|Rest], [Idx|PathRest]) ->
    get_menuitem(Rest, [Idx-1|PathRest]).


insert_menuitem(Menu, Item, DestPath) ->
    insert_menuitem(Menu, Item, DestPath, []).
insert_menuitem(Menu, Item, [1], Acc) ->
    lists:reverse(Acc) ++ [Item | Menu];
insert_menuitem([First|Rest], Item, [Idx], Acc) ->
    insert_menuitem(Rest, Item, [Idx-1], [First|Acc]);
insert_menuitem([{Id, SubMenu} | MenuRest], Item, [1|Path], Acc) ->
    lists:reverse(Acc) 
        ++ [{Id, insert_menuitem(SubMenu, Item, Path, [])} | MenuRest];
insert_menuitem([MenuItem|MenuRest], Item, [Idx|Path], Acc) ->
    insert_menuitem(MenuRest, Item, [Idx-1|Path], [MenuItem|Acc]).


path_after_delete(Path, DeletePath) ->
    path_after_delete(Path, DeletePath, []).
path_after_delete([], _, Acc) ->
    lists:reverse(Acc);
path_after_delete(Path, [], Acc) ->
    lists:reverse(Acc) ++ Path;
path_after_delete([X|Rest], [Y|Rest2], Acc) when X > Y ->
    path_after_delete(Rest, Rest2, [X-1|Acc]);
path_after_delete([X|Rest], [_|Rest2], Acc) ->
    path_after_delete(Rest, Rest2, [X|Acc]).


test() ->
    [foo] = insert_menuitem([], foo, [1]),
    [foo, bar] = insert_menuitem([bar], foo, [1]),
    [bar, foo, baz] = insert_menuitem([bar, baz], foo, [2]),
    [bar, baz, foo] = insert_menuitem([bar, baz], foo, [3]),
    [{x, [foo]}, {y, []}] = insert_menuitem([{x, []}, {y, []}], foo, [1, 1]),
    [{x, []}, {y, [foo]}] = insert_menuitem([{x, []}, {y, []}], foo, [2, 1]),
    [{x, []}, {y, [{z, [bleh, foo]}]}] = insert_menuitem([{x, []}, {y, [{z, [bleh]}]}], foo, [2, 1, 2]),
    
    [b] = delete_path([a, b], [1]),
    [a] = delete_path([a, b], [2]),
    
    [a, {b, []}, c] = delete_path([a, {b, [x]}, c], [2, 1]),
    [a, {b, [x]}, c] = delete_path([a, {b, [x,y]}, c], [2, 2]).
