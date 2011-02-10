%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Show the current menu, enables editing the menu.

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
    is_authorized/2,
    event/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, mod_menu, ReqData, Context).


html(Context) ->
	Html = z_template:render("admin_menu.tpl", [{page_admin_menu, true}, {menu, mod_menu:get_menu(Context)}], Context),
	z_context:output(Html, Context).

event({drop, {dragdrop, DragTag, _, _DragEltId}, {dragdrop, DropTag, _, _DropEltId}}, Context) ->
    Menu  = mod_menu:get_menu(Context),
    Menu1 = handle_drop(Menu, DragTag, DropTag),
    save_menu(Menu1, Context),
    Html = z_template:render("_admin_menu_menu_view.tpl", [{menu, Menu1}], Context),
    z_render:update("menu-editor", Html, Context);

event({postback, {delete, Props}, _TriggerId, _TargetId}, Context) ->
    Menu = mod_menu:get_menu(Context),
    Menu1 = case proplists:get_value(item, Props) of
        [Nr] -> 
            remove_nth(Nr, Menu);
        [Nr,SubNr] ->
            {MenuId, SubMenu}  = lists:nth(Nr, Menu),
            SubMenu1 = remove_nth(SubNr, SubMenu),
            set_nth(Nr, {MenuId, SubMenu1}, Menu)
    end,
    Context1 = save_menu(Menu1, Context),
    Html = z_template:render("_admin_menu_menu_view.tpl", [{menu, Menu1}], Context1),
    z_render:update("menu-editor", Html, Context1);

event(_Event, Context) ->
    Context.

%% @doc Save the menu to the site configuration.
%% @spec save_menu(list(), Context) -> ok
save_menu(Menu, Context) ->
    case z_acl:is_allowed(use, mod_menu, Context) of
        true ->
            m_config:set_prop(menu, menu_default, menu, Menu, Context), 
            z_depcache:flush(menu, Context),
            Context;
        false ->
            z_render:growl_error("Sorry, you need to be public publisher to edit the menu.", Context)
    end.


%% @doc Handle the drop of an id on top of a menu item.
handle_drop(Menu, ["new", Id], "top") when is_integer(Id) ->
    [ {Id, []} | Menu ];

%% drag new item to a main-menu-item
handle_drop(Menu, ["new", Id], [DropNr]) when is_integer(Id), is_integer(DropNr) ->
    {DropId, DropSubMenu} = lists:nth(DropNr, Menu),
    set_nth(DropNr, {DropId, [Id | DropSubMenu]}, Menu);

%% drag new item to a sub-menu-item
handle_drop(Menu, ["new", Id], [DropNr, DropSubNr]) when is_integer(Id), is_integer(DropNr), is_integer(DropSubNr) ->
    {DropId, DropSubMenu} = lists:nth(DropNr, Menu),
    DropSubMenu1 = after_nth(DropSubNr, Id, DropSubMenu),
    set_nth(DropNr, {DropId, DropSubMenu1}, Menu);

% drag main menu to top
handle_drop(Menu, [Nr], "top") when is_integer(Nr) ->
    {Id, Sub} = lists:nth(Nr, Menu),
    Menu1 = remove_nth(Nr, Menu),
    [ {Id, Sub} | Menu1 ];

% drag sub-menu-item to top
handle_drop(Menu, [Nr, SubNr], "top") when is_integer(Nr), is_integer(SubNr) ->
    {Id, SubMenu} = lists:nth(Nr, Menu),
    SubId = lists:nth(SubNr, SubMenu),
    SubMenu1 = remove_nth(SubNr, SubMenu),
    [{SubId, []} | set_nth(Nr, {Id,SubMenu1}, Menu) ];

% Menu item "after" another menu item
handle_drop(Menu, [Nr], ["after", DropNr]) when is_integer(Nr), is_integer(DropNr) ->
    DragMenu = lists:nth(Nr, Menu),
    Menu1 = remove_nth(Nr, Menu),
    case Nr > DropNr of
        true -> after_nth(DropNr, DragMenu, Menu1);
        false -> after_nth(DropNr-1, DragMenu, Menu1)
    end;

% Menu item "after" another menu item
handle_drop(Menu, ["new", Id], ["after", DropNr]) when is_integer(DropNr) ->
    DragMenu = {Id, []},
    after_nth(DropNr, DragMenu, Menu);

% Sub menu item "after" another main-menu item
handle_drop(Menu, [Nr, SubNr], ["after", DropNr]) when is_integer(Nr), is_integer(DropNr) ->
    {DragId, DragSubMenu} = lists:nth(Nr, Menu),
    DragSubId = lists:nth(SubNr, DragSubMenu),
    Menu1 = set_nth(Nr, {DragId, remove_nth(SubNr, DragSubMenu)}, Menu),
    after_nth(DropNr, {DragSubId, []}, Menu1);
    
% drag sub-menu-item to main-menu-item
handle_drop(Menu, [Nr, SubNr], [DropNr]) when is_integer(Nr), is_integer(SubNr), is_integer(DropNr)->
    {Id, SubMenu} = lists:nth(Nr, Menu),
    SubId = lists:nth(SubNr, SubMenu),
    % Remove the dragged item from its submenu
    SubMenu1 = remove_nth(SubNr, SubMenu),
    Menu1 = set_nth(Nr, {Id,SubMenu1}, Menu),
    % Add the item to the submenu of DropNr
    {DropId, DropSubMenu} = lists:nth(DropNr, Menu1),
    set_nth(DropNr, {DropId, [SubId | DropSubMenu]}, Menu1);

% drag sub-menu-item to sub-menu-item in same menu but below the old position
handle_drop(Menu, [Nr, SubNr], [Nr, DropSubNr]) when is_integer(Nr), is_integer(SubNr), DropSubNr > SubNr ->
    {Id, SubMenu} = lists:nth(Nr, Menu),
    SubId = lists:nth(SubNr, SubMenu),
    DropSubMenu1 = after_nth(DropSubNr-1, SubId, remove_nth(SubNr, SubMenu)),
    set_nth(Nr, {Id, DropSubMenu1}, Menu);

% drag sub-menu-item to sub-menu-item
handle_drop(Menu, [Nr, SubNr], [DropNr, DropSubNr]) when is_integer(Nr), is_integer(SubNr), is_integer(DropNr)->
    {Id, SubMenu} = lists:nth(Nr, Menu),
    SubId = lists:nth(SubNr, SubMenu),
    % Remove the dragged item from its submenu
    SubMenu1 = remove_nth(SubNr, SubMenu),
    Menu1 = set_nth(Nr, {Id,SubMenu1}, Menu),
    % Add the item to the submenu of DropNr
    {DropId, DropSubMenu} = lists:nth(DropNr, Menu1),
    DropSubMenu1 = after_nth(DropSubNr, SubId, DropSubMenu),
    set_nth(DropNr, {DropId, DropSubMenu1}, Menu1);

% drag main-menu to another main-menu
handle_drop(Menu, [Nr], [DropNr]) when is_integer(Nr), is_integer(DropNr) ->
    {DragId, DragSub} = lists:nth(Nr, Menu),
    {DropId, DropSub} = lists:nth(DropNr, Menu),
    NewSub = [DragId | DragSub ] ++ DropSub,
    Menu1 = set_nth(DropNr, {DropId, NewSub}, Menu),
    remove_nth(Nr, Menu1);



% drag main menu to sub-menu - refuse
handle_drop(Menu, _From, _To) ->
    ?DEBUG({_From, _To}),
    Menu.



remove_nth(Nr, List) ->
    remove_nth(Nr, List, []).

remove_nth(_Nr, [], Acc) ->
    lists:reverse(Acc);
remove_nth(1, [_H|T], Acc) ->
    lists:reverse(Acc, T);
remove_nth(N, [H|T], Acc) ->
    remove_nth(N-1, T, [H|Acc]).


set_nth(Nr, Value, List) ->
    set_nth(Nr, Value, List, []).

set_nth(_Nr, _Value, [], Acc) ->
    lists:reverse(Acc);
set_nth(1, Value, [_H|T], Acc) ->
    lists:reverse([Value|Acc], T);
set_nth(N, Value, [H|T], Acc) ->
    set_nth(N-1, Value, T, [H|Acc]).


after_nth(Nr, Value, List) ->
    after_nth(Nr, Value, List, []).

after_nth(_Nr, Value, [], Acc) ->
    lists:reverse([Value | Acc]);
after_nth(1, Value, [H|T], Acc) ->
    lists:reverse([Value, H | Acc], T);
after_nth(N, Value, [H|T], Acc) ->
    after_nth(N-1, Value, T, [H|Acc]).

