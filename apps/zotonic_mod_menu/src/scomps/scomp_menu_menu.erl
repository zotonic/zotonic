%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2012 Marc Worrell
%% @doc Render the menu.  Add classes to highlight the current item.  The menu is always build as seen by the anonymous user.

%% Copyright 2009-2012 Marc Worrell
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

-module(scomp_menu_menu).
-moduledoc("
Show a page menu.

This tag is part of the module mod_menu. The `{% menu %}` tag is used to generate the HTML for the menu defined in the admin.

You can define multiple menus in your site. By default there is one menu, called “main_menu”. If you want another
one, create a page of type “page menu” (under “Categorization”) and start editing your menu. You can use the
“menu_id” argument to select which menu you want to display.

Example:


```erlang
{% menu id=id %}
```

Generates something like:


```erlang
<ul id=\"navigation\" class=\"nav\">
  <li>
    <a href=\"/\" class=\"welcome\">Home</a>
  </li>
  <li>
    <a href=\"/features\" class=\"page_features\">Features</a>
  </li>
  <li>
    <a href=\"/documentation\" class=\"documentation active\">Documentation</a>
  </li>
  <li>
    <a href=\"/documentation/628/installation\" class=\"page_install\">Install</a>
  </li>
</ul>
```

The menu has the following features:

*   The menu is a unordered list.
*   The id of the menu is `navigation` and can be prepended with param `id_prefix`.
*   The class of the menu is set with param `class` (default `nav`).
*   Menu items are a &lt;li> with a single &lt;a>
*   The link of the menu item referring to the current page has the class `active`.
*   Every link also gets the unique name of the target as a class.
*   Every menu item can have single level submenus. A submenu has the same properties as the menu.

| Argument    | Description                                                                      | Example |
| ----------- | -------------------------------------------------------------------------------- | ------- |
| id          | Set this to the id of the current shown page and it wil highlight its page path. |         |
| menu\\\\_id   | The id of the menu that you want to display. If left empty, the main menu is shown. |         |
| id\\\\_prefix | String prepended to menu id.                                                     |         |
| class       | HTML class for the list; default “nav”.                                          |         |
| maxdepth    | Maximum depth of the menu; default 999.                                          |         |
| template    | Template to render the menu; default “\\\\_menu.tpl”                               |         |
").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

% Menu structure is a like:
%
% <ul id="navigation" class="nav">
% 	<li>
% 		<a href="" class="home-page active">home</a>
% 	</li>
% 	<li>
% 		<a href="" class="about-page">about</a>
% 	</li>
% 	<li>
% 		<a href="" class="contact-page">contact</a>
% 	</li>
% </ul>


vary(_Params, _Context) -> default.

% Params:
% menu_id
% is_superfish
% maxdepth
% class (used in template)
% id_prefix (used in template)
render(Params, _Vars, Context) ->
    MenuId = m_rsc:rid(get_menu_id(Params, Context), Context),
    Template = proplists:get_value(template, Params, "_menu.tpl"),
    IdPrefix = proplists:get_value(id_prefix, Params, ""),
    Class = proplists:get_value(class, Params, "nav navbar-nav"),
    MaxDepth = proplists:get_value(maxdepth, Params, 999),
    Menu = mod_menu:get_menu(MenuId, Context),
    Vars = [
        {menu, mod_menu:menu_flat(Menu, MaxDepth, Context)},
        {menu_id, MenuId},
        {id_prefix, IdPrefix},
        {class, Class}
        | Params
    ],
    {ok, z_template:render(Template, Vars, Context)}.

get_menu_id(Params, Context) ->
    case proplists:get_value(menu_id, Params) of
        undefined -> filter_menu_rsc:menu_rsc(proplists:get_value(id, Params), Context);
        MenuId -> MenuId
    end.
