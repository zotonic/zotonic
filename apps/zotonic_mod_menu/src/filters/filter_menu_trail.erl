%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011-2021 Arjan Scherpenisse
%% @doc Get a "trail" of menu parents

%% Copyright 2011-2021 Arjan Scherpenisse
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
-moduledoc("
See also

[menu\\_subtree](/id/doc_template_filter_filter_menu_subtree), [menu\\_flat](/id/doc_template_filter_filter_menu_flat)

Return a breadcrumb navigation trail for the given id.

This filter locates the filter value which represents the current page in the main menu or in the menu saved in the
resource of the given id.

For example:


```django
{% print id|menu_trail:55 %}
```

Could print the list `[13, 33]` if ids 13 and 33 are the parents of the id argument in the menu resource 55.

If no argument is given, it takes menu from the resource with the name `main_menu`.

Instead of a single id it is possible to give a list of ids. The trail for the first id that returns a trail in the menu
is returned.



Showing Menu Trail only for submenu items
-----------------------------------------

It is sometimes useful to suppress the menu trail on top level items. Here is how to do it.

The [menu\\_trail](#filter-menu-trail) includes the whole path through the menu to the current page if it is reachable
that way. Sometimes it may seem pointless to show the menu trail if we are on the first level of the menu. If we want to
avoid this we need to avoid rendering a trail when it is less than two items long.

One simple condition change to \\_article\\_chapeau.tpl from the blog skeleton makes this work:


```django
{% with id|menu_trail as parents %}
  {% if parents|length > 1 %}
  <h5 class=\"chapeau\">
  {% for p in parents %}
  <a href=\"{{ m.rsc[p].page_url }}\">{{ m.rsc[p].title }}</a>
  {% if not forloop.last %}&raquo;{% endif %}
{% endfor %}</h5>{% endif %}{% endwith %}
```

The key here is `{% if parents|length \\> 1 %}` in place of just `{% if parents %}`.

The [if](/id/doc_template_tag_tag_if) tag is now rendering the menu\\_trail only if there are two or more items in it
which - as I mentioned before - happens when you are at least two levels deep in the menu.
").
-export([menu_trail/2, menu_trail/3, test/0]).

-include_lib("zotonic_core/include/zotonic.hrl").

menu_trail(undefined, _Context) ->
    undefined;
menu_trail(Id, Context) ->
    menu_trail(Id, filter_menu_rsc:menu_rsc(Id, Context), Context).

menu_trail(_Id, undefined, _Context) ->
    undefined;
menu_trail(_Id, [], _Context) ->
    undefined;
menu_trail(_Id, <<>>, _Context) ->
    undefined;
menu_trail(undefined, _Menu, _Context) ->
    undefined;
menu_trail(Ids, Menu, Context) when is_list(Ids) ->
    lists:foldl(
        fun
            (Id, []) -> menu_trail(Id, Menu, Context);
            (Id, undefined) -> menu_trail(Id, Menu, Context);
            (_Id, Trail) -> Trail
        end,
        undefined,
        Ids);
menu_trail(Id, [ #rsc_tree{} | _ ] = Menu, Context) ->
    trail(m_rsc:rid(Id, Context), Menu, Context);
menu_trail(Id, [{_MenuId,Sub}|_] = Menu, Context) when is_list(Sub) ->
    % Old style nested tuples
    trail(m_rsc:rid(Id, Context), Menu, Context);
menu_trail(Id, MenuId, Context) ->
    Menu = mod_menu:get_menu(MenuId, Context),
    trail(m_rsc:rid(Id, Context), Menu, Context).


trail(_Id, [], _Context) ->
    []; %% not found
trail(Id, [ #rsc_tree{ id = Id }|_], _Context) ->
    [ Id ]; %% found
trail(Id, [ #rsc_tree{ id = RscId, tree = Children } | Rest ], Context) ->
    case m_rsc:rid(RscId, Context) of
        Id ->
            [ Id ];
        MenuId ->
            case trail(Id, Children, Context) of
                [] ->
                    %% not found
                    trail(Id, Rest, Context);
                Path ->
                    %% Found
                    [ MenuId | Path ]
            end
    end;
trail(Id, [{RscId, Sub} | Rest ], Context) ->
    %% legacy menu notation
    trail(Id, [ #rsc_tree{ id = RscId, tree = Sub } | Rest ], Context).


test() ->
    [1] = trail(1, [{1, []}], x),
    [] = trail(1, [{33, []}], x),
    [33,66] = trail(66, [{4, []}, {33, [{3, []}, {66, []}]}], x).
