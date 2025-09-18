%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2022 Marc Worrell
%% @doc Filter a list of menu items on visibility. Does not filter sub-menus.

%% Copyright 2013-2022 Marc Worrell
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

-module(filter_menu_is_visible).
-moduledoc("
Filters a list of menu items on visibility and existance. Only top-level menu items that are both visible and exist are
kept in the list. Note that sub-menus are not filtered, they need to be filtered separately.

The [is\\_visible](/id/doc_template_filter_filter_is_visible) filter can’t be used due to the structure of a menu item list.

Example:


```django
{% with m.rsc.main_menu.menu|menu_is_visible as menu %}
        {% if menu %}
        <ul>
                {% for item in menu %}
                        <li>{{ item.id.title }}</li>
                {% endfor %}
        </ul>
        {% endif %}
{% endwith %}
```
").
-export([menu_is_visible/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

menu_is_visible(Items, Context) when is_list(Items) ->
	lists:filter(fun(Item) -> is_visible(Item, Context) end, Items);
menu_is_visible(_, _Context) ->
	[].

is_visible(undefined, _Context) ->
	false;
is_visible(#rsc_tree{ id = RscId }, Context) ->
	is_visible(RscId, Context);
is_visible({RscId, _Items}, Context) ->
	is_visible(RscId, Context);
is_visible(RscId, Context) ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            false;
        Id ->
            z_acl:rsc_visible(Id, Context)
            andalso m_rsc:exists(Id, Context)
    end.
