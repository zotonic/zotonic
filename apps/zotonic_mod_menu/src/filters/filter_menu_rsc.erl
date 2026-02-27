%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2023 Marc Worrell
%% @doc Return the menu of a resource.
%% @end

%% Copyright 2013-2023 Marc Worrell
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

-module(filter_menu_rsc).
-moduledoc("
Return the menu to be displayed with a resource.

Checks the results of the [menu_rsc](/id/doc_notification_menu_rsc#menu-rsc) notification. If that returns undefined
then the predicate `hasmenu` is used to find menus attached to the resource. The first in the list is returned by this filter.

If no menu is found then the menu resource id with name `main_menu` is returned.

Example:


```django
{% if id|menu_rsc as menu_id %}
    .. display the menu with id menu_id ..
{% endif %}
```
").
-export([menu_rsc/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

menu_rsc(RscId, Context) ->
	case z_notifier:first(#menu_rsc{ id = RscId }, Context) of
		undefined ->
			case m_edge:objects(RscId, hasmenu, Context) of
				[MId|_] -> MId;
				[] -> m_rsc:rid(main_menu, Context)
			end;
		none ->
			undefined;
		MenuId ->
			m_rsc:rid(MenuId, Context)
	end.
