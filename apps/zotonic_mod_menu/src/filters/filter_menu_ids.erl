%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell
%% @doc Return all resource ids in a menu.
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
-module(filter_menu_ids).
-moduledoc("
Returns all resource ids in a menu. Could return invisible and non existing resource ids. The returned ids are a flat
list, the hierarchy of the menu is lost.

Example:


```django
{% for mid in id|menu_ids %}
    {{ mid.title }}
{% endif %}
```

See also

[menu_flat](/id/doc_template_filter_filter_menu_flat), [menu_is_visible](/id/doc_template_filter_filter_menu_is_visible)").

-export([ menu_ids/2 ]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Return all resource ids in a menu.
-spec menu_ids(MenuId, Context) -> RscIds when
    MenuId :: m_rsc:resource(),
    Context :: z:context(),
    RscIds :: [ m_rsc:resource_id() ].
menu_ids(undefined, _Context) ->
    [];
menu_ids([#rsc_tree{}|_] = Menu, Context) ->
    menu_ids_1(Menu, Context);
menu_ids([{_,_}|_] = Menu, Context) ->
    menu_ids_1(Menu, Context);
menu_ids(Id, Context) ->
    menu_ids(m_rsc:p(Id, <<"menu">>, Context), Context).

menu_ids_1(Menu, Context) ->
    Ids = menu_list_ids(Menu, []),
    lists:filtermap(
        fun(RId) ->
            case m_rsc:rid(RId, Context) of
                undefined -> false;
                RId2 -> {true, RId2}
            end
        end,
        Ids).

menu_list_ids(undefined, Acc) ->
    Acc;
menu_list_ids([], Acc) ->
    Acc;
menu_list_ids(<<>>, Acc) ->
    Acc;
menu_list_ids([{Id, SubMenu}|Rest], Acc) when is_list(SubMenu) ->
    % Old menu format
    Acc1 = menu_list_ids(SubMenu, Acc),
    menu_list_ids(Rest, [Id|Acc1]);
menu_list_ids([#rsc_tree{ id = Id, tree = SubMenu }|Rest], Acc) ->
    Acc1 = menu_list_ids(SubMenu, Acc),
    menu_list_ids(Rest, [Id|Acc1]).

