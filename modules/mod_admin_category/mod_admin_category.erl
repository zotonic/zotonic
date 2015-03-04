%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2015 Marc Worrell
%% @doc Module for editing and managing categories.

%% Copyright 2009-2015 Marc Worrell
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

-module(mod_admin_category).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin category support").
-mod_description("Support editing and changing the category hierarchy.").
-mod_prio(600).
-mod_depends([admin, menu]).
-mod_provides([]).

-export([
         observe_category_hierarchy_save/2,
         observe_admin_menu/3
]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").


observe_category_hierarchy_save(#category_hierarchy_save{tree=New}, Context) ->
    case z_acl:is_allowed(insert, category, Context) of
        true ->
            case m_hierarchy:menu('$category', Context) of
                New -> 
                    ok;
                Old ->
                    % Check if any ids are added or deleted
                    NewIds = lists:sort(ids(New, [])),
                    OldIds = lists:sort(ids(Old, [])),
                    Deleted = OldIds -- NewIds,
                    % Inserted = NewIds -- OldIds,

                    % Delete all ids not in the new category tree
                    lists:map(fun(Id) ->
                                  ok = m_category:delete(Id, undefined, Context)
                              end,
                              Deleted),

                    _ = m_hierarchy:save_nocheck('$category', New, Context),
                    m_category:flush(Context),
                    ok
            end;
        false ->
            undefined
    end.

observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_categories,
                parent=admin_structure,
                label=?__("Categories", Context),
                url={admin_category_sorter},
                visiblecheck={acl, insert, category}}
     
     |Acc].


ids([], Acc) ->
    Acc;
ids([{Id,Sub}|Rest], Acc) ->
    Acc1 = ids(Sub, Acc),
    ids(Rest, [Id|Acc1]).

