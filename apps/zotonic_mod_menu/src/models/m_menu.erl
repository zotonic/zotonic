%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2024-2026 Marc Worrell
%% @doc Menu support routines.
%% @end

%% Copyright 2024-2026 Marc Worrell
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

-module(m_menu).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "frontend_developer", "model", "navigation",
        "collection", "query", "backup_and_restore"
    ]
}).
-moduledoc(<<"
Menu queries and removal support. The query paths reuse the menu filters:

| Path | Result |
| --- | --- |
| `rsc/+id` | Menu resource for a page, using the `menu_rsc` notification, `hasmenu`, then `main_menu`. |
| `ids/+menu_id` | All resource ids in a menu, including invisible or non-existing ids, as with `menu_ids`. |
| `trail/+menu_id/+id` | Breadcrumb ids through the visible menu, including the requested page. |
| `subtree/+menu_id/+id` | Visible children below the requested page, excluding the page itself. |

Resource arguments accept ids or names. For example, `m.menu.rsc[id]`,
`m.menu.ids.main_menu`, `m.menu.trail.main_menu[id]`, and
`m.menu.subtree.main_menu[id]`. Return values and ordering follow the respective
filters; `ids` does not filter individual menu items for visibility.

`removal_status` accepts a list of resource ids as its
payload, including every descendant of the menu item being removed. It returns:

* `remove_only`: no dependent pages are included, without revision backup.
* `recover_menu`: no dependent pages are included, with revision backup.
* `delete_dependent`: dependent pages may be permanently deleted.
* `recover_dependent`: dependent pages may be deleted but can be recovered.

The dependency check uses only the `is_dependent` flag. It does not inspect
incoming connections, so the warning indicates possible deletion.
">>).

-export([
    m_get/3
]).

m_get([ <<"rsc">>, Id | Rest ], _Msg, Context) ->
    {ok, {filter_menu_rsc:menu_rsc(m_rsc:rid(Id, Context), Context), Rest}};
m_get([ <<"ids">>, MenuId | Rest ], _Msg, Context) ->
    {ok, {filter_menu_ids:menu_ids(MenuId, Context), Rest}};
m_get([ <<"trail">>, MenuId, Id | Rest ], _Msg, Context) ->
    {ok, {filter_menu_trail:menu_trail(Id, MenuId, Context), Rest}};
m_get([ <<"subtree">>, MenuId, Id | Rest ], _Msg, Context) ->
    {ok, {filter_menu_subtree:menu_subtree(Id, MenuId, Context), Rest}};
m_get([ <<"removal_status">> | Rest ], #{ payload := Ids }, Context) when is_list(Ids) ->
    case lists:all(fun(Id) -> is_integer(Id) andalso Id > 0 end, Ids) of
        true ->
            HasDependent = lists:any(
                fun(Id) -> is_dependent(Id, Context) end,
                lists:usort(Ids)),
            HasBackup = z_module_manager:active(mod_backup, Context),
            {ok, {removal_status(HasDependent, HasBackup), Rest}};
        false ->
            {error, badarg}
    end;
m_get([ <<"removal_status">> | _ ], _Msg, _Context) ->
    {error, badarg};
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.

-spec removal_status(HasDependent, HasBackup) -> binary() when
    HasDependent :: boolean(),
    HasBackup :: boolean().
removal_status(false, false) -> <<"remove_only">>;
removal_status(false, true) -> <<"recover_menu">>;
removal_status(true, false) -> <<"delete_dependent">>;
removal_status(true, true) -> <<"recover_dependent">>.

%% @doc Check whether the page is marked as dependent.
-spec is_dependent(Id, Context) -> boolean() when
    Id :: m_rsc:resource_id(),
    Context :: z:context().
is_dependent(Id, Context) ->
    z_convert:to_bool(m_rsc:p_no_acl(Id, <<"is_dependent">>, Context)).
