%% @copyright 2015 Arjan Scherpenisse
%% @doc Adds content groups to enable access-control rules on resources.

%% Copyright 2015 Arjan Scherpenisse
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

-module(mod_acl_user_groups).
-author("Arjan Scherpenisse <arjan@miraclethings.nl>").

-mod_title("ACL User Groups").
-mod_description("Organize users into hierarchical groups").
-mod_prio(400).
-mod_schema(1).
-mod_depends([menu]).
-mod_provides([]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-export([
         init/1,
         observe_admin_menu/3,
         manage_schema/2
        ]).

init(Context) ->
    m_acl_rule:init(Context).

observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_acl_user_groups,
                parent=admin_auth,
                label=?__("User groups", Context),
                url={admin_menu_hierarchy, [{name, "acl_user_group"}]},
                visiblecheck={acl, use, mod_acl_user_groups}},
     #menu_item{id=admin_content_groups,
                parent=admin_auth,
                label=?__("Access control rules", Context),
                url={admin_acl_rules, []},
                visiblecheck={acl, use, mod_acl_user_groups}}
     |Acc].

manage_schema(install, Context) ->
	m_hierarchy:ensure(acl_user_group, Context),
    z_datamodel:manage(
      ?MODULE,
      #datamodel{
         categories=
             [
              {acl_user_group, meta,
               [
                {title, {trans, [{en, "User Group"}, {nl, "Gebruikersgroep"}]}}
               ]}
             ],

         resources=
             [
              {acl_user_group_anonymous,
               acl_user_group,
               [{title, {trans, [{en, "Anonymous"}, {nl, "Anoniem"}]}}]},
              {acl_user_group_members,
               acl_user_group,
               [{title, {trans, [{en, "Members"}, {nl, "Gebruikers"}]}}]},
              {acl_user_group_editors,
               acl_user_group,
               [{title, {trans, [{en, "Editors"}, {nl, "Redactie"}]}}]},
              {acl_user_group_managers,
               acl_user_group,
               [{title, {trans, [{en, "Managers"}, {nl, "Beheerders"}]}}]}
             ]
        },
      Context),

    R = fun(N) -> m_rsc:rid(N, Context) end,
    Tree = m_hierarchy:menu(acl_user_group, Context),
    NewTree = [ {R(acl_user_group_anonymous),
                 [ {R(acl_user_group_members),
                    [ {R(acl_user_group_editors),
                       [ {R(acl_user_group_managers),
                          []
                         } ]
                      } ]
                   } ]
                } | Tree ],
    m_hierarchy:save(acl_user_group, NewTree, Context),
    ok.
