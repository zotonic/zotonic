%% @copyright 2015 Marc Worrell
%% @doc Adds content groups to enable access-control rules on resources.

%% Copyright 2015 Marc Worrell
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

-module(mod_content_groups).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Content Groups").
-mod_description("Categorize content into a hierarchical structure of content groups.").
-mod_prio(400).
-mod_schema(1).
-mod_depends([menu, mod_mqtt]).
-mod_provides([]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-export([
    observe_rsc_get/3,
    observe_admin_menu/3,
    manage_schema/2
    ]).

observe_rsc_get(#rsc_get{}, [], Context) ->
    [];
observe_rsc_get(#rsc_get{}, Props, Context) ->
    case proplists:get_value(content_group_id, Props) of
        undefined ->
            [
                {content_group_id, 
                        case m_category:is_meta(proplists:get_value(category_id, Props), Context) of
                            true -> m_rsc:rid(system_content_group, Context);
                            false -> m_rsc:rid(default_content_group, Context)
                        end}
                | proplists:delete(content_group_id, Props)
            ];
        _ ->
            Props
    end.

observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_content_groups,
                parent=admin_structure,
                label=?__("Content groups", Context),
                url={admin_menu_hierarchy, [{name, "content_group"}]},
                visiblecheck={acl, use, mod_admin_config}}
     |Acc].

manage_schema(_Version, Context) ->
    z_datamodel:manage(
              ?MODULE,
              #datamodel{
                categories=[
                    {content_group, meta, [
                        {title, {trans, [{en, "Content Group"}, {nl, "Paginagroep"}]}}
                    ]}
                ],
                resources=[
                    {system_content_group, content_group, [
                        {title, {trans, [{en, "System Content"}, {nl, "Systeempagina’s"}]}}
                    ]},
                    {default_content_group, content_group, [
                        {title, {trans, [{en, "Default Content Group"}, {nl, "Standaard paginagroep"}]}}
                    ]}
                ]
              },
              Context),
    m_hierarchy:ensure(content_group, Context),
    SysId = m_rsc:rid(system_content_group, Context),
    {MetaFrom, MetaTo} = m_category:get_range(meta, Context),
    z_db:q("
        update rsc
        set content_group_id = $1
        where pivot_category_nr >= $2
          and pivot_category_nr <= $3
          and content_group_id is null
        ",
        [SysId, MetaFrom, MetaTo],
        Context),
    ok.

