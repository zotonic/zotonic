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
-include_lib("emqtt/include/emqtt.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-export([
    observe_admin_menu/3,
	manage_schema/2
	]).

observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_content_groups,
                parent=admin_auth,
                label=?__("Content Groups", Context),
                url={admin_menu_hierarchy, [{name, "content_group"}]},
                visiblecheck={acl, use, mod_admin_config}}
     |Acc].

manage_schema(_Version, Context) ->
	m_menu_hierarchy:ensure(content_group, Context),
	#datamodel{
		categories=[
			{content_group, undefined, [
				{title, {trans, [{en, "Content Group"}, {nl, "Pagina Groep"}]}}
			]}
		]
	}.
