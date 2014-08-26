%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Explain template selection.

%% Copyright 2014 Marc Worrell
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

-module(z_development_template).

-export([
	event/2,

	find_templates/3
	]).

-include_lib("zotonic.hrl").
-include_lib("wm_host_dispatch_list.hrl").

event(#submit{message=explain_tpl}, Context) ->
	case z_acl:is_allowed(use, mod_development, Context) of
		true ->
			CatName = z_context:get_q("tpl_cat", Context), 
			TplName = z_string:trim(z_context:get_q("tpl_name", Context)),
			Tpls = find_templates(CatName, TplName, Context),
			Vars = [
				{tpls, Tpls}
			],
			Context1 = z_render:update(
							"explain-tpl-output", 
							#render{template="_development_template.tpl", vars=Vars}, 
							Context),
			z_render:wire({fade_in, [{target, "explain-tpl-output"}]}, Context1);
		false ->
			z_render:growl(?__("You are not allowed to use the template debugging.", Context), Context) 
	end.

find_templates(CatName, TplName, Context) ->
	[
		{UA, index_props(find_template(CatName, TplName, z_user_agent:set_class(UA, Context)))}
		|| UA <- z_user_agent:classes()
	].

find_template(NoCat, TplName, Context) when NoCat =:= ""; NoCat =:= <<>> ->
	z_template:find_template(TplName, Context);
find_template(CatName, TplName, Context) ->
	IsA = m_category:is_a(CatName, Context),
	z_template:find_template_cat(TplName, IsA, Context).

index_props({error, _}) ->
	undefined;
index_props({ok, #module_index{filepath=Path, module=Module}}) ->
	[
		{module, Module},
		{path, Path}
	].
