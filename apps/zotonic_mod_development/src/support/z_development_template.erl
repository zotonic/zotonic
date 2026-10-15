%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2023 Marc Worrell
%% @doc Explain template selection.
%% @end

%% Copyright 2014-2023 Marc Worrell
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
    matching_templates/2,
    event/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("template_compiler/include/template_compiler.hrl").

event(#postback{message = {template_matches, Args}}, Context) ->
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            Template = proplists:get_value(template, Args),
            Matches = matching_templates(Template, Context),
            z_render:dialog(?__("Matching templates", Context),
                "_development_template_matches.tpl",
                [{templates, Matches}, {template_option, Template},
                 {is_category, is_category_template(Template)}, {width, <<"large">>}], Context);
        false ->
            z_render:growl_error(?__("You are not allowed to use the template debugging.", Context), Context)
    end;
event(#submit{message=explain_tpl}, Context) ->
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            CatName = z_context:get_q(<<"tpl_cat">>, Context, <<>>),
            TplName = z_string:trim(z_context:get_q(<<"tpl_name">>, Context)),
            Tpl = index_props(find_template(CatName, TplName, Context)),
            Vars = [
                {tpl, Tpl}
            ],
            Context1 = z_render:update(
                            "explain-tpl-output",
                            #render{template="_development_template.tpl", vars=Vars},
                            Context),
            z_render:wire({fade_in, [{target, "explain-tpl-output"}]}, Context1);
        false ->
            z_render:growl(?__("You are not allowed to use the template debugging.", Context), Context)
    end.

find_template(<<>>, TplName, Context) ->
    z_module_indexer:find(template, TplName, Context);
find_template(CatName, TplName, Context) ->
    IsA = m_category:is_a(CatName, Context),
    z_template_compiler_runtime:map_template({cat, TplName, IsA}, #{}, Context).

index_props({error, _}) ->
    undefined;
index_props({ok, #template_file{ filename = Path}}) ->
    [
        {module, <<>>},
        {path, Path}
    ];
index_props({ok, #module_index{filepath=Path, module=Module}}) ->
    [
        {module, Module},
        {path, Path}
    ].

%% @doc List all module overrides; category options also include category and
%% resource-name variants. This lists candidates, not a selected resource's winner.
-spec matching_templates(Template, Context) -> Templates when
    Template :: term(), Context :: z:context(), Templates :: [map()].
matching_templates({overrules, Name, File}, Context) ->
    %% find_all/3 uses the same priority order as runtime overrules resolution.
    case lists:dropwhile(fun(#{path := Path}) -> Path =/= File end,
            template_matches(Name, Context)) of
        [_, Next | _] -> [Next];
        _ -> []
    end;
matching_templates({cat, Name}, Context) -> category_templates(Name, Context);
matching_templates({cat, Name, _}, Context) -> category_templates(Name, Context);
matching_templates(Name, Context) when is_binary(Name); is_list(Name) ->
    template_matches(z_convert:to_binary(Name), Context);
matching_templates(_, _) -> [].

%% @doc Recognize category-dependent template options for the matching-template dialog.
is_category_template({cat, _}) -> true;
is_category_template({cat, _, _}) -> true;
is_category_template(_) -> false.

%% @doc Collect base, category and resource-name variants, including all module overrides.
category_templates(Name, Context) when is_binary(Name); is_list(Name) ->
    NameB = z_convert:to_binary(Name),
    Root = filename:rootname(NameB),
    Ext = filename:extension(NameB),
    Prefix = <<Root/binary, ".">>,
    Names = lists:usort([
        N || #module_index{key = #module_index_key{name = N}} <- z_module_indexer:all(template, Context),
             N =:= NameB orelse (
                 filename:dirname(N) =:= filename:dirname(NameB)
                 andalso filename:extension(N) =:= Ext
                 andalso binary:match(N, Prefix) =:= {0, byte_size(Prefix)})
    ]),
    lists:append([template_matches(N, Context) || N <- Names]);
category_templates(_, _) -> [].

%% @doc Return indexed files for an exact template name in module priority order.
template_matches(Name, Context) ->
    [#{name => Name, module => Module, path => Path}
     || #module_index{module = Module, filepath = Path} <- z_module_indexer:find_all(template, Name, Context)].
