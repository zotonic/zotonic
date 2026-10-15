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
    source_links/2,
    event/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("template_compiler/include/template_compiler.hrl").

%% @doc Accept a literal template name from the source viewer; resolution remains index-only.
event(#postback{message = template_matches_source}, Context) ->
    case z_context:get_q(<<"template">>, Context) of
        Name when is_binary(Name), byte_size(Name) > 0, byte_size(Name) < 4096 ->
            Template = case z_context:get_q(<<"is_category">>, Context) of
                <<"true">> -> {cat, Name};
                _ -> Name
            end,
            event(#postback{message = {template_matches, [{template, Template}]}}, Context);
        _ ->
            z_render:growl_error(?__("Invalid template name.", Context), Context)
    end;
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

%% @doc Resolve filters, scomp tags, models, actions and literal delegates to signed editor locations.
%% Only existing module/function atoms are used; unknown filters remain plain text.
-spec source_links(binary(), z:context()) -> [map()].
source_links(Source, Context) ->
    case z_acl:is_allowed(use, mod_development, Context) of
        true ->
            case template_compiler_scanner:scan(<<"source">>, Source) of
                {ok, Tokens} -> source_links_tokens(Tokens, Context);
                {error, _} -> []
            end;
        false -> []
    end.

%% @doc Recognize source references without treating strings or comments as code.
source_links_tokens([{Kind, _, _}, {string_literal, {_, Row, Column}, Name} | Rest], Context)
    when Kind =:= include_keyword; Kind =:= catinclude_keyword ->
    [#{kind => template, name => Name, row => Row, column => Column + 1,
       is_category => Kind =:= catinclude_keyword} | source_links_tokens(Rest, Context)];
source_links_tokens([{identifier, _, <<"template">>}, {equal, _, _},
                     {string_literal, {_, Row, Column}, Name} | Rest], Context) ->
    [#{kind => template, name => Name, row => Row, column => Column + 1}
        | source_links_tokens(Rest, Context)];
source_links_tokens([{identifier, _, <<"action">>}, {equal, _, _},
                     {open_curly, _, _}, {identifier, _, _} = Token | Rest], Context) ->
    source_link(action, Token, Context) ++ source_links_tokens([Token | Rest], Context);
source_links_tokens([{identifier, _, <<"delegate">>}, {equal, _, _},
                     {Type, {Ref, Row, Column}, Name} | Rest], Context)
    when Type =:= atom_literal; Type =:= string_literal ->
    %% Leave the quotes visible and make only the module name clickable.
    source_link(delegate, {identifier, {Ref, Row, Column + 1}, Name}, Context)
        ++ source_links_tokens(Rest, Context);
source_links_tokens([{m_keyword, _, _}, {dot, _, _}, {identifier, _, _} = Token | Rest], Context) ->
    source_link(model, Token, Context) ++ source_links_tokens([Token | Rest], Context);
source_links_tokens([{open_tag, _, _}, {identifier, _, _} = Token | Rest], Context) ->
    source_link(scomp, Token, Context) ++ source_links_tokens([Token | Rest], Context);
source_links_tokens([{Kind, _, _}, {identifier, _, _} = Token | Rest], Context)
    when Kind =:= pipe; Kind =:= filter_keyword ->
    source_link(filter, Token, Context) ++ source_links_tokens([Token | Rest], Context);
source_links_tokens([{string_literal, {_, Row, Column}, Name} | Rest], Context) ->
    %% Explicit references above take precedence over this filename heuristic.
    Links = case filename:extension(Name) of
        <<".tpl">> -> [#{kind => template, name => Name, row => Row, column => Column + 1}];
        _ -> []
    end,
    Links ++ source_links_tokens(Rest, Context);
source_links_tokens([_ | Rest], Context) ->
    source_links_tokens(Rest, Context);
source_links_tokens([], _Context) ->
    [].

%% @doc Attach the token position and kind to an existing source location.
source_link(Kind, {identifier, {_, Row, Column}, Name}, Context) ->
    case reference_source(Kind, Name, Context) of
        undefined -> [];
        Location -> [Location#{kind => Kind, name => Name, row => Row, column => Column}]
    end.

%% @doc Resolve references through the same site-aware lookup as rendering.
reference_source(delegate, Name, Context) ->
    try binary_to_existing_atom(Name, utf8) of
        Module ->
            case code:ensure_loaded(Module) of
                {module, Module} ->
                    case erlang:function_exported(Module, event, 2) of
                        true -> signed_source(z_development_editor:source_location(Module, {event, 2}), Context);
                        false -> undefined
                    end;
                {error, _} -> undefined
            end
    catch
        error:badarg -> undefined
    end;
reference_source(filter, Name, Context) ->
    filter_source(Name, Context);
reference_source(model, Name, Context) ->
    case z_model:get_module(Name, Context) of
        {ok, Module} -> callback_source(Module, m_get, Context);
        {error, _} -> undefined
    end;
reference_source(Kind, Name, Context) when Kind =:= scomp; Kind =:= action ->
    Function = case Kind of
        scomp -> {render, 3};
        action -> {render_action, 4}
    end,
    try binary_to_existing_atom(Name, utf8) of
        IndexedName ->
            case z_module_indexer:find(Kind, IndexedName, Context) of
                {ok, #module_index{erlang_module = Module}} ->
                    callback_source(Module, Function, Context);
                {error, _} -> undefined
            end
    catch
        error:badarg -> undefined
    end.

%% @doc Prefer the rendering/read callback, falling back to the module's source.
callback_source(Module, Function, Context) ->
    Location = case z_development_editor:source_location(Module, Function) of
        {ok, _, _} = Found -> Found;
        {error, _} -> z_development_editor:source_location(Module)
    end,
    signed_source(Location, Context).

%% @doc Find a filter function's source and sign the file and line for editor launch.
filter_source(Name, Context) ->
    try
        Module = binary_to_existing_atom(<<"filter_", Name/binary>>, utf8),
        {module, Module} = code:ensure_loaded(Module),
        Function = binary_to_existing_atom(Name, utf8),
        signed_source(z_development_editor:source_location(Module, Function), Context)
    catch
        error:badarg -> undefined;
        error:{badmatch, {error, _}} -> undefined
    end.

%% @doc Sign only existing source files for the permission-checked editor endpoint.
signed_source({ok, File, Line}, Context) ->
    case filelib:is_regular(File) of
        true ->
            FileB = unicode:characters_to_binary(filename:absname(File)),
            #{file => FileB, line => Line,
              token => z_crypto:pickle({development_editor_source, FileB, Line}, Context)};
        false -> undefined
    end;
signed_source({error, _}, _Context) ->
    undefined.
