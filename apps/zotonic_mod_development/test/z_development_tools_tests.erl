%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Tests for development inspection APIs and production access restrictions.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_development_tools_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("template_compiler/include/template_compiler.hrl").

tools_test_() -> z_test_support:isolated(fixture()).

fixture() ->
    {setup, fun setup/0, fun(_) -> meck:unload() end, fun(_) -> [
        {"all inspection and capture operations reject production, including sudo", fun production/0},
        {"all supported non-production environments require admin", fun permissions/0},
        {"trusted CLI cannot bypass the production guard", fun cli_production/0},
        {"dispatch traces stay on the supplied site and produce JSON", fun dispatch/0},
        {"template lookup uses category resolution and preserves priority order", fun templates/0},
        {"template listing accepts empty prefix and validates pagination", fun listing/0},
        {"template source is resolved, bounded UTF-8 with access checks", fun source/0},
        {"composition uses compiler metadata without rendering", fun composition/0},
        {"model discovery respects site resolution and documents missing metadata", fun models/0}
    ] end}.

setup() ->
    meck:new([z_acl, m_site, z, z_module_indexer, z_module_manager,
        z_template_compiler_runtime, m_rsc], [no_link]),
    meck:new([z_context, z_sites_manager], [passthrough, no_link]),
    meck:expect(z_context, new, fun(prod_tools_test) -> ctx(production) end),
    meck:expect(z_sites_manager, get_sites, fun() -> #{prod_tools_test => running} end),
    meck:expect(z_acl, sudo, fun(C) -> C#context{props = (C#context.props)#{admin => true}} end),
    meck:expect(z_acl, is_admin, fun(C) -> maps:get(admin, C#context.props, false) end),
    meck:expect(z_acl, is_allowed, fun(use, mod_development, C) -> maps:get(developer, C#context.props, true) end),
    meck:expect(m_site, environment, fun(C) -> maps:get(environment, C#context.props, production) end),
    meck:expect(z, dispatch_list, fun(_) -> {ok, #{dispatch_list => [#{path => [{id, "[0-9]+"}],
        controller_options => [{template, <<"page.tpl">>}]}]}} end),
    meck:expect(z, dispatch_path, fun(Path, C) -> {ok, #{path => Path, site => C#context.site}} end),
    meck:expect(z_module_manager, prio, fun(site) -> 1; (mod_base) -> 500 end),
    meck:expect(z_module_indexer, all, fun(template, _) -> [idx(<<"page.tpl">>, site), idx(<<"page.tpl">>, mod_base), idx(<<"base.tpl">>, mod_base)] end),
    meck:expect(z_module_indexer, find, fun(template, <<"page.tpl">>, _) -> {ok, idx(<<"page.tpl">>, site)};
        (template, <<"base.tpl">>, _) -> {ok, idx(<<"base.tpl">>, mod_base)} end),
    meck:expect(z_module_indexer, find_all, fun(template, Name, _) -> [idx(Name, site), idx(Name, mod_base)] end),
    meck:expect(m_rsc, rid, fun(<<"article">>, _) -> 42; (_, _) -> undefined end),
    meck:expect(z_template_compiler_runtime, map_template, fun
        ({cat, <<"page.tpl">>, 42}, #{}, _) ->
            {ok, #template_file{template = <<"page.article.tpl">>, filename = <<"/site/page.article.tpl">>}};
        (<<"page.tpl">>, #{}, _) ->
            {ok, #template_file{template = <<"page.tpl">>, filename = <<"/site/page.tpl">>}}
    end).

-spec ctx(atom()) -> z:context().
ctx(Env) -> #context{site = test_site, props = #{admin => true, environment => Env}}.
get(Path, P, C) -> m_development_tools:m_get(Path, #{payload => P}, C).
idx(Name, Mod) -> #module_index{key = #module_index_key{name = Name}, module = Mod, filepath = <<"/template.tpl">>}.

production() ->
    lists:foreach(fun(Env) ->
        C = ctx(Env),
        lists:foreach(fun(Path) ->
            ?assertEqual({error, eacces}, get(Path, #{}, C))
        end, [[<<"dispatch">>, <<"list">>], [<<"dispatch">>, <<"trace">>],
            [<<"template">>, <<"lookup">>], [<<"template">>, <<"source">>], [<<"template">>, <<"composition">>], [<<"templates">>], [<<"modules">>], [<<"models">>], [<<"model">>, <<"documentation">>]]),
        lists:foreach(fun(Op) ->
            ?assertEqual({error, eacces}, m_development_capture:m_get([Op, <<"id">>], #{}, C))
        end, [<<"list">>, <<"status">>, <<"trace">>, <<"console">>]),
        lists:foreach(fun(Op) ->
            ?assertEqual({error, eacces}, m_development_capture:m_post([Op, <<"id">>], #{}, C))
        end, [<<"attach">>, <<"start">>, <<"stop">>, <<"poll">>])
    end, [production, education, backup, undefined]).

permissions() ->
    lists:foreach(fun(Env) ->
        C = ctx(Env),
        ?assert(m_development_tools:allowed(C)),
        ?assert(m_development_capture:console_allowed(C)),
        ?assertNot(m_development_tools:allowed(C#context{props = (C#context.props)#{admin => false}})),
        ?assertNot(m_development_tools:allowed(C#context{props = (C#context.props)#{developer => false}}))
    end, [development, test, acceptance]).

dispatch() ->
    C = ctx(development),
    {ok, {#{site := test_site, path := <<"/nl/example">>}, []}} =
        get([<<"dispatch">>, <<"trace">>], #{<<"path">> => <<"/nl/example">>}, C),
    lists:foreach(fun(Path) ->
        ?assertEqual({error, badarg}, get([<<"dispatch">>, <<"trace">>], #{<<"path">> => Path}, C))
    end, [<<"https://production.example/">>, <<"//production.example/">>, 1]),
    {ok, {Result, []}} = get([<<"dispatch">>, <<"list">>], #{}, C),
    ?assert(is_binary(z_json:encode(Result))).

templates() ->
    C = ctx(test),
    {ok, {#{name := <<"page.article.tpl">>, candidates := [#{priority := 1}, #{priority := 500}]}, []}} =
        get([<<"template">>, <<"lookup">>], #{<<"name">> => <<"page.tpl">>, <<"id">> => <<"article">>}, C),
    lists:foreach(fun(Name) ->
        ?assertEqual({error, badarg}, get([<<"template">>, <<"lookup">>], #{<<"name">> => Name}, C))
    end, [<<"/etc/passwd">>, <<"../secret">>, <<"a/../../secret">>, <<>>, 42]),
    ?assertEqual({error, enoent}, get([<<"template">>, <<"lookup">>],
        #{<<"name">> => <<"page.tpl">>, <<"id">> => <<"missing">>}, C)).

listing() ->
    C = ctx(acceptance),
    {ok, {#{total := 2, items := [#{name := <<"base.tpl">>}, #{name := <<"page.tpl">>}]}, []}} =
        get([<<"templates">>], #{}, C),
    {ok, {#{total := 1}, []}} = get([<<"templates">>], #{<<"prefix">> => <<"page">>}, C),
    {ok, {#{items := [#{name := <<"page.tpl">>}]}, []}} =
        get([<<"templates">>], #{<<"offset">> => <<"1">>, <<"limit">> => <<"1">>}, C),
    ?assertEqual({error, badarg}, get([<<"templates">>], #{<<"limit">> => <<"201">>}, C)),
    ?assertEqual({error, badarg}, get([<<"templates">>], #{<<"offset">> => <<"-1">>}, C)).

cli_production() ->
    ?assertEqual({error, eacces}, m_development_tools:cli("prod_tools_test", ["dispatch"])),
    ?assertEqual({error, eacces}, m_development_tools:cli("prod_tools_test", ["template", "page.tpl"])),
    ?assertEqual({error, eacces}, m_development_capture:cli("prod_tools_test", "list", "")),
    ?assertEqual({error, eacces}, m_development_capture:cli("prod_tools_test", "start", "id")),
    ?assertEqual({error, eacces}, m_development_capture:cli("prod_tools_test", "console", "id")).

source() ->
    File = filename:join("/tmp", "zotonic-template-source-" ++ binary_to_list(z_ids:id(16)) ++ ".tpl"),
    FileB = unicode:characters_to_binary(File),
    Parent = File ++ ".parent.tpl",
    ParentB = unicode:characters_to_binary(Parent),
    Other = File ++ ".other.tpl",
    OtherB = unicode:characters_to_binary(Other),
    C = ctx(development),
    Path = [<<"template">>, <<"source">>],
    P = #{<<"name">> => <<"page.tpl">>, <<"id">> => <<"article">>},
    meck:expect(z_template_compiler_runtime, map_template,
        fun({cat, <<"page.tpl">>, 42}, #{}, _) ->
            {ok, #template_file{template = <<"page.article.tpl">>, filename = FileB}}
        end),
    meck:expect(z_module_indexer, find_all, fun
        (template, <<"page.article.tpl">> = Name, _) ->
            [(idx(Name, site))#module_index{filepath = FileB},
             (idx(Name, mod_base))#module_index{filepath = ParentB}];
        (template, <<"other.tpl">> = Name, _) ->
            [(idx(Name, site))#module_index{filepath = OtherB}]
    end),
    try
        ok = file:write_file(Parent, <<"Parent source">>),
        ok = file:write_file(Other, <<"Unrelated indexed template">>),
        Text = <<"{% extends \"base.tpl\" %}\n<h1>Héllo 世界</h1>\n"/utf8>>,
        ok = file:write_file(File, Text),
        {ok, {Result, []}} = get(Path, P#{<<"filepath">> => <<"/etc/passwd">>}, C),
        ?assertEqual(Text, maps:get(source, Result)),
        ?assertEqual(<<"page.article.tpl">>, maps:get(name, Result)),
        ?assertEqual(FileB, maps:get(filepath, Result)),
        ?assertEqual(0, maps:get(candidate, Result)),
        ?assertMatch([#{candidate := 0}, #{candidate := 1}], maps:get(candidates, Result)),
        {ok, {Overridden, []}} = get(Path, P#{<<"candidate">> => <<"1">>}, C),
        ?assertEqual(<<"Parent source">>, maps:get(source, Overridden)),
        ?assertEqual(ParentB, maps:get(filepath, Overridden)),
        ?assertEqual(FileB, maps:get(winner_filepath, Overridden)),
        ?assertEqual(mod_base, maps:get(module, Overridden)),
        ?assertEqual(1, maps:get(candidate, Overridden)),
        ?assertMatch({ok, {#{source := Text}, []}}, get(Path, P#{<<"candidate">> => 0}, C)),
        lists:foreach(fun(Selector) ->
            ?assertEqual({error, badarg}, get(Path, P#{<<"candidate">> => Selector}, C))
        end, [-1, <<"-1">>, 1.5, <<"../secret">>, OtherB, #{}, null]),
        ?assertEqual({error, enoent}, get(Path, P#{<<"candidate">> => 2}, C)),
        ?assertEqual({error, eacces}, get(Path, P#{<<"candidate">> => 1}, ctx(production))),
        ?assertEqual({error, eacces}, get(Path, P#{<<"candidate">> => 1},
            C#context{props = #{admin => false}})),
        %% A removed candidate must not fall back to the winner or accept a caller path.
        meck:expect(z_module_indexer, find_all, fun(template, Name, _) ->
            [(idx(Name, site))#module_index{filepath = FileB}]
        end),
        ?assertEqual({error, enoent}, get(Path,
            P#{<<"candidate">> => 1, <<"filepath">> => ParentB}, C)),
        ?assert(is_binary(z_json:encode(Result))),
        lists:foreach(fun(Name) ->
            ?assertEqual({error, badarg}, get(Path, #{<<"name">> => Name}, C))
        end, [<<"/etc/passwd">>, <<"../secret">>, <<"a/../../secret">>, <<>>, 42]),
        ?assertEqual({error, badarg}, get(Path, #{<<"filepath">> => FileB}, C)),
        ?assertEqual({error, eacces}, get(Path, P, ctx(production))),
        ?assertEqual({error, eacces}, get(Path, P, C#context{props = #{admin => false}})),
        ok = file:write_file(File, <<>>),
        ?assertMatch({ok, {#{source := <<>>}, []}}, get(Path, P, C)),
        ok = file:write_file(File, binary:copy(<<"a">>, 1048576)),
        ?assertMatch({ok, {#{source := _}, []}}, get(Path, P, C)),
        ok = file:write_file(File, binary:copy(<<"a">>, 1048577)),
        ?assertEqual({error, too_large}, get(Path, P, C)),
        ok = file:write_file(File, <<255>>),
        ?assertEqual({error, invalid_utf8}, get(Path, P, C)),
        ok = file:delete(File),
        ?assertEqual({error, enoent}, get(Path, P, C))
    after
        file:delete(File),
        file:delete(Parent),
        file:delete(Other)
    end.

composition() ->
    {ok, _} = application:ensure_all_started(template_compiler),
    {ok, Child} = template_compiler:compile_binary(
        <<"{% overrules %}\n{% block body %}\n{% include \"_item.tpl\" %}\n"
          "{% catinclude \"_card.tpl\" id %}\n{% all include \"_head.tpl\" %}\n"
          "{% include dynamic_template %}\n{% endblock %}">>,
        <<"/composition/site.tpl">>, [], undefined),
    {ok, Parent} = template_compiler:compile_binary(
        <<"{% extends \"base.tpl\" %}{% block title %}Title{% endblock %}">>,
        <<"/composition/module.tpl">>, [], undefined),
    {ok, Base} = template_compiler:compile_binary(<<"{% block body %}{% endblock %}">>,
        <<"/composition/base.tpl">>, [], undefined),
    meck:new(z_template, [no_link]),
    meck:expect(z_template, template_module, fun
        (#template_file{filename = <<"/composition/site.tpl">>}, #{}, _) -> {ok, Child};
        (#template_file{filename = <<"/composition/module.tpl">>}, #{}, _) -> {ok, Parent};
        (#template_file{filename = <<"/composition/base.tpl">>}, #{}, _) -> {ok, Base}
    end),
    meck:expect(z_template_compiler_runtime, map_template, fun
        ({cat, <<"page.tpl">>, 42}, #{}, _) ->
            {ok, #template_file{template = <<"page.article.tpl">>, filename = <<"/composition/site.tpl">>}};
        (<<"base.tpl">>, #{}, _) ->
            {ok, #template_file{template = <<"base.tpl">>, filename = <<"/composition/base.tpl">>}}
    end),
    meck:expect(z_module_indexer, find_all, fun
        (template, <<"page.article.tpl">> = Name, _) ->
            [(idx(Name, site))#module_index{filepath = <<"/composition/site.tpl">>},
             (idx(Name, mod_base))#module_index{filepath = <<"/composition/module.tpl">>}];
        (template, <<"base.tpl">> = Name, _) ->
            [(idx(Name, mod_base))#module_index{filepath = <<"/composition/base.tpl">>}]
    end),
    C = ctx(development),
    Path = [<<"template">>, <<"composition">>],
    P = #{<<"name">> => <<"page.tpl">>, <<"id">> => <<"article">>},
    {ok, {Result, []}} = get(Path, P, C),
    ?assertMatch(#{analysis := static, blocks := [body], blocks_scope := local,
        includes_scope := literal_references,
        inheritance := #{type := overrules, target := #{candidate := 1}}}, Result),
    ?assertMatch([
        #{template := <<"_item.tpl">>, line := 3, method := normal, is_catinclude := false},
        #{template := <<"_card.tpl">>, line := 4, is_catinclude := true},
        #{template := <<"_head.tpl">>, line := 5, method := all}
    ], maps:get(includes, Result)),
    ?assert(is_binary(z_json:encode(Result))),
    {ok, {ParentResult, []}} = get(Path, P#{<<"candidate">> => 1}, C),
    ?assertMatch(#{blocks := [title], inheritance := #{type := extends,
        template := <<"base.tpl">>, target := #{name := <<"base.tpl">>}}}, ParentResult),
    ?assertMatch({ok, {#{inheritance := #{type := none}}, []}},
        get(Path, #{<<"name">> => <<"base.tpl">>}, C)),
    ?assertEqual({error, badarg}, get(Path, P#{<<"candidate">> => <<"/etc/passwd">>}, C)),
    ?assertEqual({error, enoent}, get(Path, P#{<<"candidate">> => 2}, C)),
    ?assertEqual({error, badarg}, get(Path, #{<<"name">> => <<"../secret">>}, C)),
    ?assertEqual({error, eacces}, get(Path, P, ctx(production))),
    ?assertEqual({error, eacces}, get(Path, P, C#context{props = #{admin => false}})),
    meck:expect(z_template_compiler_runtime, map_template, fun(<<"base.tpl">>, #{}, _) -> {error, enoent};
        ({cat, <<"page.tpl">>, 42}, #{}, _) ->
            {ok, #template_file{template = <<"page.article.tpl">>, filename = <<"/composition/site.tpl">>}}
    end),
    ?assertMatch({ok, {#{inheritance := #{type := extends, error := enoent}}, []}},
        get(Path, P#{<<"candidate">> => 1}, C)),
    meck:expect(z_template, template_module, fun(_, _, _) -> {error, syntax_error} end),
    ?assertEqual({error, syntax_error}, get(Path, P, C)).

models() ->
    meck:new([z_model, z_development_lsp_docs, z_development_lsp_model], [no_link]),
    Core = #module_index{key = #module_index_key{name = core},
        module = zotonic_core, erlang_module = m_development_tools, filepath = <<"/core.erl">>},
    Shadow = Core#module_index{module = site, erlang_module = m_site, filepath = <<"/shadow.erl">>},
    Alpha = Core#module_index{key = #module_index_key{name = alpha},
        module = site, erlang_module = m_development_capture, filepath = <<"/alpha.erl">>},
    meck:expect(z_module_indexer, all_files, fun(model, zotonic_core) -> [Core] end),
    meck:expect(z_module_indexer, all, fun(model, _) -> [Shadow, Alpha, Alpha] end),
    meck:expect(z_model, get_module, fun
        (<<"core">>, _) -> {ok, m_development_tools};
        (<<"alpha">>, _) -> {ok, m_development_capture}
    end),
    meck:expect(z_development_lsp_docs, module, fun
        (m_development_capture) -> <<"# Capture API\nGET /trace/+id\nPOST /stop/+id">>;
        (m_development_tools) -> <<>>
    end),
    meck:expect(z_development_lsp_model, paths, fun
        (m_development_capture) -> [#{path => [<<"trace">>, any]}];
        (m_development_tools) -> []
    end),
    C = ctx(development),
    {ok, {#{items := [A, B], total := 2}, []}} = get([<<"models">>], #{}, C),
    ?assertMatch(#{name := <<"alpha">>, erlang_module := m_development_capture,
        methods := [get, post]}, A),
    ?assertMatch(#{name := <<"core">>, filepath := <<"/core.erl">>, module := zotonic_core}, B),
    ?assertMatch({ok, {#{items := [#{name := <<"core">>}], total := 1}, []}},
        get([<<"models">>], #{<<"prefix">> => <<"co">>}, C)),
    ?assertMatch({ok, {#{items := [#{name := <<"core">>}], total := 2}, []}},
        get([<<"models">>], #{<<"offset">> => <<"1">>, <<"limit">> => <<"1">>}, C)),
    ?assertEqual({error, badarg}, get([<<"models">>], #{<<"limit">> => 201}, C)),
    Path = [<<"model">>, <<"documentation">>],
    {ok, {Doc, []}} = get(Path, #{<<"name">> => <<"alpha">>}, C),
    ?assertMatch(#{documentation_available := true, path_hints_complete := false,
        path_hints := [#{method := get, path := [<<"trace">>, any]}]}, Doc),
    ?assert(is_binary(z_json:encode(Doc))),
    ?assertMatch({ok, {#{documentation_available := false, documentation := <<>>}, []}},
        get(Path, #{<<"name">> => <<"core">>}, C)),
    lists:foreach(fun(Name) ->
        ?assertEqual({error, enoent}, get(Path, #{<<"name">> => Name}, C))
    end, [<<"os">>, <<"m_development_capture">>, <<"/etc/passwd">>, <<"inactive_model">>]),
    ?assertEqual({error, eacces}, get(Path, #{<<"name">> => <<"alpha">>}, ctx(production))),
    ?assertEqual({error, eacces}, get([<<"models">>], #{}, C#context{props = #{admin => false}})),
    ?assertNot(meck:called(z_development_lsp_docs, module, [os])).
