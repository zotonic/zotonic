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
        {"template listing accepts empty prefix and validates pagination", fun listing/0}
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
            [<<"template">>, <<"lookup">>], [<<"templates">>], [<<"modules">>]]),
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
