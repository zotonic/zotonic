%% @author Marc Worrell
%% @copyright 2026 Marc Worrell
%% @doc Regression tests for static model-path completion.
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

-module(z_development_lsp_model_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([m_get/3]).
-model_paths([#{path => [<<"result">>, any, <<"title">>], description => <<"Result title">>}]).

%% A completion must inspect this callback, never call it.
m_get([<<"literal">> | _], _, _) -> error(model_callback_executed).

parse_test() ->
    P = fun z_development_lsp_model_path:parse/1,
    ?assertEqual(#{model => <<"rsc">>, path => [any], part => <<"ti">>, quote => dot}, P(<<"{{ m.rsc[id].ti">>)),
    ?assertEqual(#{model => <<"acl">>, path => [], part => <<>>, quote => dot}, P(<<"{% if m.acl.">>)),
    ?assertMatch(#{path := [any, <<"is_a">>]}, P(<<"{{ m.rsc[foo[bar]].is_a.">>)),
    ?assertMatch(#{path := [<<"mod_base">>], part := <<"ti">>, quote := $"}, P(<<"{{ m.config[\"mod_base\"][\"ti">>)),
    ?assertMatch(#{model := <<"rsc">>}, P(<<"<div title=\"{{ m.rsc[id].ti">>)),
    ?assertEqual(undefined, P(<<"{{ \"m.rsc.foo">>)),
    ?assertEqual(undefined, P(<<"{{ \"{{ m.rsc.foo">>)),
    ?assertMatch(#{model := <<"acl">>}, P(<<"{% with \"}}\" as a %}{{ m.acl.">>)),
    ?assertEqual(undefined, P(<<"outside m.rsc.">>)),
    ?assertEqual(undefined, P(<<"{# m.rsc.">>)),
    ?assertEqual(undefined, P(<<"{{ m.rsc[id].title }} outside">>)),
    ?assertEqual(undefined, P(<<"{{ m.rsc[id">>)),
    ?assertEqual(undefined, P(<<"{{ m.rsc[id].title|escape">>)).

edit_test() ->
    Prefix = unicode:characters_to_binary("😀 <a title=\"{{ m.rsc[id].ti"),
    Source = <<Prefix/binary, "tle }}\"">>,
    Match = z_development_lsp_model_path:parse(Prefix),
    #{start := #{character := Start}, 'end' := #{character := End}} =
        z_development_lsp_model_path:edit(Source, Prefix, Match),
    ?assertEqual(z_development_lsp_text:units(Prefix) - 2, Start),
    ?assertEqual(Start + 5, End),
    Quoted = <<"{{ m.req[\"ho">>,
    #{start := #{character := QS}, 'end' := #{character := QE}} =
        z_development_lsp_model_path:edit(<<Quoted/binary, "st\"] }}">>, Quoted,
            z_development_lsp_model_path:parse(Quoted)),
    ?assertEqual(byte_size(Quoted) - 2, QS),
    ?assertEqual(QS + 4, QE).

inference_test() ->
    {ok, Tokens, _} = erl_scan:string("m_get([<<\"status\">>, <<\"enabled\">> | Rest], _, _) -> Rest; m_get([Id, Key | Rest], _, _) when Key =:= <<\"title\">>; Key =:= <<\"summary\">> -> {Id, Rest}; m_get(_, _, _) -> undefined."),
    {ok, Form} = erl_parse:parse_form(Tokens),
    ?assertEqual(lists:sort([#{path => [<<"status">>, <<"enabled">>]},
        #{path => [any, <<"title">>]}, #{path => [any, <<"summary">>]}]),
        lists:sort(z_development_lsp_model:infer([Form]))).

metadata_test() ->
    Entries = z_development_lsp_model:paths(?MODULE),
    ?assert(lists:member(#{path => [<<"literal">>]}, Entries)),
    ?assert(lists:member(#{path => [<<"result">>, any, <<"title">>], description => <<"Result title">>}, Entries)).

cache_reload_test() ->
    Module = z_development_lsp_model_cache_fixture,
    Dir = filename:join("/tmp", "zotonic-model-cache-" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Dir),
    File = filename:join(Dir, atom_to_list(Module) ++ ".erl"),
    BeamFile = filename:rootname(File) ++ ".beam",
    true = code:add_patha(Dir),
    Load = fun(Key, Options) ->
        Source = iolist_to_binary(["-module(z_development_lsp_model_cache_fixture).\n",
            "-export([m_get/3]).\n-model_paths([#{path => [<<\"", Key,
            "\">>]}]).\nm_get([<<\"fixed\">>|_],_,_) -> error(must_not_execute).\n"]),
        ok = file:write_file(File, Source),
        {ok, Module, Beam} = compile:file(File, [binary | Options]),
        ok = file:write_file(BeamFile, Beam),
        code:soft_purge(Module),
        {module, Module} = code:load_binary(Module, BeamFile, Beam)
    end,
    try
        Load("first", [debug_info]),
        ?assert(lists:member(#{path => [<<"first">>], description => <<>>}, z_development_lsp_model:paths(Module))),
        Load("second", [debug_info]),
        Paths = z_development_lsp_model:paths(Module),
        ?assert(lists:member(#{path => [<<"second">>], description => <<>>}, Paths)),
        ?assertNot(lists:member(#{path => [<<"first">>], description => <<>>}, Paths)),
        Load("source_fallback", [no_debug_info]),
        ?assert(lists:member(#{path => [<<"fixed">>]}, z_development_lsp_model:paths(Module)))
    after
        code:purge(Module), code:delete(Module), code:purge(Module), code:del_path(Dir),
        persistent_term:erase({z_development_lsp_model, paths, Module}),
        file:delete(File), file:delete(BeamFile), file:del_dir(Dir)
    end.

known_keys_test() ->
    Context = z_context:new(lsp_model_test),
    Labels = fun(Text) -> [maps:get(label, I) || I <- complete(Text, false, Context)] end,
    ?assert(lists:member(<<"is_admin">>, Labels(<<"{{ m.acl.is_">>))),
    ?assert(lists:member(<<"title">>, Labels(<<"{{ m.rsc[id].">>))),
    ?assert(lists:member(<<"page_url">>, Labels(<<"{{ m.rsc[id].page_">>))),
    ?assert(lists:member(<<"host">>, Labels(<<"{{ m.req.">>))),
    ?assert(lists:member(<<"language">>, Labels(<<"{{ m.req.">>))),
    ?assert(lists:member(<<"listen_port">>, Labels(<<"{{ m.sysconfig.">>))),
    ?assert(lists:member(<<"timezone">>, Labels(<<"{{ m.sysconfig.">>))),
    ?assertEqual([], Labels(<<"{{ m.acl.is_admin.unknown">>)).

site_data_test() ->
    meck:new(z_module_manager, [non_strict]),
    meck:new(m_predicate, [non_strict]),
    try
        Context = z_context:new(lsp_model_test),
        meck:expect(z_module_manager, active, fun(_) -> [mod_example] end),
        meck:expect(z_module_manager, mod_config, fun(mod_example) ->
            [#{module => mod_example, key => secret, description => "A setting", default => <<"must not leak">>}];
            (zotonic_core) -> [] end),
        [Item] = complete(<<"{{ m.config.mod_example.se">>, false, Context),
        ?assertMatch(#{label := <<"secret">>, documentation := <<"A setting">>}, Item),
        ?assertEqual(nomatch, binary:match(term_to_binary(Item), <<"must not leak">>)),
        meck:expect(m_predicate, all, fun(_) -> [{has_test_predicate, []}] end),
        ?assertEqual([], complete(<<"{{ m.predicate.has_test">>, false, Context)),
        ?assertNot(meck:called(m_predicate, all, ['_'])),
        ?assertMatch([#{label := <<"has_test_predicate">>}], complete(<<"{{ m.predicate.has_test">>, true, Context)),
        ?assertMatch([#{label := <<"has_test_predicate">>}], complete(<<"{{ m.predicate.object_category.has_test">>, true, Context))
    after
        meck:unload(m_predicate), meck:unload(z_module_manager)
    end.

edge_predicates_test() ->
    meck:new(m_predicate, [non_strict]),
    try
        Context = z_context:new(lsp_model_test),
        meck:expect(m_predicate, all, fun(_) -> [{depiction, []}, {author, []}] end),
        lists:foreach(fun(Operation) ->
            Prefix = <<"{{ m.edge.", Operation/binary, "[someid].dep">>,
            ?assertEqual([], complete(Prefix, false, Context)),
            ?assertMatch([#{label := <<"depiction">>}], complete(Prefix, true, Context)),
            Quoted = <<"{{ m.edge.", Operation/binary, "[someid][\"dep">>,
            [#{textEdit := #{range := Range, newText := <<"depiction">>}}] = complete(Quoted, true, Context),
            ?assertEqual(#{start => #{line => 0, character => byte_size(Quoted) - 3},
                'end' => #{line => 0, character => byte_size(Quoted)}}, Range)
        end, [<<"o">>, <<"s">>, <<"o_props">>, <<"s_props">>, <<"id">>, <<"edges">>]),
        meck:reset(m_predicate),
        lists:foreach(fun(Source) ->
            ?assertEqual([], complete(Source, true, Context))
        end, [<<"{{ m.edge.o.dep">>, <<"{{ m.edge.o[id].depiction.dep">>,
              <<"{{ m.edge.graph[id].dep">>, <<"{{ m.edge[id].dep">>]),
        ?assertEqual([], complete(<<"{{ m.edge.o[id].dep">>, false, Context)),
        ?assertNot(meck:called(m_predicate, all, ['_']))
    after
        meck:unload(m_predicate)
    end.

complete(Text, Known, Context) ->
    z_development_lsp_model:complete(Text, Text, z_development_lsp_model_path:parse(Text), Known, Context).
