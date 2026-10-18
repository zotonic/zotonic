%% @author Marc Worrell
%% @copyright 2026 Marc Worrell
%% @doc Language server support for Zotonic development.
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

-module(z_development_lsp_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([site_tests/1]).

unicode_uri_test() ->
    File = unicode:characters_to_binary("/tmp/é space #%.tpl"),
    ?assertEqual(File, z_development_lsp_text:path(z_development_lsp_text:uri(File))),
    ?assertError(_, z_development_lsp_text:path(<<"https://example.org/a.tpl">>)),
    ?assertError(_, z_development_lsp_text:path(<<"file://remote/a.tpl">>)),
    ?assertError(_, z_development_lsp_text:path(<<"file:///a%00.tpl">>)).

utf16_test() ->
    Source = unicode:characters_to_binary("😀é {{ a|escape }}"),
    ?assertEqual(#{start => #{line => 0, character => 9}, 'end' => #{line => 0, character => 15}},
        z_development_lsp_text:range(Source, 1, 9, <<"escape">>)),
    ?assertEqual(unicode:characters_to_binary("😀é"),
        z_development_lsp_text:prefix(Source, #{<<"line">> => 0, <<"character">> => 3})),
    ?assertError(_, z_development_lsp_text:prefix(Source, #{<<"line">> => 0, <<"character">> => 1})).

framing_test() ->
    Frame = z_development_lsp:frame(#{result => unicode:characters_to_binary("😀é")}),
    [Header, Body] = binary:split(Frame, <<"\r\n\r\n">>),
    ?assertEqual(byte_size(Body), z_development_lsp:header_length(Header)),
    ?assertError(_, z_development_lsp:header_length(<<"Content-Length: 1\r\nContent-Length: 2">>)),
    ?assertError(_, z_development_lsp:header_length(<<"Content-Length: 8388609">>)),
    ?assertError(_, z_development_lsp:header_length(<<"Content-Length: -1">>)).

diagnostics_test() ->
    ?assertEqual([], z_development_lsp_backend:diagnostics(<<"{% if a %}ok{% endif %}">>)),
    ?assertMatch([#{severity := 1}], z_development_lsp_backend:diagnostics(<<"{% if a %}">>)),
    ?assertMatch([#{severity := 1}], z_development_lsp_backend:diagnostics(<<"{# unclosed">>)),
    ?assertEqual([], z_development_lsp_backend:diagnostics(<<"{% trans \"Hello\" %}">>)),
    ?assertEqual([], z_development_lsp_backend:diagnostics(<<"\n{% extends \"base.tpl\" %}{% block body %}hi{% endblock %}">>)).

lifecycle_test() ->
    Remote = fun(_, _) -> [] end,
    S0 = z_development_lsp:new(),
    {[#{error := #{code := -32002}}], S0} = z_development_lsp:handle(request(1, <<"zotonic/sites">>, #{}), S0, Remote),
    {[#{result := #{capabilities := _}}], S1} = z_development_lsp:handle(request(2, <<"initialize">>, #{}), S0, Remote),
    {[#{error := #{code := -32601}}], S1} = z_development_lsp:handle(request(3, <<"unknown">>, #{}), S1, Remote),
    {[], S1} = z_development_lsp:handle(notify(<<"unknown">>, #{}), S1, Remote),
    {[#{result := null}], S2} = z_development_lsp:handle(request(4, <<"shutdown">>, #{}), S1, Remote),
    {[], {exit, 0}} = z_development_lsp:handle(notify(<<"exit">>, #{}), S2, Remote),
    {[], {exit, 1}} = z_development_lsp:handle(notify(<<"exit">>, #{}), S0, Remote).

buffers_test() ->
    Remote = fun(diagnostics, #{source := Source}) -> z_development_lsp_backend:diagnostics(Source) end,
    {_, S} = z_development_lsp:handle(request(1, <<"initialize">>, #{}), z_development_lsp:new(), Remote),
    Uri = <<"file:///tmp/unsaved.tpl">>,
    Doc = #{<<"uri">> => Uri, <<"version">> => 1, <<"text">> => <<"{% if a %}">>},
    {[#{params := #{diagnostics := [_], version := 1}}], S1} =
        z_development_lsp:handle(notify(<<"textDocument/didOpen">>, #{<<"textDocument">> => Doc}), S, Remote),
    Change = #{<<"textDocument">> => #{<<"uri">> => Uri, <<"version">> => 2},
               <<"contentChanges">> => [#{<<"text">> => <<"fixed">>}]},
    {[#{params := #{diagnostics := [], version := 2}}], S2} =
        z_development_lsp:handle(notify(<<"textDocument/didChange">>, Change), S1, Remote),
    {[], S2} = z_development_lsp:handle(notify(<<"textDocument/didChange">>, Change), S2, Remote),
    {[#{params := #{diagnostics := []}}], #{documents := Docs}} =
        z_development_lsp:handle(notify(<<"textDocument/didClose">>, #{<<"textDocument">> => Doc}), S2, Remote),
    ?assertEqual(#{}, Docs).

multiple_changes_test() ->
    Remote = fun(diagnostics, _) -> [] end,
    {_, S} = z_development_lsp:handle(request(1, <<"initialize">>, #{}), z_development_lsp:new(), Remote),
    Uri = <<"file:///tmp/multiple.tpl">>,
    Doc = #{<<"uri">> => Uri, <<"version">> => 1, <<"text">> => <<"old">>},
    {_, S1} = z_development_lsp:handle(notify(<<"textDocument/didOpen">>, #{<<"textDocument">> => Doc}), S, Remote),
    Change = #{<<"textDocument">> => Doc#{<<"version">> => 2},
        <<"contentChanges">> => [#{<<"text">> => <<"first">>}, #{<<"text">> => <<"last">>}]},
    {[#{params := #{version := 2}}], S2} = z_development_lsp:handle(notify(<<"textDocument/didChange">>, Change), S1, Remote),
    ?assertMatch(#{source := <<"last">>, version := 2}, maps:get(Uri, maps:get(documents, S2))),
    {[], S2} = z_development_lsp:handle(notify(<<"textDocument/didChange">>, Change), S2, Remote),
    Bad = Change#{<<"contentChanges">> => [#{<<"text">> => <<255>>}, #{<<"text">> => <<"last">>}]},
    {_, Rejected} = z_development_lsp:handle(notify(<<"textDocument/didChange">>, Bad), S1, Remote),
    ?assertEqual(#{}, maps:get(documents, Rejected)).

automatic_conflicting_sites_test() ->
    Modules = [z_sites_manager, z_module_manager, z_module_indexer, z_development_template],
    lists:foreach(fun(M) -> meck:new(M, [non_strict]) end, Modules),
    try
        meck:expect(z_sites_manager, get_sites, fun() -> #{lsp_site_a => running, lsp_site_b => running} end),
        meck:expect(z_module_manager, active, fun(_) -> [mod_development] end),
        meck:expect(z_module_indexer, all, fun(template, _) -> [] end),
        meck:expect(z_development_template, source_references, fun(_, Context) ->
            Site = atom_to_binary(z_context:site(Context)),
            [#{kind => template, name => <<"base.tpl">>, row => 1, column => 13,
                matches => [#{path => <<"/tmp/", Site/binary, "/base.tpl">>, module => zotonic_mod_base}]}]
        end),
        Args = #{source => <<"{% include \"base.tpl\" %}">>, file => <<"/tmp/unowned.tpl">>,
            position => #{<<"line">> => 0, <<"character">> => 15}},
        ?assertMatch({error, _}, z_development_lsp_backend:request(definition, Args)),
        ?assertMatch([#{uri := _}], z_development_lsp_backend:request(definition, Args#{site => <<"lsp_site_a">>})),
        meck:expect(z_development_template, source_references, fun(_, _) ->
            [#{kind => template, name => <<"base.tpl">>, row => 1, column => 13,
                matches => [#{path => <<"/tmp/shared/base.tpl">>, module => zotonic_mod_base}]}]
        end),
        ?assertMatch([#{uri := _}], z_development_lsp_backend:request(definition, Args)),
        meck:expect(z_module_indexer, all_files, fun(model, zotonic_core) ->
            [#module_index{key = #module_index_key{name = rsc}}];
            (template, zotonic_core) -> []
        end),
        meck:expect(z_module_indexer, all, fun(model, _) -> [];
            (template, _) -> [#module_index{key = #module_index_key{name = <<"cards/card.tpl">>}}]
        end),
        Base = Args#{site => <<"lsp_site_a">>},
        Result = z_development_lsp_backend:request(completion,
            Base#{source => <<"{{ m.">>, position => #{<<"line">> => 0, <<"character">> => 5}}),
        ?assertMatch([#{label := <<"rsc">>}], Result),
        [#{textEdit := Edit}] = z_development_lsp_backend:request(completion,
            Base#{source => <<"{% include \"cards/card.tpl\" %}">>,
                position => #{<<"line">> => 0, <<"character">> => 20}}),
        ?assertEqual(#{newText => <<"cards/card.tpl">>, range => #{
            start => #{line => 0, character => 12}, 'end' => #{line => 0, character => 26}}}, Edit)
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

indexed_ranges_test() ->
    Source = unicode:characters_to_binary("first\n😀é {{ a|escape }}\nlast"),
    Lines = z_development_lsp_text:lines(Source),
    ?assertEqual(#{start => #{line => 1, character => 9}, 'end' => #{line => 1, character => 15}},
        z_development_lsp_text:range(Lines, 2, 9, <<"escape">>)).

completion_ranges_test() ->
    Prefix = unicode:characters_to_binary("😀 {% include \"cards/ca"),
    Source = <<Prefix/binary, "rd.tpl\" %}">>,
    ?assertEqual(#{start => #{line => 0, character => 15}, 'end' => #{line => 0, character => 29}},
        z_development_lsp_text:completion_range(Source, Prefix, <<"cards/ca">>)),
    ?assertEqual(#{start => #{line => 0, character => 15}, 'end' => #{line => 0, character => 23}},
        z_development_lsp_text:completion_range(Prefix, Prefix, <<"cards/ca">>)).

invalid_input_test() ->
    Remote = fun(_, _) -> [] end,
    S0 = z_development_lsp:new(),
    {[#{error := #{code := -32700}, id := null}], S0} = z_development_lsp:handle(parse_error, S0, Remote),
    {_, S1} = z_development_lsp:handle(request(1, <<"initialize">>, #{}), S0, Remote),
    Uri = <<"file:///tmp/a.tpl">>,
    Doc = #{<<"uri">> => Uri, <<"version">> => 1, <<"text">> => <<"good">>},
    {_, S2} = z_development_lsp:handle(notify(<<"textDocument/didOpen">>, #{<<"textDocument">> => Doc}), S1, Remote),
    BadChange = #{<<"textDocument">> => #{<<"uri">> => Uri, <<"version">> => 2},
                  <<"contentChanges">> => [#{<<"text">> => <<255>>}]},
    {[#{params := #{diagnostics := []}}, #{method := <<"window/logMessage">>}], #{documents := Docs}} =
        z_development_lsp:handle(notify(<<"textDocument/didChange">>, BadChange), S2, Remote),
    ?assertEqual(#{}, Docs).

path_mapping_test() ->
    Mappings = z_development_lsp_paths:prepare([
        #{<<"localRoot">> => <<"/host/project/">>, <<"remoteRoot">> => <<"/container/project">>},
        #{<<"localRoot">> => <<"/host/project/apps_user">>, <<"remoteRoot">> => <<"/container/sites">>}
    ]),
    ?assertEqual(<<"/container/project/apps/a.tpl">>,
        z_development_lsp_paths:to_server(<<"/host/project/apps/a.tpl">>, Mappings)),
    ?assertEqual(<<"/container/sites/a.tpl">>,
        z_development_lsp_paths:to_server(<<"/host/project/apps_user/a.tpl">>, Mappings)),
    ?assertThrow({lsp_error, _}, z_development_lsp_paths:to_server(<<"/host/project-other/a.tpl">>, Mappings)),
    ?assertError(_, z_development_lsp_paths:to_server(<<"/host/project/../a.tpl">>, Mappings)),
    File = unicode:characters_to_binary("/container/sites/é space #%.tpl"),
    [#{uri := Uri}] = z_development_lsp_paths:definitions([#{uri => z_development_lsp_text:uri(File)}], Mappings),
    ?assertEqual(unicode:characters_to_binary("/host/project/apps_user/é space #%.tpl"), z_development_lsp_text:path(Uri)),
    ?assertThrow({lsp_error, _}, z_development_lsp_paths:definitions([#{uri => <<"file:///unmapped/a.tpl">>}], Mappings)).

mapping_validation_test() ->
    Pair = #{<<"localRoot">> => <<"/host">>, <<"remoteRoot">> => <<"/container">>},
    ?assertError(_, z_development_lsp_paths:prepare([Pair, Pair])),
    ?assertError(_, z_development_lsp_paths:prepare([#{}])),
    ?assertError(_, z_development_lsp_paths:prepare([Pair#{<<"remoteRoot">> => <<"relative">>}])),
    ?assertEqual([], z_development_lsp_paths:prepare([])),
    ?assertEqual(<<"/unchanged.tpl">>, z_development_lsp_paths:to_server(<<"/unchanged.tpl">>, [])),
    ?assertEqual([#{uri => <<"file:///unchanged.tpl">>}],
        z_development_lsp_paths:definitions([#{uri => <<"file:///unchanged.tpl">>}], [])).

mapped_session_test() ->
    Remote = fun
        (diagnostics, _) -> [];
        (definition, #{file := <<"/container/project/a.tpl">>}) ->
            [#{uri => <<"file:///container/project/base.tpl">>, range => #{}}]
    end,
    Options = #{<<"pathMappings">> => [#{<<"localRoot">> => <<"/host/project">>,
                                      <<"remoteRoot">> => <<"/container/project">>}]},
    {_, S1} = z_development_lsp:handle(request(1, <<"initialize">>, #{<<"initializationOptions">> => Options}),
                                      z_development_lsp:new(), Remote),
    Uri = <<"file:///host/project/a.tpl">>,
    Doc = #{<<"uri">> => Uri, <<"version">> => 1, <<"text">> => <<"source">>},
    {[#{params := #{uri := Uri}}], S2} =
        z_development_lsp:handle(notify(<<"textDocument/didOpen">>, #{<<"textDocument">> => Doc}), S1, Remote),
    Params = #{<<"textDocument">> => #{<<"uri">> => Uri},
               <<"position">> => #{<<"line">> => 0, <<"character">> => 0}},
    {[#{result := [#{uri := <<"file:///host/project/base.tpl">>}]}], _} =
        z_development_lsp:handle(request(2, <<"textDocument/definition">>, Params), S2, Remote).

automatic_mappings_test() ->
    Defaults = z_development_lsp_paths:prepare([
        #{<<"localRoot">> => <<"/host/project">>, <<"remoteRoot">> => <<"/container/project">>}]),
    Initial = (z_development_lsp:new())#{path_mappings => Defaults},
    Remote = fun(_, _) -> [] end,
    {_, State} = z_development_lsp:handle(request(1, <<"initialize">>,
        #{<<"initializationOptions">> => #{<<"pathMappings">> => []}}), Initial, Remote),
    ?assertEqual(Defaults, maps:get(path_mappings, State)),
    Explicit = [#{<<"localRoot">> => <<"/explicit/host">>, <<"remoteRoot">> => <<"/explicit/server">>}],
    {_, Other} = z_development_lsp:handle(request(1, <<"initialize">>,
        #{<<"initializationOptions">> => #{<<"pathMappings">> => Explicit}}), Initial, Remote),
    ?assertEqual(z_development_lsp_paths:prepare(Explicit), maps:get(path_mappings, Other)).

hover_format_test() ->
    lists:foreach(fun({Formats, Expected}) ->
        Params = #{<<"capabilities">> => #{<<"textDocument">> => #{<<"hover">> => #{<<"contentFormat">> => Formats}}}},
        {_, State} = z_development_lsp:handle(request(1, <<"initialize">>, Params),
            z_development_lsp:new(), fun(_, _) -> [] end),
        ?assertEqual(Expected, maps:get(hover_format, State))
    end, [{[], <<"plaintext">>}, {[<<"markdown">>, <<"plaintext">>], <<"markdown">>},
          {[<<"plaintext">>, <<"markdown">>], <<"plaintext">>}]),
    Ref = #{kind => filter, name => <<"escape`<script>">>},
    #{kind := <<"markdown">>, value := Markdown} = z_development_lsp_hover:format(
        Ref, [#{file => <<"/nonexistent/a`file.erl">>, line => 42, owner => {<<"Module">>, <<"mod_base">>}}], <<"site_[test]">>, <<"markdown">>),
    ?assertNotEqual(nomatch, binary:match(Markdown, <<"**Filter** `` escape`<script> ``">>)),
    ?assertNotEqual(nomatch, binary:match(Markdown, <<"Source implementation">>)),
    ?assertEqual(nomatch, binary:match(Markdown, <<"Module priority">>)),
    ?assertNotEqual(nomatch, binary:match(Markdown, <<"a`file.erl:42">>)).

acl_hover_tables_test() ->
    Text = z_development_lsp_tables:to_lists(z_development_lsp_docs:module(m_acl)),
    ?assertEqual(15, length(binary:matches(Text, <<"- Method: `get`; Path pattern:">>))),
    ?assertEqual(nomatch, binary:match(Text, <<"| `get` |">>)),
    %% An unmatched code delimiter must not consume the row's remaining columns.
    Broken = <<"| Method | Path | Description |\n| --- | --- | --- |\n",
        "| `get` | `/auth_service | Authentication |\n",
        "| `get` | `/user` | Current user |">>,
    Fixed = z_development_lsp_tables:to_lists(Broken),
    ?assertNotEqual(nomatch, binary:match(Fixed, <<"Path: `/auth_service; Description: Authentication">>)),
    ?assertNotEqual(nomatch, binary:match(Fixed, <<"Path: `/user`; Description: Current user">>)).

rsc_hover_tables_test() ->
    Doc = z_development_lsp_docs:module(m_rsc),
    Text = z_development_lsp_tables:to_lists(Doc),
    ?assertNotEqual(nomatch, binary:match(Text, <<"- Property: page_url; Description:">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"- Property: category_id; Description:">>)),
    ?assertEqual(nomatch, binary:match(Text, <<"| Property">>)),
    ?assertEqual(nomatch, binary:match(Text, <<"| page_url |">>)).

hover_tables_test() ->
    Table = <<"| Name | Description |\n| :--- | ---: |\n| `a` | A \\| B |\n| café | Unicode |"/utf8>>,
    Converted = z_development_lsp_tables:to_lists(Table),
    ?assertEqual(<<"\n- Name: `a`; Description: A \\| B\n- Name: café; Description: Unicode\n"/utf8>>, Converted),
    lists:foreach(fun(Fence) ->
        Example = <<Fence/binary, "markdown\n", Table/binary, "\n", Fence/binary>>,
        ?assertEqual(Example, z_development_lsp_tables:to_lists(Example))
    end, [<<"```">>, <<"~~~~">>]),
    ?assertEqual(<<"A | B\nordinary prose">>, z_development_lsp_tables:to_lists(<<"A | B\nordinary prose">>)),
    ?assertEqual(<<"\n- A: 1; B: 2\n">>, z_development_lsp_tables:to_lists(<<"A | B\n--- | ---\n1 | 2">>)),
    lists:foreach(fun({Client, Expected}) ->
        Remote = fun
            (diagnostics, _) -> [];
            (hover, _) -> #{contents => #{kind => <<"markdown">>, value => Table}}
        end,
        Params = #{<<"clientInfo">> => #{<<"name">> => Client}},
        {_, S1} = z_development_lsp:handle(request(1, <<"initialize">>, Params), z_development_lsp:new(), Remote),
        Uri = <<"file:///tmp/table.tpl">>,
        Doc = #{<<"uri">> => Uri, <<"version">> => 1, <<"text">> => <<"x">>},
        {_, S2} = z_development_lsp:handle(notify(<<"textDocument/didOpen">>, #{<<"textDocument">> => Doc}), S1, Remote),
        Hover = #{<<"textDocument">> => #{<<"uri">> => Uri},
            <<"position">> => #{<<"line">> => 0, <<"character">> => 0}},
        {[#{result := #{contents := #{value := Actual}}}], _} =
            z_development_lsp:handle(request(2, <<"textDocument/hover">>, Hover), S2, Remote),
        ?assertEqual(Expected, Actual)
    end, [{<<"Sublime Text LSP">>, Converted}, {<<"Visual Studio Code">>, Table}, {<<"Other">>, Table}]).

trans_tag_documentation_test() ->
    ?assertEqual(<<"trans">>, z_development_lsp_docs:main_tag(<<"trans">>)),
    ?assertEqual(z_development_lsp_docs:tag(<<"trans_ext">>),
        z_development_lsp_docs:tag(<<"trans">>)),
    ?assertNotEqual(nomatch, binary:match(z_development_lsp_docs:tag(<<"trans">>),
        <<"substitute variables">>)).

tag_alias_documentation_test() ->
    lists:foreach(fun({Alias, Main}) ->
        ?assertEqual(Main, z_development_lsp_docs:main_tag(Alias)),
        Doc = z_development_lsp_docs:tag(Main),
        ?assert(byte_size(Doc) > 0),
        ?assertEqual(Doc, z_development_lsp_docs:tag(Alias))
    end, [{<<"else">>, <<"if">>}, {<<"elif">>, <<"if">>},
          {<<"endif">>, <<"if">>}, {<<"endfor">>, <<"for">>},
          {<<"empty">>, <<"for">>}, {<<"endblock">>, <<"block">>},
          {<<"endwith">>, <<"with">>}, {<<"endifchanged">>, <<"ifchanged">>}]).

hover_documentation_test() ->
    ?assertEqual(<<"\nHello\n\nBody\n">>, z_development_lsp_docs:clean(
        <<"---\nkeywords: [reference]\n---\n\nHello\n::: note\nBody\n:::">>)),
    ?assertEqual(<<>>, z_development_lsp_docs:tag(<<"../../README">>)),
    Doc = z_development_lsp_docs:module(filter_escape),
    ?assertNotEqual(nomatch, binary:match(Doc, <<"HTML escape">>)),
    lists:foreach(fun(Format) ->
        #{value := Text} = z_development_lsp_hover:format(
            #{kind => filter, name => <<"escape">>, erlang_module => filter_escape}, [], [], Format),
        ?assertNotEqual(nomatch, binary:match(Text, <<"HTML escape">>)),
        #{value := Tag} = z_development_lsp_hover:format(
            #{kind => tag, name => <<"include">>, documentation => <<"Include another template.">>}, [], [], Format),
        ?assertNotEqual(nomatch, binary:match(Tag, <<"Include another template.">>)),
        ?assertEqual(nomatch, binary:match(Tag, <<"No matching source">>))
    end, [<<"markdown">>, <<"plaintext">>]).

hover_scope_test() ->
    Ref = #{kind => template, name => <<"base.tpl">>},
    Targets = [#{file => <<"/nonexistent/base.tpl">>, line => 1,
        owner => {<<"Site">>, <<"owner_site">>}}],
    lists:foreach(fun(Format) ->
        #{value := Small} = z_development_lsp_hover:format(
            Ref, Targets, [<<"one">>, <<"two">>, <<"three">>], Format),
        #{value := Large} = z_development_lsp_hover:format(
            Ref, Targets, [<<"one">>, <<"two">>, <<"three">>, <<"four">>], Format),
        ?assertNotEqual(nomatch, binary:match(Small, <<"Matching sites">>)),
        ?assertEqual(nomatch, binary:match(Large, <<"Matching sites">>)),
        ?assertNotEqual(nomatch, binary:match(Large, <<"Site: ">>)),
        ?assertNotEqual(nomatch, binary:match(Large, <<"owner_site">>))
    end, [<<"plaintext">>, <<"markdown">>]).

hover_relative_path_test() ->
    Root = unicode:characters_to_binary(filename:absname(z_path:get_path())),
    Ref = #{kind => model, name => <<"rsc">>},
    File = filename:join(Root, <<"apps/zotonic_core/src/models/m_rsc.erl">>),
    Targets = [#{file => File, line => 337, owner => {<<"Module">>, <<"zotonic_core">>}}],
    lists:foreach(fun(Format) ->
        #{value := Text} = z_development_lsp_hover:format(Ref, Targets, [], Format),
        ?assertEqual(nomatch, binary:match(Text, Root)),
        ?assertNotEqual(nomatch, binary:match(Text, <<"apps/zotonic_core/src/models/m_rsc.erl:337">>))
    end, [<<"markdown">>, <<"plaintext">>]),
    Outside = <<Root/binary, "-other/file.erl">>,
    ?assertEqual(Outside, filter_development_filename:development_filename(Outside)).

hover_symlink_test() ->
    Base = filename:join(<<"/tmp">>, <<"zotonic-hover-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>),
    Source = filename:join(Base, <<"source">>),
    Build = filename:join(Base, <<"_build">>),
    File = filename:join(Source, <<"space template.tpl">>),
    ok = file:make_dir(Base),
    ok = file:make_dir(Source),
    ok = file:write_file(File, <<>>),
    ok = file:make_symlink(Source, Build),
    try
        Ref = #{kind => template, name => <<"base.tpl">>},
        #{value := Text} = z_development_lsp_hover:format(Ref,
            [#{file => filename:join(Build, <<"space template.tpl">>), line => 1, owner => {<<"Site">>, <<"test">>}}], <<"test">>, <<"plaintext">>),
        ?assertEqual(nomatch, binary:match(Text, <<"/_build/">>)),
        ?assertNotEqual(nomatch, binary:match(Text, filter_development_filename:resolve(File))),
        ?assertNotEqual(nomatch, binary:match(Text, <<"Effective:">>))
    after
        file:delete(Build), file:delete(File), file:del_dir(Source), file:del_dir(Base)
    end.

request(Id, Method, Params) -> (notify(Method, Params))#{<<"id">> => Id}.
notify(Method, Params) -> #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => Method, <<"params">> => Params}.

%% @doc Verify runtime priority and canonical source paths using a running development site.
site_tests(Context) ->
    Site = atom_to_binary(z_context:site(Context)),
    All = z_module_indexer:all(template, Context),
    Names = lists:usort([N || #module_index{key = #module_index_key{name = N}} <- All]),
    [
        {"tag manuals hover without a selected site", fun() ->
            Args = #{source => <<"{% include \"base.tpl\" %}">>, file => <<"/unowned/new.tpl">>,
                position => #{<<"line">> => 0, <<"character">> => 5}},
            #{contents := #{value := Text}} = z_development_lsp_backend:request(hover, Args),
            ?assertNotEqual(nomatch, binary:match(Text, <<"Include another template">>)),
            ?assertEqual(nomatch, binary:match(Text, <<"keywords:">>))
        end},
        {"automatic navigation resolves shared server references", fun() ->
            #module_index{filepath = Shared} = hd(z_module_indexer:find_all(template, <<"admin_development.tpl">>, Context)),
            lists:foreach(fun({Source, Column, ExpectedFile}) ->
                Args = #{source => Source, file => Shared,
                         position => #{<<"line">> => 0, <<"character">> => Column}},
                Explicit = z_development_lsp_backend:request(definition, Args#{site => Site}),
                ?assertMatch([#{uri := _} | _], Explicit),
                ?assertEqual(Explicit, z_development_lsp_backend:request(definition, Args)),
                ?assertNotEqual(nomatch, binary:match(maps:get(uri, hd(Explicit)), ExpectedFile)),
                #{contents := #{value := Hover}} = z_development_lsp_backend:request(hover, Args),
                ?assertNotEqual(nomatch, binary:match(Hover, <<"Module:">>))
            end, [
                {<<"{{ value|escape }}">>, 11, <<"filter_escape.erl">>},
                {<<"{{ m.rsc[1].title }}">>, 6, <<"m_rsc.erl">>},
                {<<"{% wire delegate=\"mod_development\" %}">>, 22, <<"mod_development.erl">>},
                {<<"{% include \"admin_development.tpl\" %}">>, 15, <<"admin_development.tpl">>}
            ])
        end},
        {"unowned buffers can resolve unique references on running sites", fun() ->
            Args = #{source => <<"{{ value|escape }}">>, file => <<"/unowned/new.tpl">>,
                     position => #{<<"line">> => 0, <<"character">> => 11}},
            ?assertMatch([#{uri := _}], z_development_lsp_backend:request(definition, Args))
        end},
        {"overrules finds the next priority from the real source path", fun() ->
            [Matches | _] = [Ms || N <- Names,
                Ms <- [z_module_indexer:find_all(template, N, Context)], length(Ms) >= 2],
            [#module_index{filepath = First}, #module_index{filepath = Next} | _] = Matches,
            RealPath = filter_development_filename:resolve(First),
            Source = <<"{% overrules %}">>,
            Result = z_development_lsp_backend:request(definition,
                #{site => Site, source => Source, file => RealPath,
                  position => #{<<"line">> => 0, <<"character">> => 6}}),
            ?assertMatch([#{uri := _}], Result),
            ?assertEqual(z_development_lsp_text:uri(Next), maps:get(uri, hd(Result)))
        end},
        {"category templates expose candidates and explain resource dependence", fun() ->
            #module_index{filepath = File} = hd(All),
            Source = <<"{% catinclude \"base.tpl\" id %}">>,
            Args = #{site => Site, source => Source, file => File,
                     position => #{<<"line">> => 0, <<"character">> => 18}},
            Matches = z_development_template:matching_templates({cat, <<"base.tpl">>}, Context),
            Targets = z_development_lsp_backend:request(definition, Args),
            ?assertEqual(length(Matches), length(Targets)),
            #{contents := #{value := Hover}} = z_development_lsp_backend:request(hover, Args),
            ?assertNotEqual(nomatch, binary:match(Hover, <<"resource">>))
        end},
        {"shared source references contain no browser authorization tokens", fun() ->
            Refs = z_development_template:source_references(<<"{{ value|escape }}">>, Context),
            ?assertMatch([#{kind := filter}], Refs),
            ?assertNot(maps:is_key(token, hd(Refs)))
        end}
    ].
