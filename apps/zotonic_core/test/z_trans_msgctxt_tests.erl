-module(z_trans_msgctxt_tests).
-mod_msgctxt("action").
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

msgctxt_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(C) -> [
        ?_test(strict_api(C)),
        ?_test(template_languages(C)),
        ?_test(template_scopes(C)),
        ?_test(template_plurals(C)),
        ?_test(module_default(C))
    ] end}.

setup() ->
    {ok, _} = application:ensure_all_started(template_compiler),
    ok = meck:new(z_notifier, [no_link]),
    ok = meck:expect(z_notifier, notify, fun(_, _) -> ok end),
    Table = ets:new(msgctxt_test, [set, public]),
    Rule = template_compiler_plural:english(),
    Three = {3, {conditional, {'==', n, 1}, 0,
        {conditional, {'<=', n, 4}, 1, 2}}},
    ets:insert(Table, [
        {<<"Open">>, [{en, <<"General English">>}, {nl, <<"Algemeen">>}]},
        {{context, <<"action">>, <<"Open">>}, [{nl, <<"Actie">>}]},
        {{context, <<"status">>, <<"Open">>}, [{nl, <<"Status">>}]},
        {{context, <<>>, <<"Open">>}, [{nl, <<"Empty context">>}]},
        {<<"Next">>, [{nl, <<"Volgende">>}]},
        {{context, <<"action">>, <<"Next">>}, [{de, <<"Weiter">>}]},
        {{plural, <<"{n} item">>, <<"{n} items">>},
            [{nl, {Rule, {<<"{n} algemeen">>, <<"{n} algemene">>}}}]},
        {{context, <<"action">>, {plural, <<"{n} item">>, <<"{n} items">>}},
            [{nl, {Three, {<<"{n} context">>, <<>>, <<"{n} context-many">>}}}]},
        {{context, <<"action">>, {plural, <<"{n} gap">>, <<"{n} gaps">>}},
            [{nl, {Rule, {<<"{n} nl">>, undefined}}},
             {de, {Rule, {<<"{n} de">>, <<"{n} de-many">>}}}]}
    ]),
    #context{site=msgctxt_tests, translation_table=Table, language=[nl, de, en]}.

cleanup(#context{translation_table=Table}) ->
    ets:delete(Table),
    meck:unload(z_notifier).

strict_api(C) ->
    ?assertEqual(<<"Actie">>, z_trans:trans_msgctxt(<<"Open">>, <<"action">>, C)),
    ?assertEqual(<<"Status">>, ?__(<<"Open">>, <<"status">>, C)),
    ?assertEqual(<<"Empty context">>, z_trans:trans_msgctxt("Open", "", C)),
    ?assertEqual(<<"Open">>, z_trans:trans_msgctxt("Open", "missing", C)),
    ?assertEqual(<<"Open">>, z_trans:trans_msgctxt("Open", "action", en, C)),
    ?assertEqual(<<"Actie">>, z_trans:trans_msgctxt("Open", "action", [fr, nl], C)),
    ?assertEqual(<<"Algemeen">>, z_trans:trans(<<"Open">>, C)),
    Resolved = ?___(<<"Open">>, <<"action">>, C),
    ?assertEqual(<<"Actie">>, z_trans:lookup(Resolved, nl, C)),
    ?assertEqual(undefined, z_trans:lookup(Resolved, fr, C)),
    ?assertEqual(<<"Actie">>, z_trans:lookup_fallback(Resolved, C)),
    ?assertEqual(<<"{n} items">>, z_trans:trans_plural("{n} item", "{n} items", 3, "action", C)),
    % Resolve missing forms before language fallback, just like templates.
    ?assertEqual(<<"{n} de-many">>, z_trans:trans_plural("{n} gap", "{n} gaps", 3, "action", C)),
    ?assertEqual(<<"{n} algemene">>, z_trans:trans_plural("{n} item", "{n} items", 3, C)).

template_languages(C) ->
    Source = <<"{% trans \"Next\" msgctxt=\"action\" %}">>,
    % Same-language contextless text wins over a contextual fallback language.
    ?assertEqual(<<"Volgende">>, render(Source, #{}, C)),
    ?assertEqual(<<"Weiter">>, render(Source, #{}, C#context{language=[de, nl, en]})),
    ?assertEqual(<<"General English">>, render(<<"{% trans \"Open\" msgctxt=\"action\" %}">>,
        #{}, C#context{language=[fr, en]})),
    ?assertEqual(<<"Unknown">>, render(<<"{% trans \"Unknown\" msgctxt=\"action\" %}">>, #{}, C)).

template_scopes(C) ->
    Source = <<"{% msgctxt \"action\" %}{_ Open _}|{{ _\"Open\" }}|"
        "{% with_msgctxt \"status\" %}{_ Open _}{% endwith_msgctxt %}|"
        "{% trans \"Open\" msgctxt=\"missing\" %}|{_ Open _}">>,
    ?assertEqual(<<"Actie|Actie|Status|Algemeen|Actie">>, render(Source, #{}, C)),
    ?assertEqual(<<"Empty context">>, render(<<"{% msgctxt \"\" %}{_ Open _}">>, #{}, C)).

template_plurals(C) ->
    Source = <<"{% msgctxt \"action\" %}{% trans \"{n} item\" \"{n} items\" n=amount %}">>,
    ?assertEqual(<<"1 context">>, render(Source, #{amount => 1}, C)),
    ?assertEqual(<<"3 algemene">>, render(Source, #{amount => 3}, C)),
    ?assertEqual(<<"5 context-many">>, render(Source, #{amount => 5}, C)),
    % No form in Dutch: continue to German instead of jumping to English.
    ?assertEqual(<<"3 de-many">>, render(
        <<"{% trans \"{n} gap\" \"{n} gaps\" n=amount msgctxt=\"action\" %}">>, #{amount => 3}, C)),
    ?assertEqual(<<"3 items">>, render(Source, #{amount => 3}, C#context{language=[fr, en]})).

module_default(C) ->
    ?assertEqual(<<"action">>, z_module_manager:mod_msgctxt(?MODULE)),
    ?assertEqual(undefined, z_module_manager:mod_msgctxt(z_trans)),
    File = "/tmp/msgctxt_owner/priv/templates/default.tpl",
    Other = "/tmp/msgctxt_override/priv/templates/default.tpl",
    Key = #module_index_key{site=msgctxt_tests, type=template, name= <<"default.tpl">>},
    ok = meck:new(z_module_indexer, [no_link]),
    ok = meck:expect(z_module_indexer, find_all, fun(template, <<"default.tpl">>, _) ->
        [#module_index{key=Key, filepath=Other, module=z_trans},
         #module_index{key=Key, filepath=File, module=?MODULE}]
    end),
    try
        ?assertEqual(undefined, z_template_compiler_runtime:get_template_msgctxt(Other, C)),
        ?assertEqual(<<"action">>, z_template_compiler_runtime:get_template_msgctxt(File, C)),
        lists:foreach(fun({Source, Expected}) ->
            {ok, Mod} = template_compiler:compile_binary(Source, File,
                [{runtime, z_template_compiler_runtime}], C),
            ?assertEqual(Expected, iolist_to_binary(Mod:render(#{}, #{}, C)))
        end, [
            {<<"{_ Open _}|{{ _\"Open\" }}|{% trans \"Open\" msgctxt=\"status\" %}">>, <<"Actie|Actie|Status">>},
            {<<"{% msgctxt \"status\" %}{_ Open _}">>, <<"Status">>},
            {<<"{% msgctxt \"\" %}{_ Open _}">>, <<"Empty context">>}
        ])
    after
        meck:unload(z_module_indexer)
    end.

render(Source, Vars, C) ->
    Name = "msgctxt_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".tpl",
    File = filename:join(os:getenv("TMPDIR", "/tmp"), Name),
    {ok, Mod} = template_compiler:compile_binary(Source, File, [{runtime, z_template_compiler_runtime}], C),
    iolist_to_binary(Mod:render(Vars, #{}, C)).
