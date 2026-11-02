-module(i18n_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

gettext_test() ->
    X = z_gettext:parse_po_bin(<<"msgid \"\"
msgstr \"header value\"">>),
    [{header, <<"header value">>}] = X,

    X2 = z_gettext:parse_po_bin(<<"msgid \"\"
msgstr \"header value\"

msgid \"en\"
msgstr \"nl\"
">>),
    [{header, <<"header value">>}, {<<"en">>, <<"nl">>}] = X2,

    X3 = z_gettext:parse_po_bin(<<"msgid \"\"
msgstr \"header value\"

msgid \"en\"
msgstr \"nl\"

msgid \"empty trans\"
msgstr \"\"
">>),
    [{header, <<"header value">>}, {<<"en">>, <<"nl">>}] = X3,

    X4 = z_gettext:parse_po_bin(<<"msgid \"\"
msgstr \"header value\"

#~ msgid \"en\"
#~ msgstr \"nl\"

msgid \"en\"
msgstr \"de\"
">>),
    [{header, <<"header value">>}, {<<"en">>, <<"de">>}] = X4,
    ok.

plural_po_test() ->
    [{header, _}, {{plural, <<"hour">>, <<"hours">>}, {Rule, Forms}},
        {<<"Save">>, <<"Bewaar">>}] = z_gettext:parse_po_bin(plural_po()),
    ?assertEqual({<<"hodina">>, <<"hodiny">>, <<"hodin">>}, Forms),
    ?assertEqual({ok, 2}, template_compiler_plural:index(Rule, 5)),
    ?assertEqual({ok, 1}, template_compiler_plural:index(Rule, 3)),
    Sparse = <<"msgid \"hour\"\nmsgid_plural \"hours\"\nmsgstr[2] \"many\"\n">>,
    [{{plural, _, _}, {undefined, {undefined, undefined, <<"many">>}}}] = z_gettext:parse_po_bin(Sparse),
    Empty = <<"msgid \"hour\"\nmsgid_plural \"hours\"\nmsgstr[0] \"\"\nmsgstr[1] \"\"\n">>,
    ?assertEqual([], z_gettext:parse_po_bin(Empty)),
    ?assertMatch([{{plural, _, _}, {undefined, {<<>>, <<>>}}}], z_gettext:parse_po_bin(Empty, false)).

plural_pot_roundtrip_test() ->
    File = filename:join(os:getenv("TMPDIR", "/tmp"),
        "zotonic_plural_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".pot"),
    Singular = <<"{n} \"hour\"\nago">>,
    Plural = <<"{n} \"hours\"\nago">>,
    try
        ok = z_gettext_compile:generate(File, [{{plural, Singular, Plural}, <<>>, undefined},
            {<<"Save">>, <<>>, undefined}]),
        [{header, Header}, {{plural, Singular, Plural}, {undefined, {<<>>, <<>>}}},
            {<<"Save">>, <<>>}] = z_gettext:parse_pot(File),
        ?assertNotEqual(nomatch, binary:match(Header, <<"Plural-Forms:">>))
    after
        file:delete(File)
    end.

plural_lookup_test() ->
    [{header, _}, {Key, Translation}, _] = z_gettext:parse_po_bin(plural_po()),
    Table = ets:new(plural_lookup_test, [set]),
    Context = #context{translation_table=Table, language=[cs, en]},
    try
        ets:insert(Table, {Key, [{cs, Translation}]}),
        ets:insert(Table, {<<"hour">>, [{cs, <<"singular lookup">>}]}),
        ?assertEqual(<<"hodina">>, z_trans:trans_plural(<<"hour">>, <<"hours">>, 1, Context)),
        ?assertEqual(<<"hodina">>, z_trans:trans_plural("hour", "hours", 1, Context)),
        ?assertEqual(<<"hodiny">>, z_trans:trans_plural(<<"hour">>, <<"hours">>, 3, Context)),
        ?assertEqual(<<"hodin">>, z_trans:trans_plural(<<"hour">>, <<"hours">>, 5, Context)),
        ?assertEqual(<<"singular lookup">>, z_trans:trans(<<"hour">>, Context)),
        FallbackContext = Context#context{language=[ar, cs, en]},
        ?assertEqual(<<"hodiny">>, z_trans:trans_plural(<<"hour">>, <<"hours">>, 3, FallbackContext)),
        EnglishContext = Context#context{language=[ar, en]},
        ?assertEqual(<<"hour">>, z_trans:trans_plural(<<"hour">>, <<"hours">>, 1, EnglishContext)),
        ?assertEqual(<<"hours">>, z_trans:trans_plural(<<"hour">>, <<"hours">>, 100, EnglishContext)),
        {Rule, _} = Translation,
        ets:insert(Table, {Key, [{cs, {Rule, {<<"hodina">>, <<>>, undefined}}}]}),
        ?assertEqual(<<"hours">>, z_trans:trans_plural(<<"hour">>, <<"hours">>, 5, Context))
    after
        ets:delete(Table)
    end.

plural_catalog_render_test() ->
    {ok, _} = application:ensure_all_started(template_compiler),
    File = filename:join(os:getenv("TMPDIR", "/tmp"),
        "zotonic_plural_catalog_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".po"),
    Catalog = <<(plural_po())/binary,
        "\nmsgctxt \"past\"\nmsgid \"hour\"\nmsgid_plural \"hours\"\n"
        "msgstr[0] \"hodinou\"\nmsgstr[1] \"hodinami\"\nmsgstr[2] \"hodinami\"\n"
        "\nmsgctxt \"future\"\nmsgid \"hour\"\nmsgid_plural \"hours\"\n"
        "msgstr[0] \"hodinu\"\nmsgstr[1] \"hodiny\"\nmsgstr[2] \"hodin\"\n">>,
    ok = file:write_file(File, Catalog),
    ok = meck:new(z_module_indexer, [no_link]),
    ok = meck:new(z_notifier, [no_link]),
    ok = meck:new(z_template, [no_link]),
    ok = meck:expect(z_module_indexer, translations, fun(_) -> [{test_module, [{cs, File}]}] end),
    ok = meck:expect(z_notifier, observe, fun(_, _, _) -> ok end),
    ok = meck:expect(z_notifier, detach, fun(_, _) -> ok end),
    ok = meck:expect(z_notifier, notify, fun(_, _) -> ok end),
    ok = meck:expect(z_template, reset, fun(_) -> ok end),
    {ok, Server} = z_trans_server:start_link(plural_catalog_test),
    Context = #context{site=plural_catalog_test, language=[cs, en],
        translation_table=z_trans_server:table(plural_catalog_test)},
    try
        ok = z_trans_server:load_translations(Context),
        _ = sys:get_state(Server),
        ?assertEqual(<<"hodinami">>, z_trans:trans_plural("hour", "hours", 3, "past", Context)),
        ?assertEqual(<<"hodiny">>, z_trans:trans_plural("hour", "hours", 3, "future", Context)),
        ?assertEqual(<<"hours">>, z_trans:trans_plural("hour", "hours", 3, "missing context", Context)),
        ?assertEqual(<<"hours">>, z_trans:trans_plural("hour", "hours", 3, "past",
            Context#context{language=[en]})),
        % The server must not synthesize a contextual English entry. It would
        % mask a genuine contextless English translation during template fallback.
        [{_, RawContextual}] = ets:lookup(Context#context.translation_table,
            {context, <<"past">>, {plural, <<"hour">>, <<"hours">>}}),
        ?assertEqual(false, proplists:is_defined(en, RawContextual)),
        Template = <<"{% trans \"hour\" \"hours\" n=hours %}">>,
        Options = [{runtime, z_template_compiler_runtime}],
        {ok, Mod} = template_compiler:compile_binary(Template, File ++ ".tpl", Options, Context),
        ?assertEqual(<<"hodin">>, iolist_to_binary(Mod:render(#{hours => 5}, #{}, Context))),
        English = Context#context{language=[ar, en]},
        ?assertEqual(<<"hours">>, iolist_to_binary(Mod:render(#{hours => 100}, #{}, English))),
        % Changing only the rule must invalidate the compiled translation data.
        ok = file:write_file(File, binary:replace(Catalog, <<"n<=4">>, <<"n<=5">>)),
        ok = z_trans_server:load_translations(Context),
        _ = sys:get_state(Server),
        {ok, Mod} = template_compiler:compile_binary(Template, File ++ ".tpl", Options, Context),
        ?assertEqual(<<"hodiny">>, iolist_to_binary(Mod:render(#{hours => 5}, #{}, Context))),
        ?assertEqual(2, meck:num_calls(z_template, reset, [plural_catalog_test])),
        % An invalid header must not prevent loading the singular translations.
        ok = file:write_file(File, binary:replace(Catalog, <<"n<=4">>, <<"invalid(n)">>)),
        ok = z_trans_server:load_translations(Context),
        _ = sys:get_state(Server),
        ?assertEqual(<<"hours">>, z_trans:trans_plural("hour", "hours", 5, Context)),
        ?assertEqual(<<"Bewaar">>, z_trans:trans(<<"Save">>, Context))
    after
        gen_server:stop(Server),
        meck:unload([z_module_indexer, z_notifier, z_template]),
        file:delete(File)
    end.

context_po_test() ->
    Entries = z_gettext:parse_po_bin(<<"msgctxt \"button\"\nmsgid \"Open\"\nmsgstr \"Open action\"\n"
        "msgctxt \"status\"\nmsgid \"Open\"\nmsgstr \"Open state\"\n"
        "msgid \"Open\"\nmsgstr \"No context\"\n"
        "msgctxt \"\"\nmsgid \"Open\"\nmsgstr \"Empty context\"\n">>),
    ?assertEqual([
        {{context, <<"button">>, <<"Open">>}, <<"Open action">>},
        {{context, <<"status">>, <<"Open">>}, <<"Open state">>},
        {<<"Open">>, <<"No context">>},
        {{context, <<>>, <<"Open">>}, <<"Empty context">>}
    ], Entries).

context_pot_roundtrip_test() ->
    File = filename:join(os:getenv("TMPDIR", "/tmp"),
        "zotonic_context_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".pot"),
    Past = {context, <<"relative time: past">>, {plural, <<"{n} hour">>, <<"{n} hours">>}},
    Future = {context, <<"relative time: future">>, {plural, <<"{n} hour">>, <<"{n} hours">>}},
    try
        ok = z_gettext_compile:generate(File, [{Past, <<>>, undefined}, {Future, <<>>, undefined}]),
        [{header, Header}, {Past, _}, {Future, _}] = z_gettext:parse_pot(File),
        ?assertNotEqual(nomatch, binary:match(Header, <<"Plural-Forms:">>))
    after file:delete(File)
    end.

plural_po() ->
    <<"msgid \"\"\nmsgstr \"\"\n"
      "\"Plural-Forms: nplurals=3; plural=n==1 ? 0 : n>=2 && n<=4 ? 1 : 2;\\n\"\n"
      "\nmsgid \"hour\"\nmsgid_plural \"hours\"\n"
      "msgstr[0] \"hodina\"\nmsgstr[1] \"hodiny\"\nmsgstr[2] \"hodin\"\n"
      "\nmsgid \"Save\"\nmsgstr \"Bewaar\"\n">>.
