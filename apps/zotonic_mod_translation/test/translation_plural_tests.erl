-module(translation_plural_tests).
-mod_msgctxt(<<"module default">>).
-include_lib("eunit/include/eunit.hrl").

consolidated_header_test() ->
    with_directory(fun(Dir) ->
        Singular = filename:join(Dir, "singular.pot"),
        Plural = filename:join(Dir, "plural.pot"),
        Key = {context, <<"duration">>, {plural, <<"{n} hour">>, <<"{n} hours">>}},
        ok = z_gettext_compile:generate(Singular, [{<<"Save">>, <<>>, undefined}]),
        ok = z_gettext_compile:generate(Plural, [{Key, <<>>, undefined}]),
        Header = proplists:get_value(header, z_gettext:parse_pot(Singular)),
        ?assertEqual(Header, proplists:get_value(header, z_gettext:parse_pot(Plural))),
        case os:find_executable("msgcat") of
            false -> ok;
            Msgcat ->
                Merged = filename:join(Dir, "merged.pot"),
                Command = lists:flatten(lists:join(" ", [z_filelib:os_filename(F)
                    || F <- [Msgcat, "-o", Merged, Singular, Plural]])),
                ?assertEqual([], os:cmd(Command)),
                Entries = z_gettext:parse_pot(Merged),
                ?assertEqual(Header, proplists:get_value(header, Entries)),
                ?assertEqual(3, length(Entries)),
                ?assert(lists:keymember(Key, 1, Entries)),
                ?assert(lists:keymember(<<"Save">>, 1, Entries))
        end
    end).

module_default_extraction_test() ->
    ?assertEqual(<<"module default">>, z_module_manager:mod_msgctxt(?MODULE)),
    with_directory(fun(Dir) ->
        File = filename:join([Dir, "priv", "templates", "default.tpl"]),
        ok = filelib:ensure_dir(File),
        ok = file:write_file(File, <<"{_ Open _}{% with_msgctxt \"scope\" %}{_ Close _}{% endwith_msgctxt %}">>),
        [{?MODULE, Dir, Labels}] = translation_scan:scan([{?MODULE, Dir}]),
        ?assertMatch({_, [], _}, lists:keyfind({context, <<"module default">>, <<"Open">>}, 1, Labels)),
        ?assertMatch({_, [], _}, lists:keyfind({context, <<"scope">>, <<"Close">>}, 1, Labels)),
        ok = file:write_file(File, <<"{% msgctxt \"\" %}{_ Open _}">>),
        [{?MODULE, Dir, Explicit}] = translation_scan:scan([{?MODULE, Dir}]),
        ?assertMatch([{{context, <<>>, <<"Open">>}, [], _}], Explicit)
    end).

plural_extraction_test() ->
    with_directory(fun(Dir) ->
        Template = filename:join([Dir, "priv", "templates", "plural.tpl"]),
        ok = filelib:ensure_dir(Template),
        ok = file:write_file(Template,
            <<"{% trans \"{n} hour\" \"{n} hours\" n=count %}\n"
              "{% trans \"{n} hour\" \"{n} hours\" n=other_count %}\n{_ Save _}">>),
        [{plural_test, Dir, Labels}] = translation_scan:scan([{plural_test, Dir}]),
        ?assertEqual(2, length(Labels)),
        {_, [], Positions} = lists:keyfind({plural, <<"{n} hour">>, <<"{n} hours">>}, 1, Labels),
        ?assertEqual(2, length(Positions)),
        Pot = filename:join(Dir, "test.pot"),
        ok = z_gettext_compile:generate(Pot, [{Key, <<>>, Pos} || {Key, _, Pos} <- Labels]),
        Parsed = z_gettext:parse_pot(Pot),
        ?assertMatch({_, {undefined, {<<>>, <<>>}}},
            lists:keyfind({plural, <<"{n} hour">>, <<"{n} hours">>}, 1, Parsed)),
        ok = file:write_file(Template,
            <<"{% trans \"one\" \"many\" n=n %}{% trans \"one\" \"others\" n=n %}">>),
        ?assertException(error, {conflicting_translation_sources, _, _, _},
            translation_scan:scan([{plural_test, Dir}]))
    end).

erlang_extraction_test() ->
    with_directory(fun(Dir) ->
        File = filename:join(Dir, "plural_source.erl"),
        ok = file:write_file(File, <<"-module(plural_source).\n"
            "f(N, C) -> z_trans:trans_plural(<<\"one\">>, <<\"many\">>, N, C),\n"
            "z_trans:trans_plural(\"single\", \"multiple\", N, C).\n">>),
        {ok, Epp} = epp:open(File, []),
        Labels = translation_scan:parse_erl(File, Epp),
        ?assertEqual(2, length(Labels)),
        ?assertMatch({_, [], {File, 2}}, lists:keyfind({plural, <<"one">>, <<"many">>}, 1, Labels)),
        ?assertMatch({_, [], {File, 3}}, lists:keyfind({plural, <<"single">>, <<"multiple">>}, 1, Labels))
    end).

context_extraction_test() ->
    with_directory(fun(Dir) ->
        File = filename:join(Dir, "context_source.erl"),
        ok = file:write_file(File, <<"-module(context_source).\n"
            "f(N, C) -> z_trans:trans_plural(<<\"one\">>, <<\"many\">>, N, <<\"past\">>, C),\n"
            "z_trans:plural_translations(\"one\", \"many\", \"future\", C).\n">>),
        {ok, Epp} = epp:open(File, []),
        Labels = translation_scan:parse_erl(File, Epp),
        ?assertEqual(2, length(Labels)),
        ?assertMatch({_, [], {File, 2}}, lists:keyfind({context, <<"past">>, {plural, <<"one">>, <<"many">>}}, 1, Labels)),
        ?assertMatch({_, [], {File, 3}}, lists:keyfind({context, <<"future">>, {plural, <<"one">>, <<"many">>}}, 1, Labels))
    end).

relative_time_extraction_test() ->
    Core = code:lib_dir(zotonic_core),
    File = filename:join([Core, "src", "support", "z_datetime.erl"]),
    {ok, Epp} = epp:open(File, [filename:join(Core, "include")]),
    Labels = translation_scan:parse_erl(File, Epp),
    Contexts = [<<"relative time: past">>, <<"relative time: future">>, <<"relative time: duration">>],
    lists:foreach(fun(C) ->
        Pairs = [Pair || {{context, Context, Pair}, _, _} <- Labels, C =:= Context],
        ?assertEqual(6, length(Pairs)),
        ?assert(lists:member({plural, <<"{n} hour">>, <<"{n} hours">>}, Pairs))
    end, Contexts),
    ?assertMatch({_, _, _}, lists:keyfind(<<"{duration} ago">>, 1, Labels)),
    ?assertMatch({_, _, _}, lists:keyfind(<<"in {duration}">>, 1, Labels)).

singular_msgctxt_extraction_test() ->
    with_directory(fun(Dir) ->
        File = filename:join(Dir, "singular_msgctxt.erl"),
        ok = file:write_file(File, <<"-module(singular_msgctxt).\n"
            "-include_lib(\"zotonic_core/include/zotonic.hrl\").\n"
            "f(C) -> z_trans:trans_msgctxt(<<\"Open\">>, <<\"action\">>, C),\n"
            "z_trans:trans_msgctxt(\"Open\", \"status\", nl, C),\n"
            "z_trans:translations(\"Open\", \"all\", C),\n"
            "?__(<<\"Next\">>, <<\"button\">>, C),\n"
            "?___(<<\"Next\">>, <<\"pagination\">>, C).\n">>),
        {ok, Epp} = epp:open(File, []),
        Labels = translation_scan:parse_erl(File, Epp),
        Keys = [Key || {Key, _, _} <- Labels],
        ?assertEqual(lists:sort([
            {context, <<"action">>, <<"Open">>}, {context, <<"status">>, <<"Open">>},
            {context, <<"all">>, <<"Open">>}, {context, <<"button">>, <<"Next">>},
            {context, <<"pagination">>, <<"Next">>}
        ]), lists:sort(Keys)),
        Pot = filename:join(Dir, "singular.pot"),
        ok = z_gettext_compile:generate(Pot, [{Key, <<>>, undefined} || Key <- Keys]),
        ?assertEqual(lists:sort(Keys), lists:sort([Key || {Key, _} <- z_gettext:parse_pot(Pot), Key =/= header]))
    end).

with_directory(Fun) ->
    Dir = filename:join(os:getenv("TMPDIR", "/tmp"),
        "zotonic_plural_scan_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Dir),
    try Fun(Dir)
    after file:del_dir_r(Dir)
    end.
