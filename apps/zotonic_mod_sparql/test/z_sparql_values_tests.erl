-module(z_sparql_values_tests).
-moduledoc("SPARQL VALUES data block tests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").

-export([
    observe_rdf_ns/2,
    observe_sparql_mapping/2
]).

general_values_parser_test() ->
    {ok, {query, _, {select, _, _, _, {group, [Values]}, _}}} = z_sparql:parse(<<
        "PREFIX : <http://example.org/book/> "
        "SELECT ?x ?y WHERE { "
            "VALUES (?x ?y) { (:uri1 1) (:uri2 UNDEF) } "
        "}"
    >>),
    ?assertEqual(
        {values,
            [{var, <<"x">>}, {var, <<"y">>}],
            [
                [{pname, <<":uri1">>}, {integer, <<"1">>}],
                [{pname, <<":uri2">>}, undefined]
            ]},
        Values).

single_variable_and_trailing_values_parser_test() ->
    {ok, {query, _, {select, _, _, _, {group, [InlineValues]}, _}}} = z_sparql:parse(<<
        "SELECT ?z WHERE { VALUES ?z { \"abc\" \"def\" } }"
    >>),
    {ok, {query, _, {select, _, _, _, {group, [TrailingValues]}, _}}} = z_sparql:parse(<<
        "SELECT ?z WHERE {} VALUES (?z) { (\"abc\") (\"def\") }"
    >>),
    Expected = {values,
        [{var, <<"z">>}],
        [[{literal, <<"abc">>}], [{literal, <<"def">>}]]},
    ?assertEqual(Expected, InlineValues),
    ?assertEqual(Expected, TrailingValues).

invalid_values_row_test() ->
    Context = test_context(),
    {ok, Query} = z_sparql:parse(<<
        "SELECT ?x WHERE { VALUES (?x ?y) { (1) } }"
    >>),
    ?assertEqual(
        {error, {invalid_values_row, 2, [{integer, <<"1">>}]}},
        z_sparql_plan:to_query_plan(Query, Context)).

values_sql_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/> "
                "SELECT ?person ?name ?minimum WHERE { "
                    "?person test:name ?name . "
                    "VALUES (?name ?minimum) { (\"Alice\" 1) (UNDEF 2) } "
                    "FILTER (?minimum > 0) "
                "}"
            >>),
            {ok, Plan} = z_sparql_plan:to_query_plan(Query, Context),
            ?assertMatch(
                #{ where :=
                    {filter, _,
                        {join,
                            {triple, _, _, _},
                            {values, [{var, <<"name">>}, {var, <<"minimum">>}], [_, _]}}}},
                Plan),
            {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
            #search_sql{ from = From, where = Where, args = Args } =
                z_search_terms:combine(Terms, Context),
            ?assertNotEqual(nomatch, binary:match(From, <<"VALUES">>)),
            ?assertNotEqual(nomatch, binary:match(From, <<"defined_1">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"NOT sparql_values_">>)),
            ?assert(lists:member(<<"Alice">>, Args)),
            ?assert(lists:member(1, Args)),
            ?assert(lists:member(2, Args))
        end).

with_observers(Fun) ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = test_context(),
    ok = z_notifier:observe(rdf_ns, {?MODULE, observe_rdf_ns}, 100, Context),
    ok = z_notifier:observe(sparql_mapping, {?MODULE, observe_sparql_mapping}, 100, Context),
    try
        Fun(Context)
    after
        z_notifier:detach(rdf_ns, Context),
        z_notifier:detach(sparql_mapping, Context)
    end.

test_context() ->
    z_acl:sudo(z_context:new(zotonic_site_testsandbox)).

observe_rdf_ns(#rdf_ns{ ns = <<"https://example.test/">> }, _Context) ->
    {ok, <<"test">>};
observe_rdf_ns(#rdf_ns{}, _Context) ->
    undefined.

observe_sparql_mapping(#sparql_mapping{
        ns_prefix = <<"test">>,
        predicate = <<"name">>
    }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"name">>], text}};
observe_sparql_mapping(#sparql_mapping{}, _Context) ->
    undefined.
