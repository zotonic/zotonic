-module(z_sparql_exists_tests).
-moduledoc("SPARQL EXISTS and NOT EXISTS expression tests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").

-export([
    observe_rdf_ns/2,
    observe_sparql_mapping/2
]).


exists_plan_and_sql_scope_test() ->
    with_observers(
        fun(Context) ->
            {Query, Plan} = query_and_plan(exists, Context),
            ?assertMatch(
                #{
                    where := {filter,
                        {exists, {join, {triple, _, _, _}, {triple, _, _, _}}},
                        {triple, _, _, _}}
                },
                Plan),

            {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
            ?assert(has_nested_operator(Terms, <<"anyof">>)),
            Sql = z_search_terms:combine(Terms, Context),
            ?assertEqual(<<"rsc rsc">>, Sql#search_sql.from),
            ?assert(contains(Sql#search_sql.where, <<"EXISTS (SELECT 1 FROM rsc ">>)),
            ?assert(contains(Sql#search_sql.where, <<"rsc.props_json">>)),
            ?assert(contains(Sql#search_sql.where, <<".id IS NOT NULL">>))
        end).

not_exists_plan_and_sql_scope_test() ->
    with_observers(
        fun(Context) ->
            {Query, Plan} = query_and_plan(not_exists, Context),
            ?assertMatch(
                #{
                    where := {filter,
                        {not_exists, {join, {triple, _, _, _}, {triple, _, _, _}}},
                        {triple, _, _, _}}
                },
                Plan),

            {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
            ?assert(has_nested_operator(Terms, <<"noneof">>)),
            Sql = z_search_terms:combine(Terms, Context),
            ?assertEqual(<<"rsc rsc">>, Sql#search_sql.from),
            ?assert(contains(Sql#search_sql.where, <<"NOT EXISTS (SELECT 1 FROM rsc ">>)),
            ?assert(contains(Sql#search_sql.where, <<"rsc.props_json">>))
        end).

empty_exists_patterns_test() ->
    with_observers(
        fun(Context) ->
            Exists = empty_query(<<"EXISTS">>),
            NotExists = empty_query(<<"NOT EXISTS">>),
            {ok, ExistsTerms} = z_sparql_sql:to_sql_term(Exists, Context),
            {ok, NotExistsTerms} = z_sparql_sql:to_sql_term(NotExists, Context),
            ExistsSql = z_search_terms:combine(ExistsTerms, Context),
            NotExistsSql = z_search_terms:combine(NotExistsTerms, Context),
            ?assert(contains(ExistsSql#search_sql.where, <<"true">>)),
            ?assert(contains(NotExistsSql#search_sql.where, <<"NOT ">>)),
            ?assert(contains(NotExistsSql#search_sql.where, <<"true">>))
        end).

compound_exists_expressions_test() ->
    with_observers(
        fun(Context) ->
            Sql = sql(<<
                "SELECT ?subject WHERE { ?subject test:id ?id . "
                "FILTER ((EXISTS { ?object test:id 101 } && ?id > 202) "
                "|| NOT EXISTS { ?other test:id 303 }) }"
            >>, Context),
            ?assertEqual(<<"rsc rsc">>, Sql#search_sql.from),
            ?assert(contains(Sql#search_sql.where, <<" OR ">>)),
            ?assert(contains(Sql#search_sql.where, <<" AND ">>)),
            ?assert(contains(Sql#search_sql.where, <<"NOT EXISTS (SELECT 1">>)),
            ?assertEqual([101, 202, 303], lists:sort(Sql#search_sql.args)),
            assert_parameter(Sql#search_sql.where, <<".id = ">>, 101, Sql),
            assert_parameter(Sql#search_sql.where, <<"rsc.id > ">>, 202, Sql),
            assert_parameter(Sql#search_sql.where, <<".id = ">>, 303, Sql)
        end).

projected_arguments_in_exists_test() ->
    with_observers(
        fun(Context) ->
            Sql = sql(<<
                "SELECT ?subject (101 AS ?n) "
                "(EXISTS { FILTER (?n = 202) } AS ?different) "
                "(NOT EXISTS { FILTER (?n = 202) } AS ?negated) "
                "(EXISTS { FILTER (?n = ?n) } AS ?same) "
                "(EXISTS { ?subject test:id ?n } AS ?matchesId) "
                "WHERE { ?subject test:id ?id } ORDER BY ?n"
            >>, Context),
            ?assertEqual([101, 202], lists:sort(Sql#search_sql.args)),
            [N] = [I || {I, 101} <- lists:zip(lists:seq(1, length(Sql#search_sql.args)), Sql#search_sql.args)],
            [Other] = [I || {I, 202} <- lists:zip(lists:seq(1, length(Sql#search_sql.args)), Sql#search_sql.args)],
            NArg = <<"$", (integer_to_binary(N))/binary>>,
            OtherArg = <<"$", (integer_to_binary(Other))/binary>>,
            ?assert(contains(Sql#search_sql.select, <<NArg/binary, " = ", OtherArg/binary>>)),
            ?assert(contains(Sql#search_sql.select, <<NArg/binary, " = ", NArg/binary>>)),
            ?assert(contains(Sql#search_sql.select, <<"rsc.id = ", NArg/binary>>)),
            ?assertEqual(<<NArg/binary, " ASC">>, Sql#search_sql.order)
        end).

result_exists_expressions_test() ->
    with_observers(
        fun(Context) ->
            Sql = sql(<<
                "SELECT ?subject (EXISTS { ?subject test:title ?title } AS ?hasTitle) "
                "(NOT EXISTS {} AS ?never) "
                "(IF(EXISTS { ?object test:id 101 }, 202, 303) AS ?label) "
                "WHERE { ?subject test:id ?id } ORDER BY ?hasTitle"
            >>, Context),
            ?assertEqual(<<"rsc rsc">>, Sql#search_sql.from),
            ?assertNot(contains(Sql#search_sql.where, <<"EXISTS">>)),
            ?assert(contains(Sql#search_sql.select, <<"EXISTS (SELECT 1">>)),
            ?assert(contains(Sql#search_sql.select, <<"NOT EXISTS (SELECT 1)">>)),
            ?assert(contains(Sql#search_sql.select, <<"CASE WHEN">>)),
            ?assert(contains(Sql#search_sql.order, <<"EXISTS (SELECT 1">>)),
            assert_parameter(Sql#search_sql.select, <<".id = ">>, 101, Sql)
        end).

nested_exists_scope_test() ->
    with_observers(
        fun(Context) ->
            Sql = sql(<<
                "SELECT ?subject (EXISTS { ?object test:id ?objectId . "
                "FILTER (EXISTS { ?inner test:id ?objectId } && true) } AS ?found) "
                "WHERE { ?subject test:id ?id }"
            >>, Context),
            ?assertEqual(<<"rsc rsc">>, Sql#search_sql.from),
            ?assert(contains(Sql#search_sql.select, <<"FROM rsc sparql_e1_rsc_1">>)),
            ?assert(contains(Sql#search_sql.select, <<"FROM rsc sparql_e2_rsc_2">>)),
            ?assert(contains(Sql#search_sql.select,
                <<"sparql_e2_rsc_2.id = sparql_e1_rsc_1.id">>)),
            ?assertNot(contains(Sql#search_sql.where, <<"sparql_e">>)),
            {ok, Query} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/exists#> "
                "SELECT ?subject ?local WHERE { ?subject test:id ?id . "
                "FILTER (EXISTS { ?local test:id ?localId } || true) }"
            >>),
            ?assertEqual({error, {unbound_variable, {var, <<"local">>}}},
                z_sparql_sql:to_sql_term(Query, Context))
        end).

sql(Body, Context) ->
    {ok, Query} = z_sparql:parse(<<
        "PREFIX test: <https://example.test/exists#> ", Body/binary
    >>),
    {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
    z_search_terms:combine(Terms, Context).

assert_parameter(Fragment, Prefix, Value, #search_sql{ args = Args }) ->
    [Nr] = [N || {N, Arg} <- lists:zip(lists:seq(1, length(Args)), Args), Arg =:= Value],
    ?assert(contains(Fragment, <<Prefix/binary, "$", (integer_to_binary(Nr))/binary>>)).

query_and_plan(Operator, Context) ->
    Keyword = case Operator of
        exists -> <<"EXISTS">>;
        not_exists -> <<"NOT EXISTS">>
    end,
    {ok, Query} = z_sparql:parse(<<
        "PREFIX test: <https://example.test/exists#>\n"
        "SELECT ?subject WHERE {\n"
        "    ?subject test:id ?subject_id .\n"
        "    FILTER ", Keyword/binary, " {\n"
        "        ?subject test:title ?title .\n"
        "        ?object test:id ?object_id\n"
        "    }\n"
        "}"
    >>),
    {ok, Plan} = z_sparql_plan:to_query_plan(Query, Context),
    {Query, Plan}.

empty_query(Keyword) ->
    {ok, Query} = z_sparql:parse(<<
        "PREFIX test: <https://example.test/exists#>\n"
        "SELECT ?subject WHERE {\n"
        "    ?subject test:id ?subject_id .\n"
        "    FILTER ", Keyword/binary, " {}\n"
        "}"
    >>),
    Query.

with_observers(Fun) ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_notifier:observe(rdf_ns, {?MODULE, observe_rdf_ns}, 100, Context),
    ok = z_notifier:observe(sparql_mapping, {?MODULE, observe_sparql_mapping}, 100, Context),
    try
        Fun(Context)
    after
        z_notifier:detach(rdf_ns, Context),
        z_notifier:detach(sparql_mapping, Context)
    end.

observe_rdf_ns(#rdf_ns{ ns = <<"https://example.test/exists#">> }, _Context) ->
    {ok, <<"test">>};
observe_rdf_ns(#rdf_ns{}, _Context) ->
    undefined.

observe_sparql_mapping(#sparql_mapping{
        ns_prefix = <<"test">>,
        predicate = <<"id">>
    }, _Context) ->
    {ok, {column, <<"rsc">>, <<"id">>, id}};
observe_sparql_mapping(#sparql_mapping{
        ns_prefix = <<"test">>,
        predicate = <<"title">>
    }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"title">>], text}};
observe_sparql_mapping(#sparql_mapping{}, _Context) ->
    undefined.

has_nested_operator(Terms, Operator) when is_list(Terms) ->
    lists:any(fun(Term) -> has_nested_operator(Term, Operator) end, Terms);
has_nested_operator(#search_sql_nested{ operator = Operator }, Operator) ->
    true;
has_nested_operator(#search_sql_nested{ terms = Terms }, Operator) ->
    has_nested_operator(Terms, Operator);
has_nested_operator(#search_sql_term{}, _Operator) ->
    false.

contains(Text, Part) ->
    binary:match(Text, Part) =/= nomatch.
