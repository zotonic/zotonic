-module(z_sparql_sql_aggregate_tests).
-moduledoc("SPARQL aggregate parser, plan, and SQL mapping tests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").

-export([
    observe_rdf_ns/2,
    observe_sparql_mapping/2
]).


postgresql_mapping_test_() ->
    Context = z_context:new(zotonic_site_testsandbox),
    [
        mapping(count, default, all, undefined, Context, <<"count(*)">>),
        mapping(count, distinct, <<"value">>, undefined, Context,
            <<"count(DISTINCT value)">>),
        mapping(sum, default, <<"value">>, undefined, Context,
            <<"coalesce(sum(value), 0)">>),
        mapping(sum, distinct, <<"value">>, undefined, Context,
            <<"coalesce(sum(DISTINCT value), 0)">>),
        mapping(min, default, <<"value">>, undefined, Context, <<"min(value)">>),
        mapping(max, default, <<"value">>, undefined, Context, <<"max(value)">>),
        mapping(avg, default, <<"value">>, undefined, Context,
            <<"coalesce(avg(value), 0)">>),
        mapping(group_concat, default, <<"value">>, undefined, Context,
            <<"coalesce(string_agg(value, ' '), CAST('' AS text))">>),
        mapping(group_concat, distinct, <<"value">>, <<"separator">>, Context,
            <<"coalesce(string_agg(DISTINCT value, separator), CAST('' AS text))">>)
    ].

type_signature_test() ->
    ?assertEqual({ok, {any, integer}}, z_sparql_sql_aggregate:type_signature(count)),
    ?assertEqual({ok, {number, number}}, z_sparql_sql_aggregate:type_signature(sum)),
    ?assertEqual({ok, {common, common}}, z_sparql_sql_aggregate:type_signature(min)),
    ?assertEqual({ok, {common, common}}, z_sparql_sql_aggregate:type_signature(max)),
    ?assertEqual({ok, {number, float}}, z_sparql_sql_aggregate:type_signature(avg)),
    ?assertEqual({ok, {any, common}}, z_sparql_sql_aggregate:type_signature(sample)),
    ?assertEqual({ok, {text, text}}, z_sparql_sql_aggregate:type_signature(group_concat)).

count_distinct_star_is_not_supported_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    ?assertEqual(
        {error, {unsupported, count_distinct_star}},
        z_sparql_sql_aggregate:to_sql(count, distinct, all, undefined, Context)).

count_distinct_solution_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/> "
                "SELECT (COUNT(DISTINCT *) AS ?count) WHERE { "
                    "?person test:value ?value "
                "}"
            >>),
            {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
            Select = (z_search_terms:combine(Terms))#search_sql.select,
            ?assertNotEqual(nomatch, binary:match(Select, <<"count(DISTINCT ROW(">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"rsc.id">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"props_json">>))
        end).

parse_aggregates_test() ->
    {ok, {query, [_], {select, default, Select, [], {group, _}, Modifier}}} =
        z_sparql:parse(aggregate_query()),
    ?assertMatch(
        [
            {var, <<"category">>},
            {as, {aggregate, count, default, all, undefined}, {var, <<"total">>}},
            {as, {aggregate, count, distinct, {var, <<"value">>}, undefined}, _},
            {as, {aggregate, sum, default, {var, <<"value">>}, undefined}, _},
            {as, {aggregate, min, default, {var, <<"value">>}, undefined}, _},
            {as, {aggregate, max, default, {var, <<"value">>}, undefined}, _},
            {as, {aggregate, avg, default, {var, <<"value">>}, undefined}, _},
            {as, {aggregate, group_concat, distinct, {var, <<"name">>}, <<",">>}, _}
        ],
        Select),
    ?assertMatch(
        {solution_modifier,
            [{var, <<"category">>}],
            [{'>=', {aggregate, count, default, all, undefined}, {integer, <<"1">>}}],
            [{order, desc, {var, <<"total">>}}],
            []},
        Modifier).

aggregate_query_plan_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(aggregate_query()),
            {ok, Plan} = z_sparql_plan:to_query_plan(Query, Context),
            ?assertMatch(#{
                root := {var, <<"person">>},
                group_by := [{var, <<"category">>}],
                having := [{'>=', {aggregate, count, default, all, undefined}, {integer, <<"1">>}}]
            }, Plan)
        end).

aggregate_sql_terms_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(aggregate_query()),
            {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
            SearchSql = z_search_terms:combine(Terms),
            Select = SearchSql#search_sql.select,
            ?assertEqual(nomatch, binary:match(Select, <<"rsc.id">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"count(*)">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"count(DISTINCT ">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"coalesce(sum(">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"min(">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"max(">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"coalesce(avg(">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"coalesce(string_agg(DISTINCT ">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<")::bigint">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"#>> '{}'">>)),
            ?assertEqual(<<"rsc.category_id">>, SearchSql#search_sql.group_by),
            ?assertNotEqual(nomatch, binary:match(SearchSql#search_sql.having, <<"count(*) >= ">>)),
            ?assertEqual(<<"count(*) DESC">>, SearchSql#search_sql.order)
        end).

mapping(Aggregate, Distinct, Argument, Separator, Context, Expected) ->
    ?_assertEqual(
        Expected,
        begin
            {ok, Sql} = z_sparql_sql_aggregate:to_sql(
                Aggregate, Distinct, Argument, Separator, Context),
            iolist_to_binary(Sql)
        end).

aggregate_query() ->
    <<
        "PREFIX test: <https://example.test/> "
        "SELECT ?category "
            "(COUNT(*) AS ?total) "
            "(COUNT(DISTINCT ?value) AS ?distinctValues) "
            "(SUM(?value) AS ?sum) "
            "(MIN(?value) AS ?minimum) "
            "(MAX(?value) AS ?maximum) "
            "(AVG(?value) AS ?average) "
            "(GROUP_CONCAT(DISTINCT ?name; SEPARATOR=\",\") AS ?names) "
        "WHERE { "
            "?person test:category ?category . "
            "?person test:value ?value . "
            "?person test:name ?name "
        "} "
        "GROUP BY ?category "
        "HAVING (COUNT(*) >= 1) "
        "ORDER BY DESC(?total)"
    >>.

with_observers(Fun) ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_context:new(zotonic_site_testsandbox),
    ok = z_notifier:observe(rdf_ns, {?MODULE, observe_rdf_ns}, 100, Context),
    ok = z_notifier:observe(sparql_mapping, {?MODULE, observe_sparql_mapping}, 100, Context),
    try
        Fun(Context)
    after
        z_notifier:detach(rdf_ns, Context),
        z_notifier:detach(sparql_mapping, Context)
    end.

observe_rdf_ns(#rdf_ns{ ns = <<"https://example.test/">> }, _Context) ->
    {ok, <<"test">>};
observe_rdf_ns(#rdf_ns{}, _Context) ->
    undefined.

observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"category">> }, _Context) ->
    {ok, {column, <<"rsc">>, <<"category_id">>, id}};
observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"value">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"value">>], integer}};
observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"name">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"name">>], text}};
observe_sparql_mapping(#sparql_mapping{}, _Context) ->
    undefined.
