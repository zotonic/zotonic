-module(z_sparql_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").

-export([
    observe_rdf_ns/2,
    observe_sparql_mapping/2
]).

parse_list_test() ->
    ?assertMatch(
        {ok, {query, [], {ask, [], {group, []}, _}}},
        z_sparql:parse("ASK WHERE {}")),
    ?assertMatch(
        {ok, {query, [], {select, default, all, [], {group, []}, _}}},
        z_sparql:parse("SELECT * WHERE {}")).

parse_select_test() ->
    Filename = filename:join([filename:dirname(?FILE), "data", "select.sparql"]),
    {ok, Query} = file:read_file(Filename),
    ?assertMatch(
        {ok,
            {query,
                [{prefix, <<"foaf:">>, <<"http://xmlns.com/foaf/0.1/">>}],
                {select,
                    default,
                    [{var, <<"name">>}],
                    [],
                    {group,
                        [
                            {triple_pattern,
                                {subject,
                                    {var, <<"x">>},
                                    [
                                        {predicate,
                                            {pname, <<"foaf:name">>},
                                            [{var, <<"name">>}]}
                                    ]}}
                        ]},
                    _}}},
        z_sparql:parse(Query)).

scanner_error_test() ->
    ?assertMatch(
        {error, <<_/binary>>},
        z_sparql:parse(<<"SELECT @ WHERE {}">>)).

parser_error_test() ->
    ?assertMatch(
        {error, {_Location, z_sparql_parser, _Message}},
        z_sparql:parse(<<"SELECT WHERE {}">>)).

category_mapping_test() ->
    ?assertEqual(<<"text">>, z_rdf_props:category_mapping(<<"dctype:Text">>)),
    ?assertEqual(
        <<"text">>,
        z_rdf_props:category_mapping(<<"http://purl.org/dc/dcmitype/Text">>)),
    ?assertEqual(<<"article">>, z_rdf_props:category_mapping(<<"schema:Article">>)),
    ?assertEqual(
        <<"article">>,
        z_rdf_props:category_mapping(<<"https://schema.org/Article">>)).

zotonic_rsc_mapping_test() ->
    ?assertEqual(<<"name">>, z_rdf_props:mapping(<<"zotonic:name">>)),
    ?assertEqual(<<"id">>, z_rdf_props:mapping(<<"zotonic:id">>)).

select_query_plan_test() ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_context:new(zotonic_site_testsandbox),
    ok = z_notifier:observe(rdf_ns, {?MODULE, observe_rdf_ns}, 100, Context),
    ok = z_notifier:observe(sparql_mapping, {?MODULE, observe_sparql_mapping}, 100, Context),
    try
        {ok, Query} = z_sparql:parse(<<
            "PREFIX f: <http://xmlns.com/foaf/0.1/> "
            "SELECT ?name WHERE { ?person f:name ?name }"
        >>),
        {ok, Plan} = z_sparql_plan:to_query_plan(Query, Context),
        ?assertMatch(
            #{
                type := select,
                root := {var, <<"person">>},
                where :=
                    {triple,
                        {var, <<"person">>},
                        #{
                            iri := <<"http://xmlns.com/foaf/0.1/name">>,
                            ns := <<"http://xmlns.com/foaf/0.1/">>,
                            ns_prefix := <<"foaf">>,
                            predicate := <<"name">>,
                            mapping := {jsonb, <<"rsc">>, <<"props_json">>, [<<"title">>], text}
                        },
                        {var, <<"name">>}}
            },
            Plan),
        ?assertMatch(
            {ok, [#search_sql_term{}, #search_sql_term{}]},
            z_sparql_sql:to_sql_term(Query, Context)),

        {ok, UnknownNsQuery} = z_sparql:parse(<<
            "PREFIX ex: <https://example.test/vocab#> "
            "SELECT ?value WHERE { ?subject ex:label ?value }"
        >>),
        {ok, UnknownNsPlan} = z_sparql_plan:to_query_plan(UnknownNsQuery, Context),
        ?assertMatch(
            #{
                where := {triple, _, #{
                    iri := <<"https://example.test/vocab#label">>,
                    ns := <<"https://example.test/vocab#">>,
                    ns_prefix := <<"https://example.test/vocab#">>,
                    predicate := <<"label">>
                }, _}
            },
            UnknownNsPlan)
    after
        z_notifier:detach(rdf_ns, Context),
        z_notifier:detach(sparql_mapping, Context)
    end.

non_select_query_plan_test() ->
    {ok, Query} = z_sparql:parse(<<"ASK WHERE {}">>),
    ?assertEqual(
        {error, {unsupported_query, ask}},
        z_sparql_plan:to_query_plan(Query, z_context:new(zotonic_site_testsandbox))).

observe_rdf_ns(#rdf_ns{ ns = <<"http://xmlns.com/foaf/0.1/">> }, _Context) ->
    {ok, <<"foaf">>};
observe_rdf_ns(#rdf_ns{}, _Context) ->
    undefined.

observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"foaf">>, predicate = <<"name">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"title">>], text}};
observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"https://example.test/vocab#">>, predicate = <<"label">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"label">>], text}};
observe_sparql_mapping(#sparql_mapping{}, _Context) ->
    undefined.
