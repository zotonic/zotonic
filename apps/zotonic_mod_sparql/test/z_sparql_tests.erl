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

query_parse_error_message_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    ?assertMatch(
        {error, {query_parse, #{
            reason := <<"undefined:1:18: Unknown keyword or invalid prefixed name zotonic">>,
            message := <<"Unknown keyword or invalid prefixed name zotonic">>,
            line := 1,
            column := 18
        }}},
        mod_sparql:observe_search_query_parse(#search_query_parse{
            query = <<"SELECT ?r WHERE {zotonic }">>,
            query_type = <<"sparql">>,
            arguments = #{}
        }, Context)),
    ?assertMatch(
        {error, {query_parse, #{
            message := <<"Syntax error before \"WHERE\".">>,
            line := 1,
            column := 8
        }}},
        mod_sparql:observe_search_query_parse(#search_query_parse{
            query = <<"SELECT WHERE {}">>,
            query_type = <<"sparql">>,
            arguments = #{}
        }, Context)).

default_prologue_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ContextNoLang = z_context:set_language('x-default', Context),
    SiteBase = z_context:abs_url(<<"/">>, ContextNoLang),
    SiteNamespace = m_rsc:uri_prefix(ContextNoLang),
    RelativeIri = <<SiteBase/binary, "relative">>,
    LocalIri = <<SiteNamespace/binary, "local">>,
    LocalPublishedIri = <<SiteNamespace/binary, "is_published">>,

    {ok, DefaultQuery} = z_sparql:parse(<<
        "SELECT ?r WHERE { <relative> zotonic:id :local }"
    >>),
    ?assertMatch(
        {ok, #{ where := {triple,
            {iri, RelativeIri},
            #{
                iri := <<"http://zotonic.net/predicate/id">>,
                ns_prefix := <<"zotonic">>
            },
            {iri, LocalIri}
        }}},
        z_sparql_plan:to_query_plan(DefaultQuery, Context)),

    {ok, SiteQuery} = z_sparql:parse(<<
        "SELECT ?r WHERE { ?r :is_published true }"
    >>),
    ?assertMatch(
        {ok, #{ where := {triple, _, #{
            iri := LocalPublishedIri,
            ns_prefix := <<"site">>
        }, true}}},
        z_sparql_plan:to_query_plan(SiteQuery, Context)),

    {ok, SiteAliasQuery} = z_sparql:parse(<<
        "SELECT ?r WHERE { ?r site:is_published true }"
    >>),
    ?assertMatch(
        {ok, #{ where := {triple, _, #{
            iri := LocalPublishedIri,
            ns_prefix := <<"site">>
        }, true}}},
        z_sparql_plan:to_query_plan(SiteAliasQuery, Context)),

    {ok, ExplicitQuery} = z_sparql:parse(<<
        "BASE <https://example.test/base/> "
        "PREFIX : <https://example.test/id/> "
        "PREFIX zotonic: <https://example.test/predicate/> "
        "SELECT ?r WHERE { <relative> zotonic:id :local }"
    >>),
    ?assertMatch(
        {ok, #{ where := {triple,
            {iri, <<"https://example.test/base/relative">>},
            #{ iri := <<"https://example.test/predicate/id">> },
            {iri, <<"https://example.test/id/local">>}
        }}},
        z_sparql_plan:to_query_plan(ExplicitQuery, Context)).

blank_node_property_list_test() ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_notifier:observe(rdf_ns, {?MODULE, observe_rdf_ns}, 100, Context),
    ok = z_notifier:observe(sparql_mapping, {?MODULE, observe_sparql_mapping}, 100, Context),
    try
        {ok, Query} = z_sparql:parse(<<
            "PREFIX ex: <http://example/> "
            "SELECT ?o WHERE { "
                "[ ex:source ?s ] ex:p [ ex:q [ ex:r ?o ] ] . "
            "}"
        >>),
        ?assertMatch(
            {query, _, {select, _, _, _, {group, [
                {triple_pattern, {subject,
                    {blank_node_property_list, _},
                    [{predicate, _, [{blank_node_property_list, _}]}]}}
            ]}, _}},
            Query),
        {ok, #{ where := Where }} = z_sparql_plan:to_query_plan(Query, Context),
        ?assertEqual(
            [
                {{bnode, <<"anon1">>}, <<"http://example/source">>, {var, <<"s">>}},
                {{bnode, <<"anon1">>}, <<"http://example/p">>, {bnode, <<"anon2">>}},
                {{bnode, <<"anon2">>}, <<"http://example/q">>, {bnode, <<"anon3">>}},
                {{bnode, <<"anon3">>}, <<"http://example/r">>, {var, <<"o">>}}
            ],
            plan_triples(Where)),

        {ok, StandaloneQuery} = z_sparql:parse(<<
            "PREFIX ex: <http://example/> "
            "SELECT ?o WHERE { [ ex:p ?o ] . }"
        >>),
        {ok, #{ where := StandaloneWhere }} =
            z_sparql_plan:to_query_plan(StandaloneQuery, Context),
        ?assertEqual(
            [{{bnode, <<"anon1">>}, <<"http://example/p">>, {var, <<"o">>}}],
            plan_triples(StandaloneWhere))
    after
        z_notifier:detach(rdf_ns, Context),
        z_notifier:detach(sparql_mapping, Context)
    end.

inverse_predicate_path_test() ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_notifier:observe(rdf_ns, {?MODULE, observe_rdf_ns}, 100, Context),
    ok = z_notifier:observe(sparql_mapping, {?MODULE, observe_sparql_mapping}, 100, Context),
    try
        {ok, Query} = z_sparql:parse(<<
            "PREFIX ex: <http://example/> "
            "SELECT ?parent WHERE { ?child ^ex:parent ?parent . }"
        >>),
        ?assertMatch(
            {query, _, {select, _, _, _, {group, [
                {triple_pattern, {subject, {var, <<"child">>}, [
                    {predicate, {inverse, {pname, <<"ex:parent">>}}, [{var, <<"parent">>}]}
                ]}}
            ]}, _}},
            Query),
        {ok, #{ where := Where }} = z_sparql_plan:to_query_plan(Query, Context),
        ?assertEqual(
            [{{var, <<"parent">>}, <<"http://example/parent">>, {var, <<"child">>}}],
            plan_triples(Where)),

        {ok, TypeQuery} = z_sparql:parse(<<
            "SELECT ?class WHERE { ?resource ^a ?class . }"
        >>),
        {ok, #{ where := TypeWhere }} = z_sparql_plan:to_query_plan(TypeQuery, Context),
        ?assertEqual(
            [{{var, <<"class">>},
              <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#type">>,
              {var, <<"resource">>}}],
            plan_triples(TypeWhere))
    after
        z_notifier:detach(rdf_ns, Context),
        z_notifier:detach(sparql_mapping, Context)
    end.

category_mapping_test() ->
    ?assertEqual(<<"text">>, z_rdf_props:category_mapping(<<"dctype:Text">>)),
    ?assertEqual(
        <<"text">>,
        z_rdf_props:category_mapping(<<"http://purl.org/dc/dcmitype/Text">>)),
    ?assertEqual(<<"article">>, z_rdf_props:category_mapping(<<"schema:Article">>)),
    ?assertEqual(
        <<"article">>,
        z_rdf_props:category_mapping(<<"https://schema.org/Article">>)).

rdf_type_mapping_failure_test() ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_notifier:observe(sparql_mapping, {?MODULE, observe_sparql_mapping}, 100, Context),
    try
        {ok, Query} = z_sparql:parse(<<
            "SELECT ?r WHERE { "
            "?r a <http://example.test/UnavailableType> "
            "}"
        >>),
        ?assertMatch(
            {ok, #{
                where := {triple, _, #{ mapping := type_unavailable }, _}
            }},
            z_sparql_plan:to_query_plan(Query, Context)),
        ?assertMatch(
            {ok, [#search_sql_term{ where = [<<"false">>] }, #search_sql_term{}]},
            z_sparql_sql:to_sql_term(Query, Context))
    after
        z_notifier:detach(sparql_mapping, Context)
    end.

zotonic_rsc_mapping_test() ->
    ?assertEqual(<<"name">>, z_rdf_props:mapping(<<"zotonic:name">>)),
    ?assertEqual(<<"id">>, z_rdf_props:mapping(<<"zotonic:id">>)).

select_query_plan_test() ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
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
        z_sparql_plan:to_query_plan(
            Query,
            z_acl:sudo(z_context:new(zotonic_site_testsandbox)))).

observe_rdf_ns(#rdf_ns{ ns = <<"http://xmlns.com/foaf/0.1/">> }, _Context) ->
    {ok, <<"foaf">>};
observe_rdf_ns(#rdf_ns{}, _Context) ->
    undefined.

observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"foaf">>, predicate = <<"name">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"title">>], text}};
observe_sparql_mapping(#sparql_mapping{ predicate = <<"type">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"type">>], text}};
observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"https://example.test/vocab#">>, predicate = <<"label">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"label">>], text}};
observe_sparql_mapping(#sparql_mapping{}, _Context) ->
    undefined.

plan_triples({triple, Subject, #{ iri := Predicate }, Object}) ->
    [{Subject, Predicate, Object}];
plan_triples({join, Left, Right}) ->
    plan_triples(Left) ++ plan_triples(Right).
