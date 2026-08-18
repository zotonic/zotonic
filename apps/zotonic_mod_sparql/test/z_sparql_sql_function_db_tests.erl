-module(z_sparql_sql_function_db_tests).
-moduledoc("Database-backed SPARQL function tests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


literal_numeric_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, ObjectId} = m_rsc:insert(#{
        <<"category">> => article,
        <<"title">> => <<"SPARQL type check object">>
    }, Context),
    {ok, SubjectId} = m_rsc:insert(#{
        <<"category">> => article,
        <<"title">> => <<"SPARQL type check subject">>,
        <<"is_published">> => true,
        <<"sparql_type_check">> => #{
            <<"number">> => 42,
            <<"count_int">> => 42,
            <<"text">> => <<"42">>,
            <<"boolean">> => true,
            <<"is_enabled">> => true,
            <<"object">> => #{<<"number">> => 42},
            <<"translation">> => #trans{ tr = [
                {en, <<"Number">>},
                {nl, <<"Nummer">>}
            ]}
        }
    }, Context),
    try
        {ok, _EdgeId} = m_edge:insert(SubjectId, relation, ObjectId, Context),
        ObjectUri = m_rsc:uri(ObjectId, Context),
        ?assertEqual(
            [SubjectId],
            search(property_filter(<<"sparql_type_check.number">>, <<"isNumeric">>, ObjectUri), Context)),
        ?assertEqual(
            [],
            search(property_filter(<<"sparql_type_check.text">>, <<"isNumeric">>, ObjectUri), Context)),
        ?assertEqual(
            [SubjectId],
            search(property_filter(<<"sparql_type_check.number">>, <<"isLiteral">>, ObjectUri), Context)),
        ?assertEqual(
            [SubjectId],
            search(property_filter(<<"sparql_type_check.text">>, <<"isLiteral">>, ObjectUri), Context)),
        ?assertEqual(
            [SubjectId],
            search(property_filter(<<"sparql_type_check.boolean">>, <<"isLiteral">>, ObjectUri), Context)),
        ?assertEqual(
            [],
            search(property_filter(<<"sparql_type_check.object">>, <<"isLiteral">>, ObjectUri), Context)),
        ?assertEqual(
            [],
            search(property_filter(<<"sparql_type_check.translation">>, <<"isLiteral">>, ObjectUri), Context)),
        ?assertEqual(
            [SubjectId],
            search(property_filter(<<"is_published">>, <<"isLiteral">>, ObjectUri), Context)),
        ?assertEqual(
            [],
            search(property_filter(<<"is_published">>, <<"isNumeric">>, ObjectUri), Context)),
        ?assertEqual(
            [SubjectId],
            search(property_filter(<<"id">>, <<"isNumeric">>, ObjectUri), Context)),
        ?assertEqual(
            [],
            search(resource_filter(<<"isLiteral">>, ObjectUri), Context)),
        ?assertEqual(
            [],
            search(resource_filter(<<"isNumeric">>, ObjectUri), Context)),
        ?assertEqual(
            [SubjectId],
            search(
                property_expression_filter(
                    <<"sparql_type_check.count_int">>,
                    <<"?value + 1 = 43">>,
                    ObjectUri),
                Context)),
        ?assertEqual(
            [SubjectId],
            search(
                property_expression_filter(
                    <<"sparql_type_check.text">>,
                    <<"CONTAINS(?value, \"4\")">>,
                    ObjectUri),
                Context)),
        ?assertEqual(
            [SubjectId],
            search(
                property_expression_filter(
                    <<"sparql_type_check.is_enabled">>,
                    <<"?value && true">>,
                    ObjectUri),
                Context))
    after
        ok = m_rsc:delete(SubjectId, Context),
        ok = m_rsc:delete(ObjectId, Context)
    end.

uuid_and_iri_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    UniqueName = z_string:to_name(<<"sparql_uuid_", (z_ids:id(12))/binary>>),
    {ok, RscId} = m_rsc:insert(#{
        <<"category">> => article,
        <<"name">> => UniqueName,
        <<"title">> => <<"SPARQL UUID function">>
    }, Context),
    try
        Sparql = <<
            "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
            "SELECT ?subject (UUID() AS ?uuid) (STRUUID() AS ?string_uuid) "
                "(IRI(<https://example.test/iri>) AS ?iri) WHERE {\n"
            "    ?subject zotonic:name \"", UniqueName/binary, "\" .\n"
            "    FILTER(isIRI(?subject))\n"
            "    FILTER(isIRI(UUID()))\n"
            "    FILTER(!isIRI(STRUUID()))\n"
            "    FILTER(!isBLANK(?subject))\n"
            "}"
        >>,
        [{RscId, Uuid, StringUuid, Iri}] = search(Sparql, Context),
        ?assertMatch(<<"urn:uuid:", _/binary>>, Uuid),
        ?assertEqual(45, byte_size(Uuid)),
        ?assertEqual(36, byte_size(StringUuid)),
        ?assertEqual(<<"https://example.test/iri">>, Iri),
        ?assertMatch({match, _}, re:run(
            StringUuid,
            <<"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$">>))
    after
        ok = m_rsc:delete(RscId, Context)
    end.

property_filter(Predicate, Function, ObjectUri) ->
    <<
        "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
        "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
        "SELECT ?subject WHERE {\n"
        "    ?subject dcterms:relation <", ObjectUri/binary, "> .\n"
        "    ?subject zotonic:", Predicate/binary, " ?value .\n"
        "    FILTER(", Function/binary, "(?value))\n"
        "}"
    >>.

property_expression_filter(Predicate, Expression, ObjectUri) ->
    <<
        "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
        "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
        "SELECT ?subject WHERE {\n"
        "    ?subject dcterms:relation <", ObjectUri/binary, "> .\n"
        "    ?subject zotonic:", Predicate/binary, " ?value .\n"
        "    FILTER(", Expression/binary, ")\n"
        "}"
    >>.

resource_filter(Function, ObjectUri) ->
    <<
        "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
        "SELECT ?subject WHERE {\n"
        "    ?subject dcterms:relation <", ObjectUri/binary, "> .\n"
        "    FILTER(", Function/binary, "(?subject))\n"
        "}"
    >>.

search(Sparql, Context) ->
    {ok, #search_result{result = Result}} = z_sparql:search(Sparql, {1, 10}, Context),
    Result.
