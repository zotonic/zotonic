-module(z_sparql_sql_aggregate_db_tests).
-moduledoc("Database-backed SPARQL aggregate tests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


aggregate_queries_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, ObjectId} = m_rsc:insert(#{
        <<"category">> => article,
        <<"title">> => <<"SPARQL aggregate object">>
    }, Context),
    {ok, SubjectAId} = m_rsc:insert(#{
        <<"category">> => article,
        <<"title">> => <<"SPARQL aggregate subject A">>,
        <<"is_published">> => true,
        <<"sparql_aggregate_int">> => 10,
        <<"sparql_aggregate_label">> => <<"A">>
    }, Context),
    {ok, SubjectBId} = m_rsc:insert(#{
        <<"category">> => article,
        <<"title">> => <<"SPARQL aggregate subject B">>,
        <<"is_published">> => false,
        <<"sparql_aggregate_int">> => 20,
        <<"sparql_aggregate_label">> => <<"B">>
    }, Context),
    try
        ObjectUri = m_rsc:uri(ObjectId, Context),
        {ok, _} = m_edge:insert(SubjectAId, relation, ObjectId, Context),
        {ok, _} = m_edge:insert(SubjectBId, relation, ObjectId, Context),
        ?assertEqual([2], search(aggregate_query(<<"COUNT(*)">>, ObjectUri), Context)),
        ?assertEqual([2], search(aggregate_query(<<"COUNT(DISTINCT *)">>, ObjectUri), Context)),
        ?assertEqual([2], search(aggregate_query(<<"COUNT(DISTINCT ?value)">>, ObjectUri), Context)),
        [Sum] = search(aggregate_query(<<"SUM(?value)">>, ObjectUri), Context),
        ?assertEqual(30, z_convert:to_integer(Sum)),
        ?assertEqual([10], search(aggregate_query(<<"MIN(?value)">>, ObjectUri), Context)),
        ?assertEqual([20], search(aggregate_query(<<"MAX(?value)">>, ObjectUri), Context)),
        [Average] = search(aggregate_query(<<"AVG(?value)">>, ObjectUri), Context),
        ?assertEqual(15.0, z_convert:to_float(Average)),
        [Labels] = search(
            aggregate_query(<<"GROUP_CONCAT(?label; SEPARATOR=\",\")">>, ObjectUri),
            Context),
        ?assertEqual([<<"A">>, <<"B">>], lists:sort(binary:split(Labels, <<",">>, [global]))),
        ?assertEqual(
            lists:sort([{false, 1}, {true, 1}]),
            lists:sort(search(grouped_count_query(ObjectUri), Context))),
        assert_sample(ObjectUri, Context)
    after
        ok = m_rsc:delete(SubjectAId, Context),
        ok = m_rsc:delete(SubjectBId, Context),
        ok = m_rsc:delete(ObjectId, Context)
    end.

aggregate_query(Aggregate, ObjectUri) ->
    <<
        "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
        "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
        "SELECT (", Aggregate/binary, " AS ?aggregate) WHERE {\n"
        "    ?subject dcterms:relation <", ObjectUri/binary, "> .\n"
        "    ?subject zotonic:sparql_aggregate_int ?value .\n"
        "    ?subject zotonic:sparql_aggregate_label ?label\n"
        "}"
    >>.

grouped_count_query(ObjectUri) ->
    <<
        "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
        "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
        "SELECT ?published (COUNT(*) AS ?count) WHERE {\n"
        "    ?subject dcterms:relation <", ObjectUri/binary, "> .\n"
        "    ?subject zotonic:is_published ?published\n"
        "}\n"
        "GROUP BY ?published\n"
        "HAVING (COUNT(*) >= 1)"
    >>.

assert_sample(ObjectUri, Context) ->
    Query = aggregate_query(<<"SAMPLE(?value)">>, ObjectUri),
    case z_db:database_version(Context) of
        {ok, {postgresql, Major, _Minor}} when Major >= 16 ->
            [Sample] = search(Query, Context),
            ?assert(lists:member(Sample, [10, 20]));
        {ok, {postgresql, Major, _Minor}} ->
            ?assertEqual(
                {error, {unsupported_postgresql_version, sample, Major}},
                z_sparql:search(Query, {1, 10}, Context))
    end.

search(Sparql, Context) ->
    {ok, #search_result{ result = Result }} = z_sparql:search(Sparql, {1, 20}, Context),
    Result.
