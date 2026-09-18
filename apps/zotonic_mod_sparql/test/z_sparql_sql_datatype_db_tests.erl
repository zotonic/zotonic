-module(z_sparql_sql_datatype_db_tests).
-moduledoc("Database-backed XSD datatype constructor tests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


datetime_constructor_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    DateTime = {{2008, 12, 10}, {12, 34, 56}},
    DateTimeText = <<"2008-12-10T12:34:56Z">>,
    UnixSeconds = 1228912496,
    {ok, RscId} = m_rsc:insert(#{
        <<"category">> => article,
        <<"title">> => <<"SPARQL datetime constructor">>,
        <<"sparql_date_text">> => DateTimeText,
        <<"sparql_unix_int">> => UnixSeconds
    }, Context),
    1 = z_db:q1(
        <<"update rsc set created = $1 where id = $2">>,
        [DateTime, RscId],
        Context),
    try
        Sparql = <<
            "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n"
            "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
            "SELECT ?subject WHERE {\n"
            "    ?subject zotonic:created ?created .\n"
            "    ?subject zotonic:sparql_date_text ?datetime_string .\n"
            "    ?subject zotonic:sparql_unix_int ?unix_seconds .\n"
            "    FILTER(xsd:dateTime(?created) = \"", DateTimeText/binary, "\"^^xsd:dateTime)\n"
            "    FILTER(xsd:dateTime(?datetime_string) = \"", DateTimeText/binary, "\"^^xsd:dateTime)\n"
            "    FILTER(xsd:dateTime(?unix_seconds) = \"", DateTimeText/binary, "\"^^xsd:dateTime)\n"
            "    FILTER(xsd:dateTime(", (integer_to_binary(UnixSeconds))/binary,
                ") = \"", DateTimeText/binary, "\"^^xsd:dateTime)\n"
            "}"
        >>,
        ?assertEqual([RscId], search(Sparql, Context))
    after
        ok = m_rsc:delete(RscId, Context)
    end.

datatype_function_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, Id} = m_rsc:insert(#{<<"category">> => article,
        <<"title">> => <<"DATATYPE test">>, <<"sparql_datatype_value">> => 42}, Context),
    Root = <<"?s <http://zotonic.net/predicate/id> ", (integer_to_binary(Id))/binary, " . ">>,
    Xsd = <<"http://www.w3.org/2001/XMLSchema#">>,
    try
        lists:foreach(fun({Expression, Expected}) ->
            Query = <<"PREFIX xsd: <", Xsd/binary, "> "
                "SELECT (DATATYPE(", Expression/binary, ") AS ?dt) WHERE { ", Root/binary, " }">>,
            ?assertEqual([{Expected}], result_columns(Query, Context))
        end, [
            {<<"1">>, <<Xsd/binary, "integer">>},
            {<<"1.5">>, <<Xsd/binary, "decimal">>},
            {<<"1e0">>, <<Xsd/binary, "double">>},
            {<<"true">>, <<Xsd/binary, "boolean">>},
            {<<"\"plain\"">>, <<Xsd/binary, "string">>},
            {<<"\"hello\"@en">>, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#langString">>},
            {<<"\"1\"^^xsd:int">>, <<Xsd/binary, "int">>},
            {<<"\"invalid\"^^xsd:integer">>, <<Xsd/binary, "integer">>},
            {<<"xsd:short(1)">>, <<Xsd/binary, "short">>},
            {<<"\"opaque\"^^<https://example.test/custom>">>, <<"https://example.test/custom">>},
            {<<"<https://example.test/resource>">>, undefined},
            {<<"?s">>, undefined}
        ]),
        lists:foreach(fun({Select, Pattern, Expected}) ->
            Query = <<"SELECT ", Select/binary, " WHERE { ", Root/binary, Pattern/binary, " }">>,
            ?assertEqual(Expected, result_columns(Query, Context))
        end, [
            {<<"(?v AS ?word)">>,
                <<"OPTIONAL { VALUES ?v { \"hi\"@en } "
                  "FILTER(DATATYPE(?v) = <http://www.w3.org/1999/02/22-rdf-syntax-ns#langString>) }">>,
                [{<<"hi">>}]},
            {<<"(EXISTS { FILTER(DATATYPE(?v) = <http://www.w3.org/2001/XMLSchema#integer>) } AS ?found)">>,
                <<"OPTIONAL { VALUES ?v { 1 } }">>, [{true}]},
            {<<"(DATATYPE(CONCAT(?v, ?w)) AS ?dt)">>,
                <<"OPTIONAL { VALUES (?v ?w) { (\"hi\"@en \"there\"@en) (\"hi\"@en \"daar\"@nl) } }">>,
                [{<<"http://www.w3.org/1999/02/22-rdf-syntax-ns#langString">>}, {<<Xsd/binary, "string">>}]},
            {<<"(DATATYPE(?v + 1) AS ?dt)">>,
                <<"OPTIONAL { VALUES ?v { 1 } }">>, [{<<Xsd/binary, "integer">>}]},
            {<<"(DATATYPE(?v) AS ?dt)">>,
                <<"VALUES ?v { 1 UNDEF }">>, [{<<Xsd/binary, "integer">>}, {undefined}]},
            {<<"(DATATYPE(?v) AS ?dt)">>,
                <<"OPTIONAL { VALUES ?v { \"hello\"@en } }">>,
                [{<<"http://www.w3.org/1999/02/22-rdf-syntax-ns#langString">>}]},
            {<<"(DATATYPE(?v) AS ?dt)">>,
                <<"OPTIONAL { VALUES ?v { 1 } FILTER(false) }">>, [{undefined}]},
            {<<"(DATATYPE(?v) AS ?dt)">>,
                <<"OPTIONAL { OPTIONAL { VALUES ?v { 1 } } }">>, [{<<Xsd/binary, "integer">>}]},
            {<<"(DATATYPE(COALESCE(?v, \"fallback\")) AS ?dt)">>,
                <<"OPTIONAL { VALUES ?v { \"hi\"@en } FILTER(false) }">>, [{<<Xsd/binary, "string">>}]},
            {<<"(DATATYPE(IF(true, ?v, \"fallback\")) AS ?dt)">>,
                <<"VALUES ?v { \"hi\"@en }">>,
                [{<<"http://www.w3.org/1999/02/22-rdf-syntax-ns#langString">>}]},
            {<<"(UCASE(?v) AS ?word) (DATATYPE(?word) AS ?dt)">>,
                <<"OPTIONAL { VALUES ?v { \"hi\"@en } }">>,
                [{<<"HI">>, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#langString">>}]},
            {<<"(DATATYPE(?v) AS ?dt)">>,
                <<"OPTIONAL { ?s <http://zotonic.net/predicate/sparql_datatype_value> ?v }">>,
                [{<<Xsd/binary, "decimal">>}]}
        ])
    after
        ok = m_rsc:delete(Id, Context)
    end.

result_columns(Query, Context) ->
    [list_to_tuple(tl(tuple_to_list(Row))) || Row <- search(Query, Context)].

search(Sparql, Context) ->
    {ok, #search_result{ result = Result }} = z_sparql:search(Sparql, {1, 10}, Context),
    Result.
