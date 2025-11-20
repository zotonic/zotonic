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

search(Sparql, Context) ->
    {ok, #search_result{ result = Result }} = z_sparql:search(Sparql, {1, 10}, Context),
    Result.
