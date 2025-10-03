-module(z_sparql_db_tests).
-moduledoc("Database-backed SPARQL query tests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


relation_edge_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, ObjectId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL relation object">>}
    ], Context),
    {ok, SubjectId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL relation subject">>}
    ], Context),
    try
        {ok, _EdgeId} = m_edge:insert(SubjectId, relation, ObjectId, Context),
        ObjectUri = m_rsc:uri(ObjectId, Context),
        Sparql = <<
            "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
            "SELECT ?subject WHERE {\n"
            "    ?subject dcterms:relation <", ObjectUri/binary, ">\n"
            "}"
        >>,
        ?assertEqual([SubjectId], search(Sparql, Context))
    after
        ok = m_rsc:delete(SubjectId, Context),
        ok = m_rsc:delete(ObjectId, Context)
    end.

is_published_column_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, ObjectId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL unpublished relation object">>},
        {is_published, false}
    ], Context),
    {ok, PublishedId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL published relation subject">>},
        {is_published, true}
    ], Context),
    {ok, UnpublishedId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL unpublished relation subject">>},
        {is_published, false}
    ], Context),
    try
        {ok, _PublishedEdgeId} = m_edge:insert(PublishedId, relation, ObjectId, Context),
        {ok, _UnpublishedEdgeId} = m_edge:insert(UnpublishedId, relation, ObjectId, Context),
        ObjectUri = m_rsc:uri(ObjectId, Context),
        PublishedSparql = <<
            "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
            "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
            "SELECT ?subject WHERE {\n"
            "    ?subject dcterms:relation <", ObjectUri/binary, "> .\n"
            "    ?subject zotonic:is_published true .\n"
            "    <", ObjectUri/binary, "> zotonic:is_published false\n"
            "}"
        >>,
        UnpublishedSparql = <<
            "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
            "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
            "SELECT ?subject WHERE {\n"
            "    ?subject dcterms:relation <", ObjectUri/binary, "> .\n"
            "    ?subject zotonic:is_published false\n"
            "}"
        >>,
        ?assertEqual([PublishedId], search(PublishedSparql, Context)),
        ?assertEqual([UnpublishedId], search(UnpublishedSparql, Context))
    after
        ok = m_rsc:delete(PublishedId, Context),
        ok = m_rsc:delete(UnpublishedId, Context),
        ok = m_rsc:delete(ObjectId, Context)
    end.

search(Sparql, Context) ->
    {ok, #search_result{ result = Result }} = z_sparql:search(Sparql, {1, 10}, Context),
    Result.
