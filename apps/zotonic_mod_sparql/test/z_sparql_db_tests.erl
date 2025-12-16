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
        InverseSparql = <<
            "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
            "SELECT ?subject WHERE {\n"
            "    <", ObjectUri/binary, "> ^dcterms:relation ?subject\n"
            "}"
        >>,
        {ok, RelationId} = m_predicate:name_to_id(relation, Context),
        ?assertEqual({edge, RelationId, false}, query_mapping(Sparql, Context)),
        ?assertEqual([SubjectId], search(Sparql, Context)),
        ?assertEqual([SubjectId], search(InverseSparql, Context))
    after
        ok = m_rsc:delete(SubjectId, Context),
        ok = m_rsc:delete(ObjectId, Context)
    end.

reversed_relation_edge_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, PredicateId} = m_predicate:insert(<<"SPARQL reversed predicate">>, Context),
    {ok, PredicateId} = m_rsc:update(PredicateId, [{reversed, true}], Context),
    {ok, StoredSubjectId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL stored edge subject">>}
    ], Context),
    {ok, StoredObjectId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL stored edge object">>}
    ], Context),
    try
        {ok, _EdgeId} = m_edge:insert(StoredSubjectId, PredicateId, StoredObjectId, Context),
        PredicateUri = m_rsc:uri(PredicateId, Context),
        StoredSubjectUri = m_rsc:uri(StoredSubjectId, Context),
        StoredObjectUri = m_rsc:uri(StoredObjectId, Context),
        ReversedSparql = <<
            "SELECT ?subject WHERE {\n"
            "    ?subject <", PredicateUri/binary, "> <", StoredSubjectUri/binary, ">\n"
            "}"
        >>,
        InverseReversedSparql = <<
            "SELECT ?subject WHERE {\n"
            "    <", StoredSubjectUri/binary, "> ^<", PredicateUri/binary, "> ?subject\n"
            "}"
        >>,
        StoredDirectionSparql = <<
            "SELECT ?subject WHERE {\n"
            "    ?subject <", PredicateUri/binary, "> <", StoredObjectUri/binary, ">\n"
            "}"
        >>,
        ?assertEqual({edge, PredicateId, true}, query_mapping(ReversedSparql, Context)),
        ?assertEqual([StoredObjectId], search(ReversedSparql, Context)),
        ?assertEqual([StoredObjectId], search(InverseReversedSparql, Context)),
        ?assertEqual([], search(StoredDirectionSparql, Context))
    after
        ok = m_rsc:delete(StoredSubjectId, Context),
        ok = m_rsc:delete(StoredObjectId, Context),
        ok = m_rsc:delete(PredicateId, Context)
    end.

resource_identifier_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    UniqueName = z_string:to_name(<<"sparql_rid_", (z_ids:id(12))/binary>>),
    {ok, RscId} = m_rsc:insert([
        {category, article},
        {name, UniqueName},
        {title, <<"SPARQL resource identifier">>}
    ], Context),
    try
        {ok, _EdgeId} = m_edge:insert(RscId, relation, RscId, Context),
        CompactNameSparql = <<
            "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
            "SELECT ?subject WHERE {\n"
            "    ?subject zotonic:name \"", UniqueName/binary, "\"\n"
            "}"
        >>,
        FullNameSparql = <<
            "SELECT ?subject WHERE {\n"
            "    ?subject <http://zotonic.net/predicate/name> \"",
                UniqueName/binary, "\"\n"
            "}"
        >>,
        CompactIdSparql = <<
            "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
            "SELECT ?subject WHERE {\n"
            "    ?subject zotonic:id ", (integer_to_binary(RscId))/binary, "\n"
            "}"
        >>,
        FullIdSparql = <<
            "SELECT ?subject WHERE {\n"
            "    ?subject <http://zotonic.net/predicate/id> ",
                (integer_to_binary(RscId))/binary, "\n"
            "}"
        >>,
        CompactRscSparql = <<
            "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
            "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
            "SELECT ?subject WHERE {\n"
            "    ?subject dcterms:relation zotonic:", UniqueName/binary, "\n"
            "}"
        >>,
        FullRscSparql = <<
            "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
            "SELECT ?subject WHERE {\n"
            "    ?subject dcterms:relation <http://zotonic.net/predicate/",
                (integer_to_binary(RscId))/binary, ">\n"
            "}"
        >>,
        ?assertEqual([RscId], search(CompactNameSparql, Context)),
        ?assertEqual([RscId], search(FullNameSparql, Context)),
        ?assertEqual([RscId], search(CompactIdSparql, Context)),
        ?assertEqual([RscId], search(FullIdSparql, Context)),
        ?assertEqual([RscId], search(CompactRscSparql, Context)),
        ?assertEqual([RscId], search(FullRscSparql, Context)),
        RscIdBinary = integer_to_binary(RscId),
        ?assertEqual(RscId, m_rsc:uri_lookup(<<"zotonic:", UniqueName/binary>>, Context)),
        ?assertEqual(
            RscId,
            m_rsc:uri_lookup(
                <<"http://zotonic.net/predicate/", UniqueName/binary>>,
                Context)),
        ?assertEqual(RscId, m_rsc:uri_lookup(<<"zotonic:", RscIdBinary/binary>>, Context)),
        ?assertEqual(
            RscId,
            m_rsc:uri_lookup(
                <<"http://zotonic.net/predicate/", RscIdBinary/binary>>,
                Context))
    after
        ok = m_rsc:delete(RscId, Context)
    end.

json_property_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, RscId} = m_rsc:insert(#{
            <<"category">> => article,
            <<"title">> => <<"SPARQL nested JSON property">>,
            <<"sparql_nested">> => #{
                <<"number">> => 42,
                <<"text">> => <<"42">>,
                <<"is_enabled">> => true
            }
        }, Context),
    try
        Prefix = <<"PREFIX zotonic: <http://zotonic.net/predicate/>\n">>,
        NumberSparql = <<
            Prefix/binary,
            "SELECT ?subject WHERE { ?subject zotonic:sparql_nested.number 42 }"
        >>,
        NumberAsTextSparql = <<
            Prefix/binary,
            "SELECT ?subject WHERE { ?subject zotonic:sparql_nested.number \"42\" }"
        >>,
        TextSparql = <<
            Prefix/binary,
            "SELECT ?subject WHERE { ?subject zotonic:sparql_nested.text \"42\" }"
        >>,
        BooleanSparql = <<
            Prefix/binary,
            "SELECT ?subject WHERE { ?subject zotonic:sparqlNested.isEnabled true }"
        >>,
        FullIriSparql = <<
            "SELECT ?subject WHERE {\n"
            "    ?subject <http://zotonic.net/predicate/sparql_nested.number> 42\n"
            "}"
        >>,
        ?assertEqual([RscId], search(NumberSparql, Context)),
        ?assertEqual([], search(NumberAsTextSparql, Context)),
        ?assertEqual([RscId], search(TextSparql, Context)),
        ?assertEqual([RscId], search(BooleanSparql, Context)),
        ?assertEqual([RscId], search(FullIriSparql, Context))
    after
        ok = m_rsc:delete(RscId, Context)
    end.

category_type_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, ObjectId} = m_rsc:insert([
        {category, event},
        {title, <<"SPARQL category relation object">>}
    ], Context),
    {ok, ArticleId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL category article">>}
    ], Context),
    {ok, NewsId} = m_rsc:insert([
        {category, news},
        {title, <<"SPARQL category news">>}
    ], Context),
    {ok, PersonId} = m_rsc:insert([
        {category, person},
        {title, <<"SPARQL category person">>}
    ], Context),
    try
        {ok, _ArticleEdgeId} = m_edge:insert(ArticleId, relation, ObjectId, Context),
        {ok, _NewsEdgeId} = m_edge:insert(NewsId, relation, ObjectId, Context),
        {ok, _PersonEdgeId} = m_edge:insert(PersonId, relation, ObjectId, Context),
        {ok, TextCategoryId} = m_category:name_to_id(text, Context),
        {ok, ArticleCategoryId} = m_category:name_to_id(article, Context),
        {ok, NewsCategoryId} = m_category:name_to_id(news, Context),
        ?assertEqual(TextCategoryId, m_rsc:uri_lookup(<<"dctype:Text">>, Context)),
        ?assertEqual(
            TextCategoryId,
            m_rsc:uri_lookup(<<"http://purl.org/dc/dcmitype/Text">>, Context)),
        ?assertEqual(ArticleCategoryId, m_rsc:uri_lookup(<<"schema:Article">>, Context)),
        ?assertEqual(
            ArticleCategoryId,
            m_rsc:uri_lookup(<<"https://schema.org/Article">>, Context)),
        ObjectUri = m_rsc:uri(ObjectId, Context),
        DctypeSparql = <<
            "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
            "PREFIX dctype: <http://purl.org/dc/dcmitype/>\n"
            "SELECT ?subject WHERE {\n"
            "    ?subject dcterms:relation <", ObjectUri/binary, "> .\n"
            "    ?subject a dctype:Text\n"
            "}"
        >>,
        TypeSparql = <<
            "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
            "PREFIX schema: <https://schema.org/>\n"
            "SELECT ?subject WHERE {\n"
            "    ?subject dcterms:relation <", ObjectUri/binary, "> .\n"
            "    ?subject a schema:Article\n"
            "}"
        >>,
        SubclassSparql = <<
            "PREFIX dctype: <http://purl.org/dc/dcmitype/>\n"
            "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
            "SELECT ?category WHERE {\n"
            "    ?category rdfs:subClassOf dctype:Text\n"
            "}"
        >>,
        FullSubclassSparql = <<
            "PREFIX schema: <https://schema.org/>\n"
            "SELECT ?category WHERE {\n"
            "    ?category <http://www.w3.org/2000/01/rdf-schema#subClassOf> schema:Article\n"
            "}"
        >>,
        Expected = lists:sort([ArticleId, NewsId]),
        ?assertEqual(Expected, lists:sort(search(DctypeSparql, Context))),
        ?assertEqual(Expected, lists:sort(search(TypeSparql, Context))),
        ?assertEqual(
            lists:sort([ArticleCategoryId, NewsCategoryId]),
            lists:sort(search(SubclassSparql, Context))),
        ?assertEqual([NewsCategoryId], search(FullSubclassSparql, Context))
    after
        ok = m_rsc:delete(ArticleId, Context),
        ok = m_rsc:delete(NewsId, Context),
        ok = m_rsc:delete(PersonId, Context),
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

arguments_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, RelatedId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL argument relation object">>}
    ], Context),
    {ok, MatchingId} = m_rsc:insert([
        {category, article},
        {title, <<"sparql_argument_title">>},
        {date_start, {{2008, 12, 11}, {12, 0, 0}}},
        {is_published, false}
    ], Context),
    {ok, EarlierId} = m_rsc:insert([
        {category, article},
        {title, <<"sparql_argument_title">>},
        {date_start, {{2008, 12, 9}, {12, 0, 0}}},
        {is_published, false}
    ], Context),
    {ok, PublishedMatchingId} = m_rsc:insert([
        {category, article},
        {title, <<"sparql_argument_title">>},
        {date_start, {{2008, 12, 11}, {12, 0, 0}}},
        {is_published, true}
    ], Context),
    try
        {ok, _MatchingEdgeId} = m_edge:insert(MatchingId, relation, RelatedId, Context),
        {ok, _EarlierEdgeId} = m_edge:insert(EarlierId, relation, RelatedId, Context),
        {ok, _PublishedMatchingEdgeId} =
            m_edge:insert(PublishedMatchingId, relation, RelatedId, Context),
        Sparql = <<
            "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
            "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
            "SELECT ?subject WHERE {\n"
            "    ?subject dcterms:relation ?related .\n"
            "    ?subject zotonic:is_published ?published .\n"
            "    ?subject zotonic:date_start ?date_start .\n"
            "    ?subject zotonic:title ?title .\n"
            "    FILTER (?date_start >= ?since)\n"
            "}"
        >>,
        Arguments = #{
            related => {rsc, RelatedId},
            published => false,
            <<"since">> => {2008, 12, 10},
            title => sparql_argument_title
        },
        ?assertEqual([MatchingId], search(Sparql, Arguments, Context)),
        UnboundArguments = Arguments#{ published => undefined },
        ?assertEqual(
            lists:sort([MatchingId, PublishedMatchingId]),
            lists:sort(search(Sparql, UnboundArguments, Context)))
    after
        ok = m_rsc:delete(MatchingId, Context),
        ok = m_rsc:delete(EarlierId, Context),
        ok = m_rsc:delete(PublishedMatchingId, Context),
        ok = m_rsc:delete(RelatedId, Context)
    end.

facet_and_pivot_column_mapping_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ?assertEqual(
        {column, <<"search_facet">>, <<"f_category">>, id},
        predicate_mapping(<<"facet.category">>, Context)),
    ?assertEqual(
        {column, <<"rsc">>, <<"pivot_date_start">>, datetime},
        predicate_mapping(<<"pivot.dateStart">>, Context)),
    ?assertEqual(
        {jsonb, <<"rsc">>, <<"props_json">>, [<<"pivot">>, <<"not_defined">>], text},
        predicate_mapping(<<"pivot.notDefined">>, Context)).

predicate_mapping(Predicate, Context) ->
    Sparql = <<
        "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
        "SELECT ?subject WHERE { ?subject zotonic:", Predicate/binary, " ?value }"
    >>,
    query_mapping(Sparql, Context).

query_mapping(Sparql, Context) ->
    {ok, Query} = z_sparql:parse(Sparql),
    {ok, #{ where := {triple, _, #{ mapping := Mapping }, _} }} = z_sparql_plan:to_query_plan(Query, Context),
    Mapping.

search(Sparql, Context) ->
    {ok, #search_result{ result = Result }} = z_sparql:search(Sparql, {1, 10}, Context),
    Result.

search(Sparql, Arguments, Context) ->
    {ok, #search_result{ result = Result }} = z_sparql:search(Sparql, Arguments, {1, 10}, Context),
    Result.
