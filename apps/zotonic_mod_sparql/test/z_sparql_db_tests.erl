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
        {ok, _AuthorEdgeId} = m_edge:insert(RscId, author, 1, Context),
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
        DefaultZotonicSparql = <<
            "SELECT ?subject WHERE {\n"
            "    ?subject zotonic:relation zotonic:", UniqueName/binary, "\n"
            "}"
        >>,
        DefaultSiteSparql = <<
            "SELECT ?subject WHERE {\n"
            "    ?subject :relation :", (integer_to_binary(RscId))/binary, "\n"
            "}"
        >>,
        DefaultSiteNameSparql = <<
            "SELECT ?subject WHERE {\n"
            "    ?subject :relation :", UniqueName/binary, "\n"
            "}"
        >>,
        AdministratorIdSparql = <<
            "SELECT ?subject WHERE {\n"
            "    ?subject :author :1\n"
            "}"
        >>,
        AdministratorNameSparql = <<
            "SELECT ?subject WHERE {\n"
            "    ?subject :author :administrator\n"
            "}"
        >>,
        SiteAliasSparql = <<
            "SELECT ?subject WHERE {\n"
            "    ?subject site:relation site:", (integer_to_binary(RscId))/binary, "\n"
            "}"
        >>,
        ?assertEqual([RscId], search(CompactNameSparql, Context)),
        ?assertEqual([RscId], search(FullNameSparql, Context)),
        ?assertEqual([RscId], search(CompactIdSparql, Context)),
        ?assertEqual([RscId], search(FullIdSparql, Context)),
        ?assertEqual([RscId], search(CompactRscSparql, Context)),
        ?assertEqual([RscId], search(FullRscSparql, Context)),
        ?assertEqual([RscId], search(DefaultZotonicSparql, Context)),
        SiteNamespace = m_rsc:uri_prefix(Context),
        ?assertEqual(
            RscId,
            m_rsc:uri_lookup(
                <<SiteNamespace/binary, (integer_to_binary(RscId))/binary>>,
                Context)),
        ?assertEqual(
            RscId,
            m_rsc:uri_lookup(<<SiteNamespace/binary, UniqueName/binary>>, Context)),
        ?assertEqual([RscId], search(DefaultSiteSparql, Context)),
        ?assertEqual([RscId], search(DefaultSiteNameSparql, Context)),
        ?assertEqual([RscId], search(AdministratorIdSparql, Context)),
        ?assertEqual([RscId], search(AdministratorNameSparql, Context)),
        ?assertEqual([RscId], search(SiteAliasSparql, Context)),
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

unavailable_type_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ?assertEqual([], search(<<
        "SELECT ?r WHERE { "
        "?r a <http://example.test/UnavailableType> "
        "}"
    >>, Context)).

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

values_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, TutorialId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL Tutorial">>}
    ], Context),
    {ok, SemanticWebId} = m_rsc:insert([
        {category, article},
        {title, <<"The Semantic Web">>}
    ], Context),
    {ok, OtherId} = m_rsc:insert([
        {category, article},
        {title, <<"Another SPARQL Book">>}
    ], Context),
    try
        SemanticWebUri = m_rsc:uri(SemanticWebId, Context),
        InlineSparql = <<
            "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
            "SELECT ?book ?title WHERE {\n"
            "    ?book zotonic:title ?title .\n"
            "    VALUES (?book ?title) {\n"
            "        (UNDEF \"SPARQL Tutorial\")\n"
            "        (<", SemanticWebUri/binary, "> UNDEF)\n"
            "    }\n"
            "}"
        >>,
        TrailingSparql = <<
            "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
            "SELECT ?book ?title WHERE {\n"
            "    ?book zotonic:title ?title .\n"
            "}\n"
            "VALUES (?book ?title) {\n"
            "    (UNDEF \"SPARQL Tutorial\")\n"
            "    (<", SemanticWebUri/binary, "> UNDEF)\n"
            "}"
        >>,
        Expected = lists:sort([
            {TutorialId, <<"SPARQL Tutorial">>},
            {SemanticWebId, <<"The Semantic Web">>}
        ]),
        ?assertEqual(Expected, lists:sort(search(InlineSparql, Context))),
        ?assertEqual(Expected, lists:sort(search(TrailingSparql, Context)))
    after
        ok = m_rsc:delete(TutorialId, Context),
        ok = m_rsc:delete(SemanticWebId, Context),
        ok = m_rsc:delete(OtherId, Context)
    end.

optional_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    Title = <<"SPARQL optional title">>,
    {ok, WithTitleId} = m_rsc:insert([
        {category, article},
        {title, Title}
    ], Context),
    {ok, WithoutTitleId} = m_rsc:insert([
        {category, article}
    ], Context),
    {ok, ObjectId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL optional relation object">>}
    ], Context),
    try
        {ok, _EdgeId} = m_edge:insert(WithTitleId, relation, ObjectId, Context),
        WithTitleUri = m_rsc:uri(WithTitleId, Context),
        WithoutTitleUri = m_rsc:uri(WithoutTitleId, Context),
        PropertySparql = <<
            "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
            "SELECT ?subject ?title WHERE {\n"
            "    VALUES ?subject {\n"
            "        <", WithTitleUri/binary, ">\n"
            "        <", WithoutTitleUri/binary, ">\n"
            "    }\n"
            "    OPTIONAL { ?subject zotonic:title ?title }\n"
            "}"
        >>,
        ?assertEqual(
            lists:sort([
                {WithTitleId, Title},
                {WithoutTitleId, undefined}
            ]),
            lists:sort(search(PropertySparql, Context))),

        FilteredPropertySparql = <<
            "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
            "SELECT ?subject ?title WHERE {\n"
            "    VALUES ?subject {\n"
            "        <", WithTitleUri/binary, ">\n"
            "        <", WithoutTitleUri/binary, ">\n"
            "    }\n"
            "    OPTIONAL {\n"
            "        ?subject zotonic:title ?title .\n"
            "        FILTER (?title = \"does not match\")\n"
            "    }\n"
            "}"
        >>,
        ?assertEqual(
            lists:sort([
                {WithTitleId, undefined},
                {WithoutTitleId, undefined}
            ]),
            lists:sort(search(FilteredPropertySparql, Context))),

        RelationSparql = <<
            "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
            "SELECT ?subject ?object WHERE {\n"
            "    VALUES ?subject {\n"
            "        <", WithTitleUri/binary, ">\n"
            "        <", WithoutTitleUri/binary, ">\n"
            "    }\n"
            "    OPTIONAL { ?subject dcterms:relation ?object }\n"
            "}"
        >>,
        ?assertEqual(
            lists:sort([
                {WithTitleId, ObjectId},
                {WithoutTitleId, undefined}
            ]),
            lists:sort(search(RelationSparql, Context)))
    after
        ok = m_rsc:delete(WithTitleId, Context),
        ok = m_rsc:delete(WithoutTitleId, Context),
        ok = m_rsc:delete(ObjectId, Context)
    end.

exists_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, WithRelationId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL EXISTS relation subject">>}
    ], Context),
    {ok, WithoutRelationId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL EXISTS subject without relation">>}
    ], Context),
    {ok, ObjectId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL EXISTS relation object">>}
    ], Context),
    try
        {ok, _EdgeId} = m_edge:insert(
            WithRelationId, relation, ObjectId, Context),
        WithRelationUri = m_rsc:uri(WithRelationId, Context),
        WithoutRelationUri = m_rsc:uri(WithoutRelationId, Context),
        ExistsSparql = exists_query(
            <<"EXISTS">>, WithRelationUri, WithoutRelationUri),
        NotExistsSparql = exists_query(
            <<"NOT EXISTS">>, WithRelationUri, WithoutRelationUri),
        ?assertEqual([WithRelationId], search(ExistsSparql, Context)),
        ?assertEqual([WithoutRelationId], search(NotExistsSparql, Context))
    after
        ok = m_rsc:delete(WithRelationId, Context),
        ok = m_rsc:delete(WithoutRelationId, Context),
        ok = m_rsc:delete(ObjectId, Context)
    end.

exists_result_expressions_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, WithId} = m_rsc:insert(#{ <<"category">> => article }, Context),
    {ok, WithoutId} = m_rsc:insert(#{ <<"category">> => article }, Context),
    {ok, ObjectId} = m_rsc:insert(#{ <<"category">> => article }, Context),
    try
        {ok, _} = m_edge:insert(WithId, relation, ObjectId, Context),
        {ok, _} = m_edge:insert(WithId, relation, WithoutId, Context),
        WithUri = m_rsc:uri(WithId, Context),
        WithoutUri = m_rsc:uri(WithoutId, Context),
        Prefix = <<"PREFIX dcterms: <http://purl.org/dc/terms/> ">>,
        Values = <<"VALUES ?subject { <", WithUri/binary, "> <", WithoutUri/binary, "> } ">>,
        Pattern = <<"{ ?subject dcterms:relation ?object }">>,
        ResultQuery = <<Prefix/binary,
            "SELECT ?subject (EXISTS ", Pattern/binary, " AS ?hasRelation) "
            "(NOT EXISTS ", Pattern/binary, " AS ?missing) "
            "(IF(EXISTS ", Pattern/binary, ", \"yes\", \"no\") AS ?label) "
            "(EXISTS {} AS ?always) (NOT EXISTS {} AS ?never) "
            "WHERE { ", Values/binary, "} ORDER BY ?missing"
        >>,
        % Two inner matches still produce exactly one row for the subject.
        ?assertEqual([
            {WithId, true, false, <<"yes">>, true, false},
            {WithoutId, false, true, <<"no">>, true, false}
        ], search(ResultQuery, Context)),
        OrQuery = <<Prefix/binary, "SELECT ?subject WHERE { ", Values/binary,
            "FILTER (EXISTS ", Pattern/binary, " || NOT EXISTS ", Pattern/binary, ") }">>,
        ?assertEqual(lists:sort([WithId, WithoutId]), lists:sort(search(OrQuery, Context))),
        AndQuery = <<Prefix/binary, "SELECT ?subject WHERE { ", Values/binary,
            "FILTER (EXISTS ", Pattern/binary, " && !(NOT EXISTS ", Pattern/binary, ")) }">>,
        ?assertEqual([WithId], search(AndQuery, Context)),
        NestedQuery = <<Prefix/binary,
            "SELECT ?subject (EXISTS { ?subject dcterms:relation ?object . "
            "FILTER (NOT EXISTS { ?object dcterms:relation ?other } && true) } AS ?found) "
            "WHERE { ", Values/binary, "}">>,
        ?assertEqual(lists:sort([{WithId, true}, {WithoutId, false}]),
            lists:sort(search(NestedQuery, Context))),
        OptionalQuery = <<Prefix/binary,
            "SELECT ?subject (EXISTS { ?subject dcterms:relation ?object } AS ?found) "
            "WHERE { ", Values/binary,
            "OPTIONAL { ?subject dcterms:relation ?object } }">>,
        ?assertEqual(lists:sort([{WithId, true}, {WithId, true}, {WithoutId, false}]),
            lists:sort(search(OptionalQuery, Context))),
        AggregateQuery = <<Prefix/binary,
            "SELECT (COUNT(DISTINCT EXISTS ", Pattern/binary, ") AS ?matches) "
            "WHERE { ", Values/binary, "}">>,
        ?assertEqual([2], search(AggregateQuery, Context)),
        HavingQuery = <<Prefix/binary,
            "SELECT ?subject WHERE { ", Values/binary, "} GROUP BY ?subject "
            "HAVING (EXISTS ", Pattern/binary, " = true)">>,
        ?assertEqual([WithId], search(HavingQuery, Context)),
        UnionQuery = <<Prefix/binary,
            "SELECT ?subject (EXISTS { { ?subject dcterms:relation ?object } "
            "UNION { ?object dcterms:relation ?subject } } AS ?found) "
            "WHERE { ", Values/binary, "}">>,
        ?assertEqual(lists:sort([{WithId, true}, {WithoutId, true}]),
            lists:sort(search(UnionQuery, Context)))
    after
        ok = m_rsc:delete(WithId, Context),
        ok = m_rsc:delete(WithoutId, Context),
        ok = m_rsc:delete(ObjectId, Context)
    end.

exists_scope_regressions_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, Id} = m_rsc:insert(#{
        <<"category">> => article,
        <<"is_published">> => true
    }, Context),
    try
        Uri = m_rsc:uri(Id, Context),
        Where = <<"WHERE { VALUES ?subject { <", Uri/binary, "> } }">>,
        Query = <<"SELECT ?subject (101 AS ?n) "
            "(EXISTS { FILTER (?n = 202) } AS ?different) "
            "(NOT EXISTS { FILTER (?n = 202) } AS ?negated) "
            "(EXISTS { FILTER (?n = ?n) } AS ?same) "
            "(EXISTS { FILTER (EXISTS { FILTER (?n = 202) } || false) } AS ?nested) "
            "(EXISTS { FILTER (?different) } AS ?reused) ",
            Where/binary, " ORDER BY ?n"
        >>,
        ?assertEqual([{Id, <<"101">>, false, true, true, false, false}], search(Query, Context)),
        DeepExpression = deep_exists_expression(12),
        DeepQuery = <<"SELECT ?subject (", DeepExpression/binary, " AS ?found) ", Where/binary>>,
        ?assertEqual([{Id, true}], search(DeepQuery, Context))
    after
        ok = m_rsc:delete(Id, Context)
    end.

deep_exists_expression(1) ->
    <<"EXISTS { ?a zotonic:is_published true . ?b zotonic:is_published true }">>;
deep_exists_expression(Depth) ->
    Inner = deep_exists_expression(Depth - 1),
    <<"EXISTS { FILTER (", Inner/binary, " && true) }">>.

exists_query(Keyword, WithRelationUri, WithoutRelationUri) ->
    <<
        "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
        "SELECT ?subject WHERE {\n"
        "    VALUES ?subject {\n"
        "        <", WithRelationUri/binary, ">\n"
        "        <", WithoutRelationUri/binary, ">\n"
        "    }\n"
        "    FILTER ", Keyword/binary,
            " { ?subject dcterms:relation ?object }\n"
        "}"
    >>.

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
