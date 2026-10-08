-module(z_sparql_model_db_tests).
-moduledoc("Database-backed tests for the SPARQL model and query resources.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


query_classification_test() ->
    Context = context(),
    {ok, #{
        query_type := <<"search">>,
        is_live := true,
        show_parsed := true
    }} = search_query_resource:parse(<<"cat=article">>, Context),
    {ok, #{
        query_type := <<"search_json">>,
        is_live := true,
        show_parsed := true
    }} = search_query_resource:parse(
        <<"{\"q\":[{\"term\":\"cat\",\"value\":\"article\"}]}">>,
        Context),
    {ok, #{
        query_type := <<"sparql">>,
        is_live := false,
        show_parsed := false
    }} = search_query_resource:parse(<<
        "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
        "SELECT ?r WHERE { ?r zotonic:id 1 }"
    >>, Context),
    ?assertMatch(
        {error, {query_parse, #{
            query_type := <<"sparql">>,
            query_type_label := _,
            is_live := false,
            show_parsed := false,
            reason := _
        }}},
        search_query_resource:parse(<<"SELECT WHERE {">>, Context)),
    ?assertMatch(
        {error, {query_parse, #{
            query_type := <<"search_json">>,
            query_type_label := _,
            is_live := true,
            show_parsed := true,
            reason := query_type_mismatch
        }}},
        search_query_resource:parse(
            <<"cat=article">>, <<"search_json">>, #{}, Context)).

model_named_arguments_test() ->
    Context = context(),
    {ok, RscId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL model named argument">>}
    ], Context),
    try
        Query = id_query(),
        Args = #{
            <<"query">> => Query,
            <<"args">> => #{ <<"wanted">> => RscId },
            <<"pagelen">> => 10
        },
        ?assertMatch(
            {ok, #search_result{ result = [RscId] }},
            m_sparql:search(Args, Context)),
        ?assertMatch(
            {ok, #search_result{ result = [RscId] }},
            m_search:search(<<"sparql">>, Args, Context))
    after
        ok = m_rsc:delete(RscId, Context)
    end.

stored_query_resource_test() ->
    Context = context(),
    QueryName = <<"sparql_query_", (z_ids:id(12))/binary>>,
    {ok, RscId} = m_rsc:insert([
        {category, article},
        {title, <<"SPARQL stored query target">>}
    ], Context),
    try
        {ok, QueryId} = m_rsc:insert([
            {category, query},
            {name, QueryName},
            {title, <<"SPARQL stored named query">>},
            {query_type, <<"sparql">>},
            {query, id_query()},
            {is_query_live, true}
        ], Context),
        try
            ?assertEqual(<<"sparql">>, m_rsc:p(QueryId, query_type, Context)),
            ?assertMatch(
                {ok, #search_result{ result = [RscId] }},
                m_search:search(QueryName, #{
                    <<"args">> => #{ <<"wanted">> => RscId }
                }, Context)),
            ?assertNot(proplists:is_defined(
                QueryId,
                search_query_notify:init(Context)))
        after
            ok = m_rsc:delete(QueryId, Context)
        end
    after
        ok = m_rsc:delete(RscId, Context)
    end.

stored_query_requires_resource_projection_test() ->
    Context = context(),
    Query = <<
        "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
        "SELECT ?r ?id WHERE { ?r zotonic:id ?id }"
    >>,
    ?assertEqual(
        {error, invalid_query},
        m_rsc:insert([
            {category, query},
            {title, <<"Invalid SPARQL stored query">>},
            {query_type, <<"sparql">>},
            {query, Query}
        ], Context)).

id_query() ->
    <<
        "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
        "SELECT ?r WHERE { ?r zotonic:id ?wanted }"
    >>.

context() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    z_acl:sudo(z_context:new(zotonic_site_testsandbox)).
