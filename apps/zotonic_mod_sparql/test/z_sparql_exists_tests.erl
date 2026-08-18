-module(z_sparql_exists_tests).
-moduledoc("SPARQL standalone FILTER EXISTS and FILTER NOT EXISTS tests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").

-export([
    observe_rdf_ns/2,
    observe_sparql_mapping/2
]).


exists_plan_and_sql_scope_test() ->
    with_observers(
        fun(Context) ->
            {Query, Plan} = query_and_plan(exists, Context),
            ?assertMatch(
                #{
                    where := {filter,
                        {exists, {join, {triple, _, _, _}, {triple, _, _, _}}},
                        {triple, _, _, _}}
                },
                Plan),

            {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
            ?assert(has_nested_operator(Terms, <<"anyof">>)),
            Sql = z_search_terms:combine(Terms, Context),
            ?assertEqual(<<"rsc rsc">>, Sql#search_sql.from),
            ?assert(contains(Sql#search_sql.where, <<"EXISTS (SELECT 1 FROM rsc ">>)),
            ?assert(contains(Sql#search_sql.where, <<"rsc.props_json">>)),
            ?assert(contains(Sql#search_sql.where, <<".id IS NOT NULL">>))
        end).

not_exists_plan_and_sql_scope_test() ->
    with_observers(
        fun(Context) ->
            {Query, Plan} = query_and_plan(not_exists, Context),
            ?assertMatch(
                #{
                    where := {filter,
                        {not_exists, {join, {triple, _, _, _}, {triple, _, _, _}}},
                        {triple, _, _, _}}
                },
                Plan),

            {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
            ?assert(has_nested_operator(Terms, <<"noneof">>)),
            Sql = z_search_terms:combine(Terms, Context),
            ?assertEqual(<<"rsc rsc">>, Sql#search_sql.from),
            ?assert(contains(Sql#search_sql.where, <<"NOT EXISTS (SELECT 1 FROM rsc ">>)),
            ?assert(contains(Sql#search_sql.where, <<"rsc.props_json">>))
        end).

empty_exists_patterns_test() ->
    with_observers(
        fun(Context) ->
            Exists = empty_query(<<"EXISTS">>),
            NotExists = empty_query(<<"NOT EXISTS">>),
            {ok, ExistsTerms} = z_sparql_sql:to_sql_term(Exists, Context),
            {ok, NotExistsTerms} = z_sparql_sql:to_sql_term(NotExists, Context),
            ExistsSql = z_search_terms:combine(ExistsTerms, Context),
            NotExistsSql = z_search_terms:combine(NotExistsTerms, Context),
            ?assert(contains(ExistsSql#search_sql.where, <<"true">>)),
            ?assert(contains(NotExistsSql#search_sql.where, <<"NOT ">>)),
            ?assert(contains(NotExistsSql#search_sql.where, <<"true">>))
        end).

combined_exists_expression_is_explicitly_unsupported_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/exists#>\n"
                "SELECT ?subject WHERE {\n"
                "    ?subject test:id ?subject_id .\n"
                "    FILTER (EXISTS { ?subject test:title ?title } && true)\n"
                "}"
            >>),
            ?assertEqual(
                {error, {unsupported, exists_expression}},
                z_sparql_sql:to_sql_term(Query, Context))
        end).

query_and_plan(Operator, Context) ->
    Keyword = case Operator of
        exists -> <<"EXISTS">>;
        not_exists -> <<"NOT EXISTS">>
    end,
    {ok, Query} = z_sparql:parse(<<
        "PREFIX test: <https://example.test/exists#>\n"
        "SELECT ?subject WHERE {\n"
        "    ?subject test:id ?subject_id .\n"
        "    FILTER ", Keyword/binary, " {\n"
        "        ?subject test:title ?title .\n"
        "        ?object test:id ?object_id\n"
        "    }\n"
        "}"
    >>),
    {ok, Plan} = z_sparql_plan:to_query_plan(Query, Context),
    {Query, Plan}.

empty_query(Keyword) ->
    {ok, Query} = z_sparql:parse(<<
        "PREFIX test: <https://example.test/exists#>\n"
        "SELECT ?subject WHERE {\n"
        "    ?subject test:id ?subject_id .\n"
        "    FILTER ", Keyword/binary, " {}\n"
        "}"
    >>),
    Query.

with_observers(Fun) ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_notifier:observe(rdf_ns, {?MODULE, observe_rdf_ns}, 100, Context),
    ok = z_notifier:observe(sparql_mapping, {?MODULE, observe_sparql_mapping}, 100, Context),
    try
        Fun(Context)
    after
        z_notifier:detach(rdf_ns, Context),
        z_notifier:detach(sparql_mapping, Context)
    end.

observe_rdf_ns(#rdf_ns{ ns = <<"https://example.test/exists#">> }, _Context) ->
    {ok, <<"test">>};
observe_rdf_ns(#rdf_ns{}, _Context) ->
    undefined.

observe_sparql_mapping(#sparql_mapping{
        ns_prefix = <<"test">>,
        predicate = <<"id">>
    }, _Context) ->
    {ok, {column, <<"rsc">>, <<"id">>, id}};
observe_sparql_mapping(#sparql_mapping{
        ns_prefix = <<"test">>,
        predicate = <<"title">>
    }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"title">>], text}};
observe_sparql_mapping(#sparql_mapping{}, _Context) ->
    undefined.

has_nested_operator(Terms, Operator) when is_list(Terms) ->
    lists:any(fun(Term) -> has_nested_operator(Term, Operator) end, Terms);
has_nested_operator(#search_sql_nested{ operator = Operator }, Operator) ->
    true;
has_nested_operator(#search_sql_nested{ terms = Terms }, Operator) ->
    has_nested_operator(Terms, Operator);
has_nested_operator(#search_sql_term{}, _Operator) ->
    false.

contains(Text, Part) ->
    binary:match(Text, Part) =/= nomatch.
