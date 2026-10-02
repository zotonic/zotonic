-module(z_sparql_optional_tests).
-moduledoc("SPARQL OPTIONAL planning and SQL scope tests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").

-export([
    observe_rdf_ns/2,
    observe_sparql_mapping/2
]).


optional_plan_and_sql_scope_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/optional#>\n"
                "SELECT ?subject ?title WHERE {\n"
                "    ?subject test:id ?id .\n"
                "    OPTIONAL {\n"
                "        ?subject test:title ?title .\n"
                "        FILTER (?title = \"included\")\n"
                "    }\n"
                "}"
            >>),
            ?assertMatch(
                {ok, #{
                    where := {left_join, {triple, _, _, _}, {filter, _, _}, none}
                }},
                z_sparql_plan:to_query_plan(Query, Context)),

            {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
            ?assert(has_optional_term(Terms)),
            Sql = z_search_terms:combine(Terms, Context),
            ?assert(contains(Sql#search_sql.from, <<"left join LATERAL (SELECT">>)),
            ?assert(contains(Sql#search_sql.from, <<"props_json">>)),
            ?assert(contains(Sql#search_sql.from, <<" IS NOT NULL">>)),
            ?assertNot(contains(Sql#search_sql.where, <<"props_json">>))
        end).

filter_after_optional_uses_nullable_outer_binding_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/optional#>\n"
                "SELECT ?subject WHERE {\n"
                "    ?subject test:id ?id .\n"
                "    OPTIONAL { ?subject test:title ?title }\n"
                "    FILTER (BOUND(?title))\n"
                "}"
            >>),
            {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
            Sql = z_search_terms:combine(Terms, Context),
            ?assert(contains(Sql#search_sql.from, <<"left join LATERAL (SELECT">>)),
            ?assert(contains(Sql#search_sql.where, <<"sparql_optional_">>)),
            ?assert(contains(Sql#search_sql.where, <<" IS NOT NULL">>))
        end).

nested_optional_keeps_each_right_scope_isolated_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/optional#>\n"
                "SELECT ?subject ?title ?subtitle WHERE {\n"
                "    ?subject test:id ?id .\n"
                "    OPTIONAL {\n"
                "        ?subject test:title ?title .\n"
                "        OPTIONAL { ?subject test:subtitle ?subtitle }\n"
                "    }\n"
                "}"
            >>),
            {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
            Sql = z_search_terms:combine(Terms, Context),
            ?assertEqual(2, count(Sql#search_sql.from, <<"left join LATERAL (SELECT">>)),
            ?assertNot(contains(Sql#search_sql.where, <<"props_json">>))
        end).

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

observe_rdf_ns(#rdf_ns{ ns = <<"https://example.test/optional#">> }, _Context) ->
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
observe_sparql_mapping(#sparql_mapping{
        ns_prefix = <<"test">>,
        predicate = <<"subtitle">>
    }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"subtitle">>], text}};
observe_sparql_mapping(#sparql_mapping{}, _Context) ->
    undefined.

has_optional_term(Terms) when is_list(Terms) ->
    lists:any(fun has_optional_term/1, Terms);
has_optional_term(#search_sql_nested{ operator = {left_join, _Alias} }) ->
    true;
has_optional_term(#search_sql_nested{ terms = Terms }) ->
    has_optional_term(Terms);
has_optional_term(#search_sql_term{}) ->
    false.

contains(Text, Part) ->
    binary:match(Text, Part) =/= nomatch.

count(Text, Part) ->
    length(binary:matches(Text, Part)).
