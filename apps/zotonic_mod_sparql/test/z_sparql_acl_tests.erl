-module(z_sparql_acl_tests).
-moduledoc("SPARQL SQL access-control tests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").

-export([
    observe_acl_add_sql_check/2,
    observe_rdf_ns/2,
    observe_sparql_mapping/2
]).


root_resource_acl_test() ->
    with_observers(
        fun(Context) ->
            Query0 = sparql_sql(<<
                "PREFIX test: <https://example.test/vocab#>\n"
                "SELECT ?subject WHERE { ?subject test:id ?id }"
            >>, Context),
            ?assertEqual([], acl_aliases(Query0#search_sql.args)),

            Query1 = z_search_acl:reformat_sql_query(Query0, #{}, Context),
            ?assert(contains(Query1#search_sql.where, <<"rsc.acl_marker = $1">>)),
            ?assertEqual([<<"rsc">>], acl_aliases(Query1#search_sql.args))
        end).

union_resource_acl_test() ->
    with_observers(
        fun(Context) ->
            Query0 = sparql_sql(<<
                "PREFIX test: <https://example.test/vocab#>\n"
                "SELECT ?subject WHERE {\n"
                "    ?subject test:id ?subject_id .\n"
                "    { ?object_a test:id ?id_a }\n"
                "    UNION\n"
                "    { ?object_b test:id ?id_b }\n"
                "}"
            >>, Context),
            LocalAliases = acl_aliases(Query0#search_sql.args),
            ?assertEqual(2, length(LocalAliases)),
            ?assertNot(lists:member(<<"rsc">>, LocalAliases)),
            ?assertEqual(2, count(Query0#search_sql.where, <<"EXISTS (">>)),
            lists:foreach(
                fun(Alias) ->
                    ?assert(contains(
                        Query0#search_sql.where,
                        <<Alias/binary, ".acl_marker = $">>))
                end,
                LocalAliases),

            Query1 = z_search_acl:reformat_sql_query(Query0, #{}, Context),
            ?assertEqual(
                lists:sort([<<"rsc">> | LocalAliases]),
                lists:sort(acl_aliases(Query1#search_sql.args))),
            ?assert(contains(Query1#search_sql.where, <<"rsc.acl_marker = $">>))
        end).

optional_resource_acl_stays_inside_lateral_scope_test() ->
    with_observers(
        fun(Context) ->
            Query0 = sparql_sql(<<
                "PREFIX test: <https://example.test/vocab#>\n"
                "SELECT ?subject ?object WHERE {\n"
                "    ?subject test:id ?subject_id .\n"
                "    OPTIONAL { ?object test:id ?object_id }\n"
                "}"
            >>, Context),
            [LocalAlias] = acl_aliases(Query0#search_sql.args),
            ?assert(LocalAlias =/= <<"rsc">>),
            ?assert(contains(Query0#search_sql.from, <<"left join LATERAL (SELECT">>)),
            ?assert(contains(
                Query0#search_sql.from,
                <<LocalAlias/binary, ".acl_marker = $1">>)),
            ?assertNot(contains(Query0#search_sql.where, <<"acl_marker">>)),

            Query1 = z_search_acl:reformat_sql_query(Query0, #{}, Context),
            ?assertEqual(
                lists:sort([<<"rsc">>, LocalAlias]),
                lists:sort(acl_aliases(Query1#search_sql.args))),
            ?assert(contains(Query1#search_sql.where, <<"rsc.acl_marker = $2">>))
        end).

sparql_sql(Sparql, Context) ->
    {ok, ParsedQuery} = z_sparql:parse(Sparql),
    {ok, Terms} = z_sparql_sql:to_sql_term(ParsedQuery, Context),
    z_search_terms:combine(Terms, Context).

with_observers(Fun) ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_context:new(zotonic_site_testsandbox),
    ok = z_notifier:observe(rdf_ns, {?MODULE, observe_rdf_ns}, 100, Context),
    ok = z_notifier:observe(sparql_mapping, {?MODULE, observe_sparql_mapping}, 100, Context),
    ok = z_notifier:observe(acl_add_sql_check, {?MODULE, observe_acl_add_sql_check}, 100, Context),
    try
        Fun(Context)
    after
        z_notifier:detach(rdf_ns, Context),
        z_notifier:detach(sparql_mapping, Context),
        z_notifier:detach(acl_add_sql_check, Context)
    end.

observe_rdf_ns(#rdf_ns{ ns = <<"https://example.test/vocab#">> }, _Context) ->
    {ok, <<"test">>};
observe_rdf_ns(#rdf_ns{}, _Context) ->
    undefined.

observe_sparql_mapping(#sparql_mapping{
        ns_prefix = <<"test">>,
        predicate = <<"id">>
    }, _Context) ->
    {ok, {column, <<"rsc">>, <<"id">>, id}};
observe_sparql_mapping(#sparql_mapping{}, _Context) ->
    undefined.

observe_acl_add_sql_check(#acl_add_sql_check{ alias = Alias, args = Args }, _Context) ->
    Nr = length(Args) + 1,
    {
        [Alias, <<".acl_marker = $">>, integer_to_binary(Nr)],
        Args ++ [{acl, Alias}]
    }.

acl_aliases(Args) ->
    [ Alias || {acl, Alias} <- Args ].

contains(Text, Part) ->
    binary:match(Text, Part) =/= nomatch.

count(Text, Part) ->
    length(binary:matches(Text, Part)).
