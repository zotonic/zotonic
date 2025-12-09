-module(z_sparql_sql_fulltext_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").
-include_lib("zotonic_rdf/include/zotonic_rdf.hrl").

-export([
    observe_rdf_ns/2,
    observe_sparql_mapping/2
]).


fulltext_plan_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX zotonic: <http://zotonic.net/predicate/> "
                "PREFIX test: <https://example.test/> "
                "SELECT ?resource WHERE { "
                "FILTER(zotonic:fullText(?resource, test:body, \"SPARQL search\")) "
                "}"
            >>),
            {ok, #{ root := {var, <<"resource">>}, where := Where }} =
                z_sparql_plan:to_query_plan(Query, Context),
            ?assertMatch(
                {filter,
                    {call, fulltext, [
                        {var, <<"resource">>},
                        #{
                            mapping := {search_column,
                                <<"search_facet">>, <<"f_body">>, <<"fts_body">>, fts}
                        },
                        {literal, <<"SPARQL search">>, undefined, undefined}
                    ]},
                    identity},
                Where)
        end).

trigram_match_and_rank_sql_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX zotonic: <http://zotonic.net/predicate/> "
                "PREFIX test: <https://example.test/> "
                "SELECT ?resource "
                "       (zotonic:fullTextRank(?resource, test:title, \"SPARQL Search\") AS ?rank) "
                "WHERE { "
                "FILTER(zotonic:fullText(?resource, test:title, \"SPARQL Search\")) "
                "} "
                "ORDER BY DESC(?rank)"
            >>),
            {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
            [MatchTerm] = [
                Term
                || #search_sql_term{ args = [<<"sparql search">>] } = Term <- Terms,
                   Term#search_sql_term.where =/= []
            ],
            ?assertEqual(
                <<"($1 OPERATOR(public.<%) rsc.pivot_title)">>,
                sql_binary(MatchTerm#search_sql_term.where)),
            [RankTerm] = [
                Term
                || #search_sql_term{ args = [<<"sparql search">>], select = [_] } = Term <- Terms
            ],
            ?assertMatch(
                <<"public.word_similarity($1, rsc.pivot_title) AS sparql_1">>,
                sql_binary(RankTerm#search_sql_term.select))
        end).

facet_trigram_search_column_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX zotonic: <http://zotonic.net/predicate/> "
                "PREFIX test: <https://example.test/> "
                "SELECT ?resource WHERE { "
                "FILTER(zotonic:fullText(?resource, test:summary, \"Find words\")) "
                "}"
            >>),
            {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
            [MatchTerm] = [
                Term
                || #search_sql_term{ args = [<<"find words">>] } = Term <- Terms
            ],
            ?assertMatch(
                [{<<"search_facet">>, _On}],
                maps:values(MatchTerm#search_sql_term.join_inner)),
            ?assertNotEqual(
                nomatch,
                binary:match(
                    sql_binary(MatchTerm#search_sql_term.where),
                    <<"$1 OPERATOR(public.<%) ">>)),
            ?assertNotEqual(
                nomatch,
                binary:match(sql_binary(MatchTerm#search_sql_term.where), <<".ft_summary">>))
        end).

facet_rdf_value_column_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/> "
                "SELECT ?resource ?summary WHERE { "
                "?resource test:summary ?summary "
                "}"
            >>),
            {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
            [ValueTerm] = [
                Term
                || #search_sql_term{ join_inner = Joins } = Term <- Terms,
                   map_size(Joins) =:= 1
            ],
            Where = sql_binary(ValueTerm#search_sql_term.where),
            ?assertNotEqual(nomatch, binary:match(Where, <<".f_summary IS NOT NULL">>)),
            ?assertEqual(nomatch, binary:match(Where, <<".ft_summary">>))
        end).

default_fulltext_plan_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX zotonic: <http://zotonic.net/predicate/> "
                "SELECT ?resource WHERE { "
                "FILTER(zotonic:fullText(?resource, \"find me\")) "
                "}"
            >>),
            {ok, #{ root := {var, <<"resource">>}, where := Where }} =
                z_sparql_plan:to_query_plan(Query, Context),
            ?assertMatch(
                {filter,
                    {call, fulltext, [
                        {var, <<"resource">>},
                        {literal, <<"find me">>, undefined, undefined}
                    ]},
                    identity},
                Where)
        end).


with_observers(F) ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_context:new(zotonic_site_testsandbox),
    ok = z_notifier:observe(rdf_ns, {?MODULE, observe_rdf_ns}, 100, Context),
    ok = z_notifier:observe(sparql_mapping, {?MODULE, observe_sparql_mapping}, 100, Context),
    try
        F(Context)
    after
        z_notifier:detach(rdf_ns, Context),
        z_notifier:detach(sparql_mapping, Context)
    end.

sql_binary(Sql) ->
    iolist_to_binary(sql_iolist(Sql)).

sql_iolist(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
sql_iolist(Value) when is_list(Value) ->
    [sql_iolist(Part) || Part <- Value];
sql_iolist(Value) ->
    Value.

observe_rdf_ns(#rdf_ns{ ns = <<"https://example.test/">> }, _Context) ->
    {ok, <<"test">>};
observe_rdf_ns(#rdf_ns{}, _Context) ->
    undefined.

observe_sparql_mapping(
        #sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"body">> },
        _Context) ->
    {ok, {search_column,
        <<"search_facet">>, <<"f_body">>, <<"fts_body">>, fts}};
observe_sparql_mapping(
        #sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"title">> },
        _Context) ->
    {ok, {column, <<"rsc">>, <<"pivot_title">>, text}};
observe_sparql_mapping(
        #sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"summary">> },
        _Context) ->
    {ok, {search_column,
        <<"search_facet">>, <<"f_summary">>, <<"ft_summary">>, fulltext}};
observe_sparql_mapping(#sparql_mapping{}, _Context) ->
    undefined.
