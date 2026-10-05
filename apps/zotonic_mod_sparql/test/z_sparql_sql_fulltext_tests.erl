-module(z_sparql_sql_fulltext_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").
-include_lib("zotonic_rdf/include/zotonic_rdf.hrl").

-export([
    observe_rdf_ns/2,
    observe_url_abs/2,
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

indexed_text_projection_test() ->
    with_observers(fun(Context) ->
        lists:foreach(fun(Field) ->
            lists:foreach(fun(Select) ->
                Text = <<"PREFIX test: <https://example.test/> "
                    "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT ", Select/binary,
                    " WHERE { ?resource test:", Field/binary, " ?text }">>,
                {ok, Query} = z_sparql:parse(Text),
                ?assertEqual({error, {not_selectable, {var, <<"text">>}}},
                    z_sparql_sql:to_sql_term(Query, Context))
            end, [<<"?text">>, <<"*">>, <<"(?text AS ?copy)">>,
                <<"(xsd:string(?text) AS ?copy)">>, <<"(SUBSTR(?text, 1, 3) AS ?copy)">>,
                <<"(GROUP_CONCAT(?text) AS ?copy)">>]),
            %% The same binding remains available for matching in WHERE.
            {ok, Match} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/> SELECT ?resource "
                "WHERE { ?resource test:", Field/binary, " ?text }">>),
            ?assertMatch({ok, _}, z_sparql_sql:to_sql_term(Match, Context)),
            {ok, Optional} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/> SELECT ?text "
                "WHERE { ?resource test:title ?title OPTIONAL { ?resource test:",
                Field/binary, " ?text } }">>),
            ?assertEqual({error, {not_selectable, {var, <<"text">>}}},
                z_sparql_sql:to_sql_term(Optional, Context))
        end, [<<"summary">>, <<"body">>, <<"pivot">>, <<"custom_pivot">>])
    end).

indexed_text_binding_provenance_test() ->
    with_observers(fun(Context) ->
        lists:foreach(fun(Pattern) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/> SELECT ?text WHERE { ",
                Pattern/binary, " }">>),
            ?assertEqual({error, {not_selectable, {var, <<"text">>}}},
                z_sparql_sql:to_sql_term(Query, Context))
        end, [
            <<"?resource test:summary ?text . ?resource test:title ?text">>,
            <<"?resource test:title ?text . ?resource test:summary ?text">>,
            <<"?resource test:summary ?text . VALUES ?text { UNDEF }">>,
            <<"VALUES ?text { UNDEF } { ?resource test:summary ?text }">>,
            <<"?resource test:title ?title OPTIONAL { ?resource test:summary ?text } "
              "OPTIONAL { ?resource test:title ?text }">>
        ])
    end).

indexed_text_match_projection_test() ->
    with_observers(fun(Context) ->
        {ok, Query} = z_sparql:parse(<<
            "PREFIX test: <https://example.test/> "
            "SELECT (zotonic:fullTextRank(?resource, test:custom_pivot, \"words\") AS ?rank) "
            "(EXISTS { ?resource test:summary ?text } AS ?found) "
            "WHERE { ?resource test:custom_pivot ?indexed }">>),
        ?assertMatch({ok, _}, z_sparql_sql:to_sql_term(Query, Context))
    end).

ordinary_text_projection_test() ->
    with_observers(fun(Context) ->
        {ok, Query} = z_sparql:parse(<<
            "PREFIX test: <https://example.test/> SELECT ?text "
            "WHERE { ?resource test:title ?text }">>),
        ?assertMatch({ok, _}, z_sparql_sql:to_sql_term(Query, Context))
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
    Context = z_acl:sudo(z_context:new(sparql_fulltext_fixture)),
    Dispatch = ets:new(z_utils:name_for_site(z_dispatcher, Context), [named_table, public]),
    ok = z_notifier:observe(url_abs, {?MODULE, observe_url_abs}, 100, Context),
    ok = z_notifier:observe(rdf_ns, {?MODULE, observe_rdf_ns}, 100, Context),
    ok = z_notifier:observe(sparql_mapping, {?MODULE, observe_sparql_mapping}, 100, Context),
    try
        F(Context)
    after
        ets:delete(Dispatch),
        z_notifier:detach(url_abs, Context),
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
observe_sparql_mapping(
        #sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"pivot">> },
        _Context) ->
    {ok, {column, <<"rsc">>, <<"pivot_tsv">>, fts}};
observe_sparql_mapping(
        #sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"custom_pivot">> },
        _Context) ->
    {ok, {column, <<"pivot_search">>, <<"text">>, text}};
observe_sparql_mapping(#sparql_mapping{}, _Context) ->
    undefined.

observe_url_abs(#url_abs{url = Url}, _Context) ->
    <<"https://example.test", Url/binary>>.
