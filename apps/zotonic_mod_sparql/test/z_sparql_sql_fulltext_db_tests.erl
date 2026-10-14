-module(z_sparql_sql_fulltext_db_tests).
-moduledoc("Database-backed SPARQL full-text search tests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


fulltext_search_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    Token = z_string:to_name(<<"sparql_fulltext_", (z_ids:id(12))/binary>>),
    Text = <<"SPARQL fulltext ", Token/binary, " testfuzzymatch">>,
    {ok, RscId} = m_rsc:insert(#{
        <<"category">> => article,
        <<"title">> => Text,
        <<"is_published">> => true
    }, Context),
    try
        set_search_columns(RscId, Text, Context),
        ?assertEqual([RscId], search(default_match_query(Token), Context)),
        ?assertEqual([RscId], search(field_match_query(Token), Context)),
        % Check on a fuzzy match, one less 'z'
        ?assertEqual([RscId], search(field_match_query(<<"testfuzymatch">>), Context)),
        [{RscId, FtsRank}] = search(default_rank_query(Token), Context),
        ?assert(z_convert:to_float(FtsRank) > 0.0),
        [{RscId, TrigramRank}] = search(field_rank_query(Token), Context),
        ?assert(z_convert:to_float(TrigramRank) > 0.0)
    after
        ok = m_rsc:delete(RscId, Context)
    end.

set_search_columns(RscId, Text, Context) ->
    Stemmer = z_pivot_rsc:stemmer_language(Context),
    Normalized = z_search:normalize_value(<<"pivot_title">>, text, Text, Context),
    Sql = "update rsc "
          "set pivot_title = CAST($2 AS character varying), "
          "    pivot_tsv = to_tsvector('pg_catalog." ++ Stemmer ++ "', CAST($2 AS text)) "
          "where id = $1",
    1 = z_db:q(Sql, [RscId, Normalized], Context),
    ok.

default_match_query(Text) ->
    <<
        "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
        "SELECT ?resource WHERE {\n"
        "    FILTER(zotonic:fullText(?resource, \"", Text/binary, "\"))\n"
        "}"
    >>.

field_match_query(Text) ->
    <<
        "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
        "SELECT ?resource WHERE {\n"
        "    FILTER(zotonic:fullText(?resource, zotonic:pivot.title, \"",
            Text/binary, "\"))\n"
        "}"
    >>.

default_rank_query(Text) ->
    <<
        "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
        "SELECT ?resource (zotonic:fullTextRank(?resource, \"",
            Text/binary, "\") AS ?rank) WHERE {\n"
        "    FILTER(zotonic:fullText(?resource, \"", Text/binary, "\"))\n"
        "}"
    >>.

field_rank_query(Text) ->
    <<
        "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
        "SELECT ?resource (zotonic:fullTextRank("
            "?resource, zotonic:pivot.title, \"", Text/binary, "\") AS ?rank) WHERE {\n"
        "    FILTER(zotonic:fullText(?resource, zotonic:pivot.title, \"",
            Text/binary, "\"))\n"
        "}"
    >>.

search(Sparql, Context) ->
    {ok, #search_result{ result = Result }} = z_sparql:search(Sparql, {1, 20}, Context),
    Result.
