-module(z_sparql_sql_type_tests).
-moduledoc("Type-directed SPARQL SQL expression tests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").

-export([
    observe_rdf_ns/2,
    observe_sparql_mapping/2
]).


jsonb_expression_types_test() ->
    with_observers(
        fun(Context) ->
            {ok, Terms} = sql_terms(<<
                "PREFIX test: <https://example.test/> "
                "SELECT ?count ?name ?date ?enabled WHERE { "
                    "?person test:count ?count . "
                    "?person test:name ?name . "
                    "?person test:date ?date . "
                    "?person test:enabled ?enabled . "
                    "FILTER(?count + 1 > 10) "
                    "FILTER(CONTAINS(?name, \"Zot\")) "
                    "FILTER(YEAR(?date) = 2025) "
                    "FILTER(?enabled && true) "
                "}"
            >>, Context),
            Where = terms_where(Terms),
            ?assertNotEqual(nomatch, binary:match(Where, <<")::bigint">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"#>> '{}'">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"::timestamptz">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<")::boolean">>)),
            Select = terms_select(Terms),
            ?assertNotEqual(nomatch, binary:match(Select, <<" #> ARRAY[">>)),
            ?assertEqual(nomatch, binary:match(Select, <<"#>>">>)),
            ?assertEqual(nomatch, binary:match(Select, <<"::bigint">>))
        end).

column_expression_types_test() ->
    with_observers(
        fun(Context) ->
            {ok, Terms} = sql_terms(<<
                "PREFIX test: <https://example.test/> "
                "SELECT ?version WHERE { "
                    "?person test:version ?version . "
                    "?person test:created ?created . "
                    "FILTER(?version + 1 > 1) "
                    "FILTER(YEAR(?created) = 2025) "
                "}"
            >>, Context),
            Where = terms_where(Terms),
            ?assertNotEqual(nomatch, binary:match(Where, <<"rsc.version + $1">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"EXTRACT(YEAR FROM rsc.created)">>)),
            ?assertEqual(nomatch, binary:match(Where, <<"::bigint">>)),
            ?assertEqual(nomatch, binary:match(Where, <<"::timestamptz">>))
        end).

nested_expression_keeps_numeric_type_test() ->
    with_observers(
        fun(Context) ->
            {ok, Terms} = sql_terms(<<
                "PREFIX test: <https://example.test/> "
                "SELECT ?person WHERE { "
                    "?person test:count ?count . "
                    "FILTER(ABS(?count + 1) > 10) "
                "}"
            >>, Context),
            Where = terms_where(Terms),
            ?assertNotEqual(nomatch, binary:match(Where, <<"abs(">>)),
            ?assertEqual(1, length(binary:matches(Where, <<"::bigint">>)))
        end).

incompatible_expression_type_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/> "
                "SELECT ?person WHERE { "
                    "?person test:name ?name . "
                    "FILTER(?name + 1 > 10) "
                "}"
            >>),
            ?assertEqual(
                {error, {incompatible_types, number, text}},
                z_sparql_sql:to_sql_term(Query, Context))
        end).

type_test_storage_optimization_test() ->
    with_observers(
        fun(Context) ->
            {ok, Terms} = sql_terms(<<
                "PREFIX test: <https://example.test/> "
                "SELECT ?person WHERE { "
                    "?person test:name ?name . "
                    "?person test:count ?count . "
                    "?person test:version ?version . "
                    "FILTER(isLiteral(?name)) "
                    "FILTER(isNumeric(?count)) "
                    "FILTER(isLiteral(?version)) "
                    "FILTER(isNumeric(?version)) "
                "}"
            >>, Context),
            Where = terms_where(Terms),
            ?assertNotEqual(nomatch, binary:match(Where, <<"jsonb_typeof(rsc.props_json">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"IN ('string', 'number', 'boolean')">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"= 'number'">>)),
            ?assertEqual(2, length(binary:matches(Where, <<"true">>))),
            ?assertEqual(nomatch, binary:match(Where, <<"to_jsonb(">>))
        end).

same_term_storage_optimization_test() ->
    with_observers(
        fun(Context) ->
            {ok, Terms} = sql_terms(<<
                "PREFIX test: <https://example.test/> "
                "SELECT ?person WHERE { "
                    "?person test:name ?name . "
                    "?person test:count ?count . "
                    "?person test:version ?version . "
                    "FILTER(sameTerm(?name, ?name)) "
                    "FILTER(sameTerm(?version, ?version)) "
                    "FILTER(sameTerm(?count, ?version)) "
                    "FILTER(sameTerm(?name, ?version)) "
                "}"
            >>, Context),
            Where = terms_where(Terms),
            ?assertNotEqual(nomatch, binary:match(Where, <<"jsonb_typeof(rsc.props_json">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"(rsc.version = rsc.version)">>)),
            ?assertEqual(1, length(binary:matches(Where, <<"to_jsonb(">>))),
            ?assertEqual(1, length(binary:matches(Where, <<"false">>)))
        end).

sql_terms(Sparql, Context) ->
    {ok, Query} = z_sparql:parse(Sparql),
    z_sparql_sql:to_sql_term(Query, Context).

terms_where(Terms) ->
    sql_binary([ Where || #search_sql_term{ where = Where } <- Terms ]).

terms_select(Terms) ->
    sql_binary([ Select || #search_sql_term{ select = Select } <- Terms ]).

sql_binary(Sql) ->
    iolist_to_binary(sql_iolist(Sql)).

% The SQL where-clause is a nested list with binaries and atoms like '$1'
sql_iolist(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
sql_iolist(Value) when is_list(Value) ->
    [ sql_iolist(Part) || Part <- Value ];
sql_iolist(Value) ->
    Value.

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

observe_rdf_ns(#rdf_ns{ ns = <<"https://example.test/">> }, _Context) ->
    {ok, <<"test">>};
observe_rdf_ns(#rdf_ns{}, _Context) ->
    undefined.

observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"count">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"count">>], integer}};
observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"name">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"name">>], text}};
observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"date">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"date">>], datetime}};
observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"enabled">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"enabled">>], boolean}};
observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"version">> }, _Context) ->
    {ok, {column, <<"rsc">>, <<"version">>, integer}};
observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"created">> }, _Context) ->
    {ok, {column, <<"rsc">>, <<"created">>, datetime}};
observe_sparql_mapping(#sparql_mapping{}, _Context) ->
    undefined.
