-module(z_sparql_sql_datatype_tests).
-moduledoc("XSD datatype constructor to PostgreSQL mapping tests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").

-export([
    observe_rdf_ns/2,
    observe_sparql_mapping/2
]).

-define(NS_XSD, <<"http://www.w3.org/2001/XMLSchema#">>).


well_known_datatype_mapping_test() ->
    Mappings = [
        {<<"string">>, text, <<"text">>},
        {<<"boolean">>, boolean, <<"boolean">>},
        {<<"integer">>, integer, <<"bigint">>},
        {<<"long">>, integer, <<"bigint">>},
        {<<"int">>, integer, <<"integer">>},
        {<<"short">>, integer, <<"smallint">>},
        {<<"decimal">>, number, <<"numeric">>},
        {<<"float">>, float, <<"real">>},
        {<<"double">>, float, <<"double precision">>},
        {<<"dateTime">>, datetime, <<"timestamptz">>},
        {<<"dateTimeStamp">>, datetime, <<"timestamptz">>}
    ],
    lists:foreach(
        fun({Name, ValueType, SqlType}) ->
            ?assertEqual(
                {ok, {ValueType, SqlType}},
                z_sparql_sql_datatype:mapping(<<?NS_XSD/binary, Name/binary>>))
        end,
        Mappings),
    ?assertEqual(undefined, z_sparql_sql_datatype:mapping(<<"https://example.test/type">>)).

datatype_type_test() ->
    Types = [
        {<<"integer">>, integer},
        {<<"unsignedInt">>, integer},
        {<<"decimal">>, float},
        {<<"boolean">>, boolean},
        {<<"dateTime">>, datetime},
        {<<"date">>, datetime},
        {<<"anyURI">>, uri},
        {<<"string">>, text}
    ],
    lists:foreach(
        fun({Name, Type}) ->
            ?assertEqual(Type, z_sparql_sql_datatype:datatype_type(<<?NS_XSD/binary, Name/binary>>))
        end,
        Types),
    ?assertEqual(text, z_sparql_sql_datatype:datatype_type(undefined)),
    ?assertEqual(text, z_sparql_sql_datatype:datatype_type(<<"https://example.test/type">>)).

datetime_argument_uses_expanded_iri_test() ->
    with_observers(
        fun(Context) ->
            {ok, Terms} = sql_terms(<<
                "SELECT (<http://www.w3.org/2001/XMLSchema#dateTime>("
                    "\"2008-12-10T12:34:56Z\") AS ?datetime) "
                "WHERE { ?person <https://example.test/name> ?name . }"
            >>, Context),
            Select = terms_select(Terms),
            ?assertNotEqual(nomatch, binary:match(Select, <<"CAST(CAST($1 AS text) AS timestamptz) AS sparql_1">>)),
            [<<"2008-12-10T12:34:56Z">>] = lists:append([Args || #search_sql_term{ args = Args } <- Terms])
        end).

datetime_jsonb_constructor_test() ->
    with_observers(
        fun(Context) ->
            {ok, Terms} = sql_terms(<<
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> "
                "PREFIX test: <https://example.test/> "
                "SELECT ?person WHERE { "
                    "?person test:date_text ?date_text . "
                    "FILTER(xsd:dateTime(?date_text) = "
                        "\"2008-12-10T00:00:00Z\"^^xsd:dateTime) "
                "}"
            >>, Context),
            Where = terms_where(Terms),
            ?assertNotEqual(nomatch, binary:match(Where, <<"jsonb_typeof(">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"IN ('string')">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"#>> '{}'">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"AS timestamptz">>))
        end).

datetime_column_constructor_is_identity_test() ->
    with_observers(
        fun(Context) ->
            {ok, Terms} = sql_terms(<<
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> "
                "PREFIX test: <https://example.test/> "
                "SELECT (xsd:dateTime(?created) AS ?datetime) WHERE { "
                    "?person test:created ?created . "
                "}"
            >>, Context),
            Select = terms_select(Terms),
            ?assertNotEqual(nomatch, binary:match(Select, <<"rsc.created AS sparql_1">>)),
            ?assertEqual(nomatch, binary:match(Select, <<"timestamptz">>))
        end).

datetime_integer_constructor_test() ->
    with_observers(
        fun(Context) ->
            {ok, Terms} = sql_terms(<<
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> "
                "SELECT (xsd:dateTime(1228912496) AS ?datetime) "
                "WHERE { ?person <https://example.test/name> ?name . }"
            >>, Context),
            Select = terms_select(Terms),
            ?assertNotEqual(nomatch, binary:match(Select, <<"to_timestamp(CAST(CAST($1 AS bigint) AS double precision))">>)),
            [1228912496] = lists:append([Args || #search_sql_term{ args = Args } <- Terms])
        end).

datetime_jsonb_integer_constructor_test() ->
    with_observers(
        fun(Context) ->
            {ok, Terms} = sql_terms(<<
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> "
                "PREFIX test: <https://example.test/> "
                "SELECT ?person WHERE { "
                    "?person test:unix_seconds ?unix_seconds . "
                    "FILTER(xsd:dateTime(?unix_seconds) = "
                        "\"2008-12-10T12:34:56Z\"^^xsd:dateTime) "
                "}"
            >>, Context),
            Where = terms_where(Terms),
            ?assertNotEqual(nomatch, binary:match(Where, <<"jsonb_typeof(">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"= 'number'">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"to_timestamp(">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"AS double precision">>))
        end).

scalar_datatype_constructor_test() ->
    with_observers(
        fun(Context) ->
            {ok, Terms} = sql_terms(<<
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> "
                "SELECT "
                    "(xsd:string(\"Zotonic\") AS ?string) "
                    "(xsd:boolean(\"true\") AS ?boolean) "
                    "(xsd:integer(\"10\") AS ?integer) "
                    "(xsd:decimal(\"10.5\") AS ?decimal) "
                    "(xsd:double(\"10.5\") AS ?double) "
                "WHERE { ?person <https://example.test/name> ?name . }"
            >>, Context),
            Select = terms_select(Terms),
            ?assertNotEqual(nomatch, binary:match(Select, <<"$1 AS sparql_1">>)),
            ?assertEqual(nomatch, binary:match(Select, <<"CAST($1 AS text)">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"CAST(CAST($2 AS text) AS boolean)">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"CAST(CAST($3 AS text) AS bigint)">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"CAST(CAST($4 AS text) AS numeric)">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"CAST(CAST($5 AS text) AS double precision)">>))
        end).

datetime_constructor_invalid_arity_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> "
                "SELECT (xsd:dateTime() AS ?datetime) "
                "WHERE { ?person <https://example.test/name> ?name . }"
            >>),
            ?assertEqual(
                {error, {invalid_datatype_arity, <<"http://www.w3.org/2001/XMLSchema#dateTime">>, 0 }},
                z_sparql_sql:to_sql_term(Query, Context))
        end).

datetime_constructor_rejects_decimal_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> "
                "SELECT (xsd:dateTime(10.5) AS ?datetime) "
                "WHERE { ?person <https://example.test/name> ?name . }"
            >>),
            ?assertEqual(
                {error, {incompatible_types, datetime, float}},
                z_sparql_sql:to_sql_term(Query, Context))
        end).

datatype_metadata_projection_test() ->
    with_observers(fun(Context) ->
        Pattern = <<" WHERE { ?person <https://example.test/name> ?name . "
            "OPTIONAL { VALUES (?a ?b) { (\"one\"@en \"two\"@nl) } } }">>,
        lists:foreach(fun({Select, Suffix, Expected}) ->
            {ok, Terms} = sql_terms(<<Select/binary, Pattern/binary, Suffix/binary>>, Context),
            #search_sql{from = From} = z_search_terms:combine(Terms, Context),
            ?assertEqual(Expected, binary:match(From, <<"AS rdf_datatype_1">>) =/= nomatch),
            ?assertEqual(Expected, binary:match(From, <<"rdf_kind_">>) =/= nomatch),
            ?assertEqual(nomatch, binary:match(From, <<"rdf_language_">>)),
            % The unrelated second VALUES binding never needs metadata.
            ?assertEqual(nomatch, binary:match(From, <<"rdf_datatype_2">>))
        end, [
            {<<"SELECT ?a ?b">>, <<>>, false},
            {<<"SELECT *">>, <<>>, false},
            {<<"SELECT (DATATYPE(1) AS ?dt) ?a">>, <<>>, false},
            {<<"SELECT (DATATYPE(STRLEN(?a)) AS ?dt)">>, <<>>, false},
            {<<"SELECT (DATATYPE(?a) AS ?dt)">>, <<>>, true},
            {<<"SELECT (UCASE(?a) AS ?alias) (DATATYPE(?alias) AS ?dt)">>, <<>>, true},
            {<<"SELECT ?person">>, <<" ORDER BY DATATYPE(?a)">>, true}
        ])
    end).

datatype_filter_metadata_projection_test() ->
    with_observers(fun(Context) ->
        {ok, Terms} = sql_terms(<<"SELECT ?person WHERE { "
            "?person <https://example.test/name> ?name . "
            "OPTIONAL { VALUES ?a { \"one\"@en } } "
            "FILTER(DATATYPE(?a) = <http://www.w3.org/1999/02/22-rdf-syntax-ns#langString>) }">>, Context),
        #search_sql{from = From, where = Where} = z_search_terms:combine(Terms, Context),
        ?assertNotEqual(nomatch, binary:match(From, <<"AS rdf_datatype_1">>)),
        ?assertNotEqual(nomatch, binary:match(Where, <<"rdf_datatype_1">>))
    end).

metadata_scope_and_components_test() ->
    with_observers(fun(Context) ->
        Root = <<"?person <https://example.test/name> ?name . ">>,
        Local = <<"SELECT ?person WHERE { ", Root/binary,
            "OPTIONAL { VALUES ?a { 1 } "
            "FILTER(DATATYPE(?a) = <http://www.w3.org/2001/XMLSchema#integer>) } }">>,
        {ok, LocalTerms} = sql_terms(Local, Context),
        #search_sql{from = LocalFrom} = z_search_terms:combine(LocalTerms, Context),
        % VALUES metadata is used by the local filter, but is not exported.
        ?assertNotEqual(nomatch, binary:match(LocalFrom, <<"rdf_datatype_1">>)),
        ?assertEqual(nomatch, binary:match(LocalFrom, <<"AS rdf_datatype_1">>)),
        ?assertEqual(nomatch, binary:match(LocalFrom, <<"AS rdf_kind_1">>)),
        ?assertEqual(nomatch, binary:match(LocalFrom, <<"rdf_language_">>)),
        lists:foreach(fun(Function) ->
            Query = <<"SELECT (DATATYPE(", Function/binary, "(?a)) AS ?dt) WHERE { ",
                Root/binary, "OPTIONAL { VALUES ?a { 1 2 } } }">>,
            {ok, Terms} = sql_terms(Query, Context),
            #search_sql{from = From} = z_search_terms:combine(Terms, Context),
            ?assertEqual(nomatch, binary:match(From, <<"rdf_">>))
        end, [<<"MIN">>, <<"MAX">>, <<"SUM">>, <<"AVG">>]),
        Concat = <<"SELECT (DATATYPE(CONCAT(?a, ?b)) AS ?dt) WHERE { ", Root/binary,
            "OPTIONAL { VALUES (?a ?b) { (\"one\"@en \"two\"@en) } } }">>,
        {ok, ConcatTerms} = sql_terms(Concat, Context),
        #search_sql{from = ConcatFrom} = z_search_terms:combine(ConcatTerms, Context),
        ?assertNotEqual(nomatch, binary:match(ConcatFrom, <<"AS rdf_language_1">>)),
        ?assertNotEqual(nomatch, binary:match(ConcatFrom, <<"AS rdf_language_2">>)),
        ?assertEqual(nomatch, binary:match(ConcatFrom, <<"rdf_kind_2">>)),
        ?assertEqual(nomatch, binary:match(ConcatFrom, <<"rdf_datatype_2">>))
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

% The SQL fragments contain parameter names as atoms such as '$1'.
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

observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"name">> }, _Context) ->
    {ok, {column, <<"rsc">>, <<"name">>, text}};
observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"created">> }, _Context) ->
    {ok, {column, <<"rsc">>, <<"created">>, datetime}};
observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"date_text">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"date_text">>], text}};
observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"unix_seconds">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"unix_seconds">>], integer}};
observe_sparql_mapping(#sparql_mapping{}, _Context) ->
    undefined.
