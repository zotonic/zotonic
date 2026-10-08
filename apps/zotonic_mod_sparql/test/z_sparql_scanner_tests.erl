-module(z_sparql_scanner_tests).

-include_lib("eunit/include/eunit.hrl").

basic_query_test() ->
    Query = <<
        "PREFIX ex: <http://example.com/>\n"
        "SELECT ?subject WHERE { ?subject a ex:Thing . } LIMIT 10"
    >>,
    {ok, Tokens} = z_sparql_scanner:scan(query, Query),
    ?assertEqual(
        [
            prefix, pname_ns, iri_ref, select, var1, where, lbrace,
            var1, a, pname_ln, dot, rbrace, limit, integer
        ],
        token_types(Tokens)),
    ?assertEqual(<<"http://example.com/">>, token_value(iri_ref, Tokens)),
    ?assertEqual(<<"subject">>, token_value(var1, Tokens)),
    ?assertEqual({query, 2, 1}, token_position(select, Tokens)).

strings_and_language_tag_test() ->
    Query = <<"'short' \"line\\n\" '''long\ntext''' \"\"\"long 2\"\"\"@nl">>,
    {ok, Tokens} = z_sparql_scanner:scan(Query),
    ?assertEqual(
        [
            string_literal1,
            string_literal2,
            string_literal_long1,
            string_literal_long2,
            langtag
        ],
        token_types(Tokens)),
    ?assertEqual(
        [<<"short">>, <<"line\n">>, <<"long\ntext">>, <<"long 2">>, <<"nl">>],
        token_values(Tokens)).

names_test() ->
    Query = <<"?na\\u00EFve $x1 _:node. ex:local.name ex:escaped\\~name :empty">>,
    {ok, Tokens} = z_sparql_scanner:scan(Query),
    ?assertEqual(
        [var1, var2, blank_node_label, dot, pname_ln, pname_ln, pname_ln],
        token_types(Tokens)),
    ?assertEqual(
        [
            <<"na", 16#00EF/utf8, "ve">>, <<"x1">>, <<"node">>, <<".">>,
            <<"ex:local.name">>, <<"ex:escaped~name">>, <<":empty">>
        ],
        token_values(Tokens)).

numeric_literals_test() ->
    Query = <<"0 1.5 2e3 +4 +.5 +6e7 -8 -9.0 -1e2 true-2 a.">>,
    {ok, Tokens} = z_sparql_scanner:scan(Query),
    ?assertEqual(
        [
            integer, decimal, double,
            integer_positive, decimal_positive, double_positive,
            integer_negative, decimal_negative, double_negative,
            true, integer_negative, a, dot
        ],
        token_types(Tokens)).

operators_test() ->
    Query = <<"{}()[],;.+-*/!=< > <= >= && || ^ ^^ | ?">>,
    {ok, Tokens} = z_sparql_scanner:scan(Query),
    ?assertEqual(
        [
            lbrace, rbrace, nil, anon, comma, semicolon, dot,
            plus, minus, star, slash, ne, lt, gt, le, ge,
            andand, oror, hat, hat2, pipe, question
        ],
        token_types(Tokens)).

comments_and_empty_terms_test() ->
    Query = <<"SELECT # first\r\n (\t) [ # second\n ]">>,
    {ok, Tokens} = z_sparql_scanner:scan(source, Query),
    ?assertEqual([select, nil, anon], token_types(Tokens)),
    ?assertEqual({source, 2, 2}, token_position(nil, Tokens)),
    ?assertEqual({source, 2, 6}, token_position(anon, Tokens)).

codepoint_escapes_test() ->
    Query = <<
        "PREFIX \\u03B1: <http://example/\\U0001F600> "
        "SELECT ?\\u03B2 WHERE {}"
    >>,
    {ok, Tokens} = z_sparql_scanner:scan(Query),
    ?assertEqual(
        [prefix, pname_ns, iri_ref, select, var1, where, lbrace, rbrace],
        token_types(Tokens)),
    ?assertEqual(<<16#03B1/utf8, $:>>, token_value(pname_ns, Tokens)),
    ?assertEqual(<<"http://example/", 16#1F600/utf8>>, token_value(iri_ref, Tokens)),
    ?assertEqual(<<16#03B2/utf8>>, token_value(var1, Tokens)).

iri_longest_match_test() ->
    {ok, Tokens} = z_sparql_scanner:scan(<<$?, $a, $<, "?b&&?c", $>, $?, $d>>),
    ?assertEqual([var1, iri_ref, var1], token_types(Tokens)),
    ?assertEqual([<<"a">>, <<"?b&&?c">>, <<"d">>], token_values(Tokens)).

keywords_test() ->
    Query = <<
        "BASE PREFIX SELECT DISTINCT REDUCED AS CONSTRUCT WHERE DESCRIBE ASK "
        "FROM NAMED GROUP BY HAVING ORDER ASC DESC LIMIT OFFSET VALUES "
        "LOAD SILENT INTO CLEAR DROP CREATE ADD MOVE COPY TO INSERT DATA DELETE "
        "WITH USING DEFAULT GRAPH ALL OPTIONAL SERVICE BIND UNDEF MINUS UNION FILTER "
        "NOT EXISTS IN STR LANG LANGMATCHES DATATYPE BOUND IRI URI BNODE RAND ABS "
        "CEIL FLOOR ROUND CONCAT SUBSTR STRLEN REPLACE UCASE LCASE ENCODE_FOR_URI "
        "CONTAINS STRSTARTS STRENDS STRBEFORE STRAFTER YEAR MONTH DAY HOURS MINUTES "
        "SECONDS TIMEZONE TZ NOW UUID STRUUID MD5 SHA1 SHA256 SHA384 SHA512 COALESCE "
        "IF STRLANG STRDT sameTerm isIRI isURI isBLANK isLITERAL isNUMERIC REGEX "
        "COUNT SUM MIN MAX AVG SAMPLE GROUP_CONCAT SEPARATOR true FALSE a"
    >>,
    {ok, Tokens} = z_sparql_scanner:scan(Query),
    Types = token_types(Tokens),
    ?assert(lists:member(prefix, Types)),
    ?assert(lists:member(sameterm, Types)),
    ?assert(lists:member(group_concat, Types)),
    ?assertEqual(a, element(1, lists:last(Tokens))).

case_sensitive_a_test() ->
    ?assertMatch({error, <<_/binary>>}, z_sparql_scanner:scan(<<"A">>)).

invalid_string_escape_test() ->
    ?assertMatch({error, <<_/binary>>}, z_sparql_scanner:scan(<<"'bad\\x'">>)).

invalid_language_tag_test() ->
    ?assertMatch({error, <<_/binary>>}, z_sparql_scanner:scan(<<"'text'@en-">>)).

parser_integration_test() ->
    Query = <<"SELECT ?s WHERE { ?s a <http://example.com/Thing> }">>,
    {ok, Tokens} = z_sparql_scanner:scan(Query),
    ?assertMatch(
        {ok, {query, [], {select, default, [{var, <<"s">>}], [], {group, _}, _}}},
        z_sparql_parser:parse(Tokens)).

token_types(Tokens) ->
    [ Type || {Type, _Pos, _Value} <- Tokens ].

token_values(Tokens) ->
    [ Value || {_Type, _Pos, Value} <- Tokens ].

token_value(Type, Tokens) ->
    {Type, _Pos, Value} = lists:keyfind(Type, 1, Tokens),
    Value.

token_position(Type, Tokens) ->
    {Type, Pos, _Value} = lists:keyfind(Type, 1, Tokens),
    Pos.
