-module(z_sparql_sql_function_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").

-export([
    observe_rdf_ns/2,
    observe_sparql_mapping/2
]).

% Simple mapping test for the straightforward functions
postgresql_mapping_test_() ->
    [
        mapping(bound, [<<"value">>], <<"(value IS NOT NULL)">>),
        mapping(abs, [<<"value">>], <<"abs(value)">>),
        mapping(ceil, [<<"value">>], <<"ceil(value)">>),
        mapping(floor, [<<"value">>], <<"floor(value)">>),
        mapping(round, [<<"value">>], <<"round(value)">>),
        mapping(concat, [<<"a">>, <<"b">>], <<"concat(a, b)">>),
        mapping(concat, [], <<"CAST('' AS text)">>),
        mapping(substr, [<<"value">>, <<"2">>], <<"substr(value, 2)">>),
        mapping(substr, [<<"value">>, <<"2">>, <<"3">>], <<"substr(value, 2, 3)">>),
        mapping(strlen, [<<"value">>], <<"char_length(value)">>),
        mapping(ucase, [<<"value">>], <<"upper(value)">>),
        mapping(lcase, [<<"value">>], <<"lower(value)">>),
        mapping(contains, [<<"value">>, <<"part">>], <<"(strpos(value, part) > 0)">>),
        mapping(strstarts, [<<"value">>, <<"part">>], <<"starts_with(value, part)">>),
        mapping(strends, [<<"value">>, <<"part">>], <<"starts_with(reverse(value), reverse(part))">>),
        % These two need extra code, as there is no 1-to-1 mapping
        mapping(strbefore, [<<"value">>, <<"delimiter">>],
            <<"(CASE WHEN strpos(value, delimiter) = 0 THEN '' "
              "ELSE left(value, strpos(value, delimiter) - 1) END)">>),
        mapping(strafter, [<<"value">>, <<"delimiter">>],
            <<"(CASE WHEN strpos(value, delimiter) = 0 THEN '' "
              "ELSE substr(value, strpos(value, delimiter) + char_length(delimiter)) END)">>),
        mapping(year, [<<"value">>], <<"EXTRACT(YEAR FROM value)">>),
        mapping(month, [<<"value">>], <<"EXTRACT(MONTH FROM value)">>),
        mapping(day, [<<"value">>], <<"EXTRACT(DAY FROM value)">>),
        mapping(hours, [<<"value">>], <<"EXTRACT(HOUR FROM value)">>),
        mapping(minutes, [<<"value">>], <<"EXTRACT(MINUTE FROM value)">>),
        mapping(seconds, [<<"value">>], <<"EXTRACT(SECOND FROM value)">>),
        mapping(now, [], <<"CURRENT_TIMESTAMP">>),
        mapping(rand, [], <<"random()">>),
        mapping(uuid, [], <<"concat('urn:uuid:', CAST(gen_random_uuid() AS text))">>),
        mapping(struuid, [], <<"CAST(gen_random_uuid() AS text)">>),
        mapping(md5, [<<"value">>], <<"md5(value)">>),
        mapping(coalesce, [<<"a">>, <<"b">>], <<"coalesce(a, b)">>),
        mapping('if', [<<"condition">>, <<"a">>, <<"b">>], <<"(CASE WHEN condition THEN a ELSE b END)">>),
        mapping(iri, [<<"value">>], <<"value">>),
        mapping(uri, [<<"value">>], <<"value">>),
        % Following are now handled by the SQL generator
        % mapping(isliteral, [<<"value">>], <<"(jsonb_typeof(to_jsonb(value)) IN ('string', 'number', 'boolean'))">>),
        % mapping(isnumeric, [<<"value">>], <<"(jsonb_typeof(to_jsonb(value)) = 'number')">>),
        % mapping(sameterm, [<<"a">>, <<"b">>],
        %     <<"(jsonb_typeof(to_jsonb(a)) IN ('string', 'number', 'boolean') "
        %       "AND jsonb_typeof(to_jsonb(a)) = jsonb_typeof(to_jsonb(b)) "
        %       "AND (to_jsonb(a))::text = (to_jsonb(b))::text)">>),
        mapping(regex, [<<"value">>, <<"pattern">>], <<"(value ~ pattern)">>),
        mapping(regex, [<<"value">>, <<"pattern">>, <<"flags">>], <<"(value ~ concat('(?', flags, ')', pattern))">>),
        % The replace function needs to adapt the regexp from XPATH to POSIX
        mapping(replace, [<<"value">>, <<"pattern">>, <<"replacement">>],
            <<"regexp_replace(value, pattern, replace(replace(replacement, '$0', chr(92) || '&'), '$', chr(92)), 'g')">>),
        mapping(replace, [<<"value">>, <<"pattern">>, <<"replacement">>, <<"flags">>],
            <<"regexp_replace(value, pattern, replace(replace(replacement, '$0', chr(92) || '&'), '$', chr(92)), ",
              "concat(flags, 'g'))">>)
    ].

invalid_arity_test() ->
    ?assertEqual(
        {error, {invalid_function_arity, abs, 2}},
        z_sparql_sql_function:to_sql(abs, [<<"a">>, <<"b">>])).

% The lang funtion is now unsupported, but should be added when implementing the trans support
unsupported_function_test() ->
    ?assertEqual(
        {error, {unsupported_function, lang}},
        z_sparql_sql_function:to_sql(lang, [<<"value">>])).

type_signature_test() ->
    ?assertEqual(
        {ok, {[number], number}},
        z_sparql_sql_function:type_signature(abs, 1)),
    ?assertEqual(
        {ok, {[text, integer, integer], text}},
        z_sparql_sql_function:type_signature(substr, 3)),
    ?assertEqual(
        {ok, {[datetime], integer}},
        z_sparql_sql_function:type_signature(year, 1)),
    ?assertEqual(
        {ok, {[boolean, common, common], common}},
        z_sparql_sql_function:type_signature('if', 3)),
    ?assertEqual(
        {ok, {[], uri}},
        z_sparql_sql_function:type_signature(uuid, 0)),
    ?assertEqual(
        {ok, {[any], boolean}},
        z_sparql_sql_function:type_signature(isiri, 1)),
    ?assertEqual(
        {ok, {[uri], uri}},
        z_sparql_sql_function:type_signature(iri, 1)),
    ?assertEqual(
        {error, {invalid_function_arity, substr, 1}},
        z_sparql_sql_function:type_signature(substr, 1)).

parse_function_test() ->
    {ok, {query, [], {select, default, _, [], {group, Patterns}, _}}} =
        z_sparql:parse(<<
            "SELECT ?s WHERE { "
            "?s <https://example.test/name> ?name . "
            "FILTER(CONTAINS(UCASE(?name), \"ZOT\")) "
            "}"
        >>),
    ?assertMatch(
        [
            {triple_pattern, _},
            {filter, {call, contains, [{call, ucase, [{var, <<"name">>}]}, {literal, <<"ZOT">>}]}}
        ],
        Patterns).

function_to_sql_term_test() ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_notifier:observe(rdf_ns, {?MODULE, observe_rdf_ns}, 100, Context),
    ok = z_notifier:observe(sparql_mapping, {?MODULE, observe_sparql_mapping}, 100, Context),
    try
        {ok, Query} = z_sparql:parse(<<
            "PREFIX test: <https://example.test/> "
            "SELECT ?name WHERE { "
            "?person test:name ?name . "
            "FILTER(CONTAINS(UCASE(?name), \"ZOT\")) "
            "}"
        >>),
        {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
        [FilterTerm] = [ Term || #search_sql_term{ args = [<<"ZOT">>] } = Term <- Terms ],
        ?assertEqual(
            <<"(strpos(upper(rsc.name), $1) > 0)">>,
            sql_binary(FilterTerm#search_sql_term.where))
    after
        z_notifier:detach(rdf_ns, Context),
        z_notifier:detach(sparql_mapping, Context)
    end.

iri_constructor_test() ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_notifier:observe(rdf_ns, {?MODULE, observe_rdf_ns}, 100, Context),
    ok = z_notifier:observe(sparql_mapping, {?MODULE, observe_sparql_mapping}, 100, Context),
    try
        {ok, Query} = z_sparql:parse(<<
            "BASE <https://example.test/base/> "
            "PREFIX test: <https://example.test/> "
            "SELECT ?person (IRI(\"child\") AS ?relative) "
                "(URI(<https://example.test/absolute>) AS ?absolute) WHERE { "
                "?person test:name ?name "
            "}"
        >>),
        {ok, Terms} = z_sparql_sql:to_sql_term(Query, Context),
        Args = lists:append([ Term#search_sql_term.args || Term <- Terms ]),
        ?assertEqual(
            [<<"https://example.test/base/child">>, <<"https://example.test/absolute">>],
            Args),
        {ok, DynamicQuery} = z_sparql:parse(<<
            "PREFIX test: <https://example.test/> "
            "SELECT ?person (IRI(?name) AS ?iri) (URI(?person) AS ?resource_iri) WHERE { "
                "?person test:name ?name . "
                "FILTER(isIRI(IRI(?person))) "
            "}"
        >>),
        ?assertMatch(
            {ok, [_ | _]},
            z_sparql_sql:to_sql_term(DynamicQuery, Context))
    after
        z_notifier:detach(rdf_ns, Context),
        z_notifier:detach(sparql_mapping, Context)
    end.

mapping(Function, Arguments, Expected) ->
    ?_assertEqual(
        Expected,
        begin
            {ok, Sql} = z_sparql_sql_function:to_sql(Function, Arguments),
            iolist_to_binary(Sql)
        end).

sql_binary(Sql) ->
    iolist_to_binary(sql_iolist(Sql)).

% The SQL where-clause is a nested list with binaries and atoms like '$1'
sql_iolist(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
sql_iolist(Value) when is_list(Value) ->
    [ sql_iolist(Part) || Part <- Value ];
sql_iolist(Value) ->
    Value.

observe_rdf_ns(#rdf_ns{ ns = <<"https://example.test/">> }, _Context) ->
    {ok, <<"test">>};
observe_rdf_ns(#rdf_ns{}, _Context) ->
    undefined.

observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"name">> }, _Context) ->
    {ok, {column, <<"rsc">>, <<"name">>, text}};
observe_sparql_mapping(#sparql_mapping{}, _Context) ->
    undefined.
