-module(z_sparql_sql_metadata_db_tests).
-moduledoc("Evaluate RDF metadata SQL against PostgreSQL without changing site data.").
-include_lib("eunit/include/eunit.hrl").

jsonb_metadata_test() ->
    Context = context(),
    lists:foreach(fun({Json, Hint, Expected}) ->
        Metadata = z_sparql_sql_metadata:jsonb(<<"input.value">>, Hint),
        Actual = evaluate(Metadata,
            <<" FROM (SELECT $1::text::jsonb AS value) input">>, [Json], Context),
        ?assertEqual({Json, Hint, [Expected]}, {Json, Hint, Actual})
    end, [
        {<<"42">>, text, {<<"literal">>, xsd(<<"decimal">>), undefined}},
        {<<"42">>, integer, {<<"literal">>, xsd(<<"integer">>), undefined}},
        {<<"42.5">>, integer, {<<"literal">>, xsd(<<"decimal">>), undefined}},
        {<<"true">>, text, {<<"literal">>, xsd(<<"boolean">>), undefined}},
        {<<"\"42\"">>, integer, {<<"literal">>, xsd(<<"string">>), undefined}},
        {<<"\"2026-01-01T00:00:00Z\"">>, datetime, {<<"literal">>, xsd(<<"dateTime">>), undefined}},
        {<<"\"https://example.test/\"">>, uri, {<<"iri">>, undefined, undefined}},
        {<<"{}">>, text, {<<"bnode">>, undefined, undefined}},
        {<<"{\"_type\":\"trans\",\"tr\":{\"en\":\"hello\"}}">>, text, {undefined, undefined, undefined}},
        {<<"[]">>, text, {undefined, undefined, undefined}},
        {<<"null">>, text, {undefined, undefined, undefined}}
    ]).

branch_metadata_test() ->
    Left = z_sparql_sql_metadata:literal(undefined, <<"EN">>),
    Right = z_sparql_sql_metadata:literal(undefined, <<"nl">>),
    Metadata = z_sparql_sql_metadata:choose(<<"input.flag">>, Left, Right),
    ?assertEqual([
        {<<"literal">>, lang_string(), <<"en">>},
        {<<"literal">>, lang_string(), <<"nl">>}
    ], evaluate(Metadata, <<" FROM (VALUES (true), (false)) input(flag) ORDER BY flag DESC">>, [], context())).

numeric_metadata_test() ->
    Context = context(),
    Dynamic = (z_sparql_sql_metadata:unknown())#{datatype => [<<"input.datatype">>]},
    lists:foreach(fun({Operator, LeftType, RightType, Expected}) ->
        Right = z_sparql_sql_metadata:literal(RightType, undefined),
        Metadata = z_sparql_sql_metadata:numeric(Operator, Dynamic, Right),
        ExpectedKind = case Expected of undefined -> undefined; _ -> <<"literal">> end,
        ?assertEqual([{ExpectedKind, Expected, undefined}], evaluate(Metadata,
            <<" FROM (SELECT $1::text AS datatype) input">>, [LeftType], Context))
    end, [
        {'+', xsd(<<"int">>), xsd(<<"short">>), xsd(<<"integer">>)},
        {'/', xsd(<<"integer">>), xsd(<<"integer">>), xsd(<<"decimal">>)},
        {'+', xsd(<<"decimal">>), xsd(<<"float">>), xsd(<<"float">>)},
        {'+', xsd(<<"float">>), xsd(<<"double">>), xsd(<<"double">>)},
        {'+', <<"https://example.test/custom">>, xsd(<<"integer">>), undefined}
    ]).

context() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    z_acl:sudo(z_context:new(zotonic_site_testsandbox)).

evaluate(Metadata, From, Args, Context) ->
    Sql = [<<"SELECT ">>, lists:join(<<", ">>,
        [maps:get(Key, Metadata) || Key <- [kind, datatype, language]]), From],
    z_db:q(Sql, Args, Context).

xsd(Name) -> <<"http://www.w3.org/2001/XMLSchema#", Name/binary>>.
lang_string() -> <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#langString">>.
