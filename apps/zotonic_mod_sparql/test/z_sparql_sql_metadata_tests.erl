-module(z_sparql_sql_metadata_tests).
-include_lib("eunit/include/eunit.hrl").

metadata_identity_test() ->
    Int = z_sparql_sql_metadata:from_term(
        {literal, <<"1">>, <<"http://www.w3.org/2001/XMLSchema#int">>, undefined}),
    ?assertEqual(<<"'http://www.w3.org/2001/XMLSchema#int'">>, maps:get(datatype, Int)),
    ?assertNotEqual(Int, z_sparql_sql_metadata:from_type(integer)),
    Lang = z_sparql_sql_metadata:from_term({literal, <<"hi">>, undefined, <<"EN">>}),
    ?assertEqual(<<"'en'">>, maps:get(language, Lang)),
    ?assertEqual(<<"'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'">>, maps:get(datatype, Lang)),
    ?assertEqual(z_sparql_sql_metadata:unknown(), z_sparql_sql_metadata:from_term(undefined)).

custom_datatype_sql_escaping_test() ->
    Metadata = z_sparql_sql_metadata:literal(<<"https://example.test/a'b">>, undefined),
    ?assertEqual(<<"'https://example.test/a''b'">>, maps:get(datatype, Metadata)).

identical_branch_metadata_test() ->
    Metadata = z_sparql_sql_metadata:from_type(integer),
    ?assertEqual(Metadata, z_sparql_sql_metadata:choose(<<"test_condition">>, Metadata, Metadata)),
    ?assert(z_sparql_sql_metadata:is_static(Metadata)),
    ?assertNot(z_sparql_sql_metadata:is_static(z_sparql_sql_metadata:choose(
        <<"test_condition">>, Metadata, z_sparql_sql_metadata:from_type(text)))).

nested_numeric_metadata_size_test() ->
    Initial = (z_sparql_sql_metadata:unknown())#{datatype => [<<"input.datatype">>]},
    Integer = z_sparql_sql_metadata:from_type(integer),
    Nested = lists:foldl(fun(_, Metadata) ->
        z_sparql_sql_metadata:numeric('+', Metadata, Integer)
    end, Initial, lists:seq(1, 10)),
    ?assert(iolist_size(maps:get(datatype, Nested)) < 30000).
