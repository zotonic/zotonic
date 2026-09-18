-module(z_sparql_sql_metadata_pipeline_tests).
-moduledoc("RDF metadata preservation through the SQL expression pipeline.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/z_sparql_sql.hrl").

rdf_literal_identity_test() ->
    lists:foreach(fun({Ast, Datatype, Language}) ->
        {Expression, _} = z_sparql_sql:expression_to_sql(Ast, #sql_state{}, z_sparql_sql:empty_term()),
        ?assertEqual(z_sparql_sql_metadata:literal(Datatype, Language), z_sparql_sql:expression_metadata(Expression))
    end, [
        {{literal, <<"42">>, <<"http://www.w3.org/2001/XMLSchema#int">>, undefined},
            <<"http://www.w3.org/2001/XMLSchema#int">>, undefined},
        {{literal, <<"opaque">>, <<"https://example.test/custom">>, undefined},
            <<"https://example.test/custom">>, undefined},
        {{literal, <<"Hallo">>, undefined, <<"NL">>}, undefined, <<"nl">>},
        {{decimal, <<"1.2">>}, <<"http://www.w3.org/2001/XMLSchema#decimal">>, undefined},
        {{double, <<"1.2">>}, <<"http://www.w3.org/2001/XMLSchema#double">>, undefined},
        {{literal, <<"https://example.test/">>, <<"http://www.w3.org/2001/XMLSchema#anyURI">>, undefined},
            <<"http://www.w3.org/2001/XMLSchema#anyURI">>, undefined}
    ]).

rdf_constructor_identity_test() ->
    Iri = <<"http://www.w3.org/2001/XMLSchema#short">>,
    {Expression, _} = z_sparql_sql:expression_to_sql(
        {call, {iri, Iri}, [{integer, <<"42">>}]}, #sql_state{}, z_sparql_sql:empty_term()),
    ?assertEqual(z_sparql_sql_metadata:literal(Iri, undefined), z_sparql_sql:expression_metadata(Expression)),
    {resource, Resource} = z_sparql_sql:binding_expression({resource, <<"rsc">>}),
    ?assertEqual(z_sparql_sql_metadata:iri(), z_sparql_sql:expression_metadata(Resource)),
    ?assertNotEqual(z_sparql_sql:expression_metadata(Resource), z_sparql_sql_metadata:from_type(id)).

rdf_values_optional_scope_test() ->
    Variable = {var, <<"word">>},
    Rows = [[{literal, <<"hello">>, undefined, <<"en">>}], [{literal, <<"hallo">>, undefined, <<"nl">>}]],
    {Terms, State} = z_sparql_sql:values_to_sql([Variable], Rows, #sql_state{metadata_variables = #{Variable => [kind, datatype, language]}}),
    {value, Bound} = maps:get(Variable, State#sql_state.bindings),
    ?assertEqual(z_sparql_sql:metadata_columns(<<"sparql_values_1">>, 1), z_sparql_sql:expression_metadata(Bound)),
    {Projection, Bindings} = z_sparql_sql:optional_projection(#{}, State#sql_state.bindings, <<"opt">>, State#sql_state.metadata_variables),
    {value, Optional} = maps:get(Variable, Bindings),
    ?assertEqual(z_sparql_sql:metadata_columns(<<"opt">>, 1), z_sparql_sql:expression_metadata(Optional)),
    Sql = z_search_terms:combine([#search_sql_nested{
        operator = {left_join, <<"opt">>}, terms = Terms ++ [Projection]
    }]),
    ?assertNotEqual(nomatch, binary:match(Sql#search_sql.from, <<"rdf_language_1">>)),
    ?assertNotEqual(nomatch, binary:match(Sql#search_sql.from, <<"'en'">>)),
    ?assertNotEqual(nomatch, binary:match(Sql#search_sql.from, <<"'nl'">>)).

rdf_projection_metadata_arguments_test() ->
    Variable = {var, <<"chosen">>},
    Ast = {call, 'if', [true,
        {literal, <<"hello">>, undefined, <<"en">>},
        {literal, <<"hallo">>, undefined, <<"nl">>}]},
    {Expression, Term} = z_sparql_sql:expression_to_sql(Ast, #sql_state{}, z_sparql_sql:empty_term()),
    State = z_sparql_sql:bind_projection(Variable, Expression, Term, #sql_state{}),
    {value, Bound} = maps:get(Variable, State#sql_state.bindings),
    Sql = z_search_terms:combine([#search_sql_term{
        select = [maps:get(language, z_sparql_sql:expression_metadata(Bound))]
    }]),
    ?assertEqual([true], Sql#search_sql.args),
    ?assertNotEqual(nomatch, binary:match(Sql#search_sql.select,
        <<"CASE WHEN $1 THEN 'en' ELSE 'nl' END">>)).

rdf_storage_coercion_keeps_identity_test() ->
    Expression = #sql_expression{sql = <<"rsc.props_json->'value'">>, type = text, source = jsonb},
    Coerced = z_sparql_sql:coerce_expression(Expression, text),
    ?assertEqual(z_sparql_sql:expression_metadata(Expression), z_sparql_sql:expression_metadata(Coerced)),
    ?assertEqual(expression, Coerced#sql_expression.source).

rdf_coalesce_metadata_test() ->
    Variable = {var, <<"optional_word">>},
    First = #sql_expression{
        sql = <<"opt.value">>, type = text, source = column,
        rdf = z_sparql_sql_metadata:literal(undefined, <<"en">>)
    },
    State = #sql_state{bindings = #{Variable => {value, First}}},
    {Expression, Term} = z_sparql_sql:expression_to_sql({call, coalesce,
        [Variable, {literal, <<"hallo">>, undefined, <<"nl">>}]}, State, z_sparql_sql:empty_term()),
    Sql = z_search_terms:combine([Term#search_sql_term{
        select = [maps:get(language, z_sparql_sql:expression_metadata(Expression))]
    }]),
    ?assertEqual(<<"rsc.id, CASE WHEN (opt.value) IS NOT NULL THEN 'en' ELSE CASE WHEN ($1) IS NOT NULL THEN 'nl' ELSE NULL END END">>,
        Sql#search_sql.select).

rdf_numeric_promotion_test() ->
    {Expression, _} = z_sparql_sql:expression_to_sql(
        {'/', {integer, <<"3">>}, {integer, <<"2">>}}, #sql_state{}, z_sparql_sql:empty_term()),
    ?assertEqual(z_sparql_sql_metadata:from_type(number), z_sparql_sql:expression_metadata(Expression)),
    Dynamic = #sql_expression{
        sql = <<"input.value">>, type = integer, source = column,
        rdf = z_sparql_sql:metadata_columns(<<"input">>, 1)
    },
    ?assertEqual(z_sparql_sql_metadata:unknown(), z_sparql_sql:aggregate_metadata(min, Dynamic, integer)).
