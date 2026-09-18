%% Shared expression and compiler state records.
-ifndef(Z_SPARQL_SQL_HRL).
-define(Z_SPARQL_SQL_HRL, true).

-type value_type() ::
      any
    | boolean
    | datetime
    | float
    | fts
    | fulltext
    | id
    | ids
    | integer
    | list
    | number
    | text
    | uri.

-record(sql_expression, {
    % SQL values keep their stored type until an operator or builtin
    % requests a concrete type. This keeps JSONB values as-is until
    % they are needed as values in expressions.
    sql :: term(),
    type :: value_type(),
    source :: argument | column | expression | jsonb,
    defined = true :: true | false | term(),
    rdf = undefined :: undefined | z_sparql_sql_metadata:metadata()
}).

-record(sql_state, {
    arguments = #{} :: #{ z_sparql_plan:variable() := z_sparql_plan:argument() },
    bindings = #{} :: map(),
    solution_bindings = #{} :: map(),
    metadata_variables = #{} :: map(),
    alias_nr = 1 :: pos_integer(),
    alias_scope = 0 :: non_neg_integer(),
    context :: z:context()
}).

-endif.
