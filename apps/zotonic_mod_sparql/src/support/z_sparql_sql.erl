%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Map a parsed SPARQL SELECT query to Zotonic SQL search terms.
%% @end

%% Copyright 2025 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_sparql_sql).

-export([
    to_sql_term/2,
    to_sql_term/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-type sql_term() ::
      #search_sql_term{}
    | #search_sql_nested{}.

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
    defined = true :: true | false | term()
}).

-export_type([ sql_term/0 ]).

-record(sql_state, {
    arguments = #{} :: #{ z_sparql_plan:variable() := z_sparql_plan:argument() },
    bindings = #{} :: map(),
    solution_bindings = #{} :: map(),
    alias_nr = 1 :: pos_integer(),
    context :: z:context()
}).


%% @doc Map a parsed SPARQL SELECT query to nested Zotonic SQL search terms.
-spec to_sql_term(ParsedQuery, Context) -> {ok, [ sql_term() ]} | {error, Reason} when
    ParsedQuery :: term(),
    Context :: z:context(),
    Reason :: term().
to_sql_term(ParsedQuery, Context) ->
    to_sql_term(ParsedQuery, #{}, Context).

%% @doc Map a parsed query with pre-bound variables to Zotonic SQL terms.
-spec to_sql_term(ParsedQuery, Arguments, Context) ->
    {ok, [ sql_term() ]} | {error, Reason} when
        ParsedQuery :: term(),
        Arguments :: map(),
        Context :: z:context(),
        Reason :: term().
to_sql_term(ParsedQuery, Arguments, Context) ->
    % First map the parsed query to a query plan, normalizing values and expressions.
    case z_sparql_plan:to_query_plan(ParsedQuery, Arguments, Context) of
        {ok, Plan} ->
            % Map the query plan to Zotonic sql terms
            query_plan_to_sql(Plan, Context);
        {error, _} = Error ->
            Error
    end.


%% SQL term generation

-spec query_plan_to_sql(Plan, Context) -> {ok, [ sql_term() ]} | {error, Reason} when
    Plan :: z_sparql_plan:query_plan(),
    Context :: z:context(),
    Reason :: term().
query_plan_to_sql(#{
    arguments := Arguments,
    dataset := [],
    limit := undefined,
    offset := undefined,
    root := {var, RootName},
    where := Pattern
} = Plan, Context) ->
    State0 = #sql_state{
        arguments = Arguments,
        bindings = #{{var, RootName} => {resource, <<"rsc">>}},
        context = Context
    },
    try
        {Terms, State1} = pattern_to_sql(Pattern, State0),
        State1a = State1#sql_state{ solution_bindings = State1#sql_state.bindings },
        {Projection, State2} = projection_term(Plan, State1a),
        GroupTerms = group_terms(maps:get(group_by, Plan), State2),
        HavingTerms = having_terms(maps:get(having, Plan), State2),
        OrderTerms = order_terms(maps:get(order_by, Plan), State2),
        {ok, Terms ++ [Projection] ++ GroupTerms ++ HavingTerms ++ OrderTerms}
    catch
        throw:{error, Reason} ->
            {error, Reason}
    end;
query_plan_to_sql(#{ dataset := [_ | _] }, _Context) ->
    {error, {unsupported, dataset}};
query_plan_to_sql(#{ limit := Limit }, _Context) when Limit =/= undefined ->
    {error, {unsupported, limit}};
query_plan_to_sql(#{ offset := Offset }, _Context) when Offset =/= undefined ->
    {error, {unsupported, offset}};
query_plan_to_sql(#{ root := undefined }, _Context) ->
    {error, no_root_resource}.

pattern_to_sql(identity, State) ->
    {[], State};
pattern_to_sql({triple, Subject, Predicate, Object}, State) ->
    triple_to_sql(Subject, Predicate, Object, State);
pattern_to_sql({join, Left, Right}, State0) ->
    {LeftTerms, State1} = pattern_to_sql(Left, State0),
    {RightTerms, State2} = pattern_to_sql(Right, State1),
    {LeftTerms ++ RightTerms, State2};
pattern_to_sql({union, Branches}, State0) ->
    {BranchTerms, BranchStates, State1} = union_branches_to_sql(Branches, State0, State0),
    State2 = common_branch_bindings(BranchStates, State1),
    {[#search_sql_nested{ operator = <<"anyof">>, terms = BranchTerms }], State2};
pattern_to_sql({filter, Expression, Pattern}, State0) ->
    {Terms, State1} = pattern_to_sql(Pattern, State0),
    {Expression1, FilterTerm} = expression_to_sql(Expression, State1, empty_term()),
    FilterTerm1 = FilterTerm#search_sql_term{ where = [expression_sql(Expression1)] },
    {Terms ++ [FilterTerm1], State1};
pattern_to_sql({left_join, _, _, _}, _State) ->
    throw({error, {unsupported, optional}});
pattern_to_sql({extend, _, _, _}, _State) ->
    throw({error, {unsupported, bind}});
pattern_to_sql({values, Variables, Rows}, State) ->
    values_to_sql(Variables, Rows, State);
pattern_to_sql({graph, _, _}, _State) ->
    throw({error, {unsupported, graph}}).

values_to_sql(Variables, Rows, State0) ->
    Columns = values_columns(Variables, Rows),
    {Alias, State1} = new_alias(<<"values">>, State0),
    {Table, Term0} = values_table(Columns, Rows, State1#sql_state.context),
    Term1 = add_table(Alias, Table, Term0),
    {Term2, State2} = bind_values_columns(Columns, Alias, Term1, State1),
    Term3 = case Rows of
        [] -> add_where(<<"false">>, Term2);
        _ -> Term2
    end,
    {[Term3], State2}.

values_columns(Variables, Rows) ->
    VariableNrs = lists:zip(Variables, lists:seq(1, length(Variables))),
    [ values_column(Variable, Nr, Rows) || {Variable, Nr} <- VariableNrs ].

values_column(Variable, Nr, Rows) ->
    Values = [ lists:nth(Nr, Row) || Row <- Rows ],
    Values1 = [ Value || Value <- Values, Value =/= undefined ],
    case lists:usort([ values_kind(Value) || Value <- Values1 ]) of
        [] ->
            {Variable, Nr, unbound, any};
        [resource] ->
            {Variable, Nr, resource, id};
        [value] ->
            Types = [ Type || Value <- Values1, {_Value, Type} <- [rdf_typed_value(Value)] ],
            {Variable, Nr, value, common_types(Types)};
        _ ->
            throw({error, {incompatible_values, Variable, Values1}})
    end.

values_kind({iri, _Iri}) -> resource;
values_kind(_Value) -> value.

values_table(Columns, Rows, Context) ->
    ColumnNames = lists:append([
        [values_column_name(value, Nr), values_column_name(defined, Nr)]
        || {_Variable, Nr, _Kind, _Type} <- Columns
    ]),
    {RowsSql, Term} = values_rows(Rows, Columns, Context, empty_term(), []),
    RowsSql1 = case RowsSql of
        [] -> [<<"(1)">>];
        _ -> RowsSql
    end,
    Names1 = case ColumnNames of
        [] -> [<<"dummy">>];
        _ -> ColumnNames
    end,
    Table = [
        <<"(SELECT * FROM (VALUES ">>, lists:join(<<", ">>, RowsSql1),
        <<") AS sparql_values_row(">>, lists:join(<<", ">>, Names1), <<"))">>
    ],
    {Table, Term}.

values_rows([], _Columns, _Context, Term, Acc) ->
    {lists:reverse(Acc), Term};
values_rows([Row | Rest], Columns, Context, Term0, Acc) ->
    {Cells, Term1} = values_row(Row, Columns, Context, Term0, []),
    RowSql = case Cells of
        [] -> <<"(1)">>;
        _ -> [$\(, lists:join(<<", ">>, Cells), $\)]
    end,
    values_rows(Rest, Columns, Context, Term1, [RowSql | Acc]).

values_row([], [], _Context, Term, Acc) ->
    {lists:reverse(Acc), Term};
values_row([Value | Rest], [{_Variable, _Nr, Kind, Type} | Columns], Context, Term0, Acc) ->
    {ValueSql, DefinedSql, Term1} = values_cell(Value, Kind, Type, Context, Term0),
    values_row(Rest, Columns, Context, Term1, [DefinedSql, ValueSql | Acc]).

values_cell(undefined, _Kind, Type, _Context, Term) ->
    {[<<"CAST(NULL AS ">>, values_sql_type(Type), $\)], <<"false">>, Term};
values_cell({iri, Iri}, resource, id, Context, Term0) ->
    RscId = case m_rsc:rid(Iri, Context) of
        undefined -> -1;
        Id -> Id
    end,
    {Arg, Term1} = add_arg(RscId, Term0),
    {[<<"CAST(">>, Arg, <<" AS bigint)">>], <<"true">>, Term1};
values_cell(Value, value, Type, _Context, Term0) ->
    {Value1, _ValueType} = rdf_typed_value(Value),
    {Arg, Term1} = add_arg(Value1, Term0),
    {[<<"CAST(">>, Arg, <<" AS ">>, values_sql_type(Type), $\)], <<"true">>, Term1}.

%% @doc Values support a different set of types than arguments in datatype_argument_sql_type/1
values_sql_type(any) -> <<"text">>;
values_sql_type(boolean) -> <<"boolean">>;
values_sql_type(datetime) -> <<"timestamptz">>;
values_sql_type(float) -> <<"double precision">>;
values_sql_type(id) -> <<"bigint">>;
values_sql_type(integer) -> <<"bigint">>;
values_sql_type(number) -> <<"numeric">>;
values_sql_type(text) -> <<"text">>;
values_sql_type(uri) -> <<"text">>;
values_sql_type(Type) -> throw({error, {unsupported_values_type, Type}}).

values_column_name(value, Nr) ->
    <<"value_", (integer_to_binary(Nr))/binary>>;
values_column_name(defined, Nr) ->
    <<"defined_", (integer_to_binary(Nr))/binary>>.

bind_values_columns([], _Alias, Term, State) ->
    {Term, State};
bind_values_columns([Column | Rest], Alias, Term0, State0) ->
    {Term1, State1} = bind_values_column(Column, Alias, Term0, State0),
    bind_values_columns(Rest, Alias, Term1, State1).

bind_values_column({Variable, _Nr, unbound, any}, _Alias, Term,
        #sql_state{ bindings = Bindings } = State) ->
    case maps:is_key(Variable, Bindings) of
        true -> {Term, State};
        false ->
            Expression = #sql_expression{
                sql = <<"NULL">>, type = any, source = expression, defined = false
            },
            {Term, State#sql_state{ bindings = Bindings#{ Variable => {value, Expression} } }}
    end;
bind_values_column({Variable, Nr, Kind, Type}, Alias, Term0,
        #sql_state{ bindings = Bindings } = State) ->
    Expression = #sql_expression{
        sql = column_expression(Alias, values_column_name(value, Nr)),
        type = Type,
        source = column,
        defined = column_expression(Alias, values_column_name(defined, Nr))
    },
    case maps:find(Variable, Bindings) of
        {ok, {resource, ResourceAlias}} when Kind =:= resource ->
            Where = values_resource_where(ResourceAlias, Expression),
            {add_where(Where, Term0), State};
        {ok, {resource, _ResourceAlias}} ->
            Where = [<<"NOT (">>, expression_defined_sql(Expression), $\)],
            {add_where(Where, Term0), State};
        {ok, {value, BoundExpression}} ->
            {Where, MergedExpression} = merge_values_expressions(BoundExpression, Expression),
            Bindings1 = Bindings#{ Variable => {value, MergedExpression} },
            {add_where(Where, Term0), State#sql_state{ bindings = Bindings1 }};
        error ->
            Bindings1 = Bindings#{ Variable => {value, Expression} },
            {Term0, State#sql_state{ bindings = Bindings1 }}
    end.

values_resource_where(Alias, Expression) ->
    [
        <<"((NOT ">>, expression_defined_sql(Expression), <<") OR (">>,
        column_expression(Alias, <<"id">>), <<" = ">>, expression_sql(Expression), <<"))">>
    ].

merge_values_expressions(Left, Right) ->
    Type = common_type(Left#sql_expression.type, Right#sql_expression.type),
    Left1 = coerce_expression(Left, Type),
    Right1 = coerce_expression(Right, Type),
    LeftDefined = expression_defined_sql(Left1),
    RightDefined = expression_defined_sql(Right1),
    Where = [
        <<"((NOT ">>, LeftDefined, <<") OR (NOT ">>, RightDefined, <<") OR (">>,
        expression_sql(Left1), <<" = ">>, expression_sql(Right1), <<"))">>
    ],
    Merged = #sql_expression{
        sql = [<<"COALESCE(">>, expression_sql(Left1), <<", ">>, expression_sql(Right1), $\)],
        type = Type,
        source = expression,
        defined = [$(, LeftDefined, <<" OR ">>, RightDefined, $)]
    },
    {Where, Merged}.

triple_to_sql(_Subject, {var, _}, _Object, _State) ->
    throw({error, {unsupported, variable_predicate}});
triple_to_sql(_Subject, #{ mapping := undefined, iri := Iri }, _Object, _State) ->
    throw({error, {unknown_predicate, Iri}});
triple_to_sql(Subject, #{ mapping := Mapping }, Object, State0) ->
    {SubjectAlias, Term0, State1} = resource_alias(Subject, empty_term(), State0),
    mapped_triple_to_sql(Mapping, SubjectAlias, Object, Term0, State1).

mapped_triple_to_sql({column, Table, Column, Type}, SubjectAlias, Object, Term0, State0) ->
    {Alias, Term1, State1} = property_alias(Table, SubjectAlias, Term0, State0),
    Expression = #sql_expression{
        sql = column_expression(Alias, Column),
        type = normalize_type(Type),
        source = column
    },
    {Term2, State2} = bind_object(Object, value, Expression, Table, Column, Term1, State1),
    {[Term2], State2};
mapped_triple_to_sql(
        {search_column, Table, ValueColumn, _SearchColumn, _SearchType},
        SubjectAlias, Object, Term0, State0) ->
    {Alias, Term1, State1} = property_alias(Table, SubjectAlias, Term0, State0),
    Expression = #sql_expression{
        sql = column_expression(Alias, ValueColumn),
        type = text,
        source = column
    },
    {Term2, State2} = bind_object(
        Object, value, Expression, Table, ValueColumn, Term1, State1),
    {[Term2], State2};
mapped_triple_to_sql({jsonb, Table, Column, Selector, Type}, SubjectAlias, Object, Term0, State0) ->
    {Alias, Term1, State1} = property_alias(Table, SubjectAlias, Term0, State0),
    Expression = #sql_expression{
        sql = jsonb_expression(Alias, Column, Selector),
        type = normalize_type(Type),
        source = jsonb
    },
    {Term2, State2} = bind_jsonb_object(Object, Expression, Term1, State1),
    {[Term2], State2};
mapped_triple_to_sql({edge, Predicate, false}, SubjectAlias, Object, Term0, State0) ->
    edge_to_sql(
        Predicate, <<"subject_id">>, <<"object_id">>,
        SubjectAlias, Object, Term0, State0);
mapped_triple_to_sql({edge, Predicate, true}, SubjectAlias, Object, Term0, State0) ->
    edge_to_sql(
        Predicate, <<"object_id">>, <<"subject_id">>,
        SubjectAlias, Object, Term0, State0);
mapped_triple_to_sql(category, SubjectAlias, {iri, Iri}, Term0, State) ->
    Context = State#sql_state.context,
    case m_rsc:uri_lookup(Iri, Context) of
        undefined ->
            {[add_where(<<"false">>, Term0)], State};
        CategoryId ->
            category_to_sql(CategoryId, SubjectAlias, Term0, State)
    end;
mapped_triple_to_sql(category, SubjectAlias, {var, _} = Variable, Term0, State) ->
    case argument_resource_id(Variable, State) of
        {ok, undefined} ->
            {[add_where(<<"false">>, Term0)], State};
        {ok, CategoryId} ->
            category_to_sql(CategoryId, SubjectAlias, Term0, State);
        undefined ->
            throw({error, {expected_category, Variable}})
    end;
mapped_triple_to_sql(category, _SubjectAlias, Object, _Term, _State) ->
    throw({error, {expected_category, Object}});
mapped_triple_to_sql(subclass, SubjectAlias, {iri, Iri}, Term0, State) ->
    Context = State#sql_state.context,
    case m_rsc:uri_lookup(Iri, Context) of
        undefined ->
            {[add_where(<<"false">>, Term0)], State};
        CategoryId ->
            subclass_to_sql(CategoryId, SubjectAlias, Term0, State)
    end;
mapped_triple_to_sql(subclass, SubjectAlias, {var, _} = Variable, Term0, State) ->
    case argument_resource_id(Variable, State) of
        {ok, undefined} ->
            {[add_where(<<"false">>, Term0)], State};
        {ok, CategoryId} ->
            subclass_to_sql(CategoryId, SubjectAlias, Term0, State);
        undefined ->
            throw({error, {expected_category, Variable}})
    end;
mapped_triple_to_sql(subclass, _SubjectAlias, Object, _Term, _State) ->
    throw({error, {expected_category, Object}}).

category_to_sql(CategoryId, SubjectAlias, Term0, State) ->
    Context = State#sql_state.context,
    case m_rsc:is_a(CategoryId, category, Context) of
        true ->
            CategoryIds = category_ids(CategoryId, Context),
            {CategoryArg, Term1} = add_arg(CategoryIds, Term0),
            Where = [SubjectAlias, <<".category_id = ANY(">>, CategoryArg, <<"::int[])">>],
            {[add_where(Where, Term1)], State};
        false ->
            throw({error, {not_a_category, CategoryId}})
    end.

subclass_to_sql(CategoryId, SubjectAlias, Term0, State) ->
    Context = State#sql_state.context,
    case m_rsc:is_a(CategoryId, category, Context) of
        true ->
            CategoryIds = lists:delete(CategoryId, category_ids(CategoryId, Context)),
            {CategoryArg, Term1} = add_arg(CategoryIds, Term0),
            Where = [SubjectAlias, <<".id = ANY(">>, CategoryArg, <<"::int[])">>],
            {[add_where(Where, Term1)], State};
        false ->
            throw({error, {not_a_category, CategoryId}})
    end.

edge_to_sql(Predicate, SubjectColumn, ObjectColumn, SubjectAlias, Object, Term0, State0) ->
    PredicateId = case m_predicate:name_to_id(Predicate, State0#sql_state.context) of
        {ok, Id} -> Id;
        {error, Reason} -> throw({error, Reason})
    end,
    {EdgeAlias, State1} = new_alias(<<"edge">>, State0),
    Term1 = add_table(EdgeAlias, <<"edge">>, Term0),
    Term2 = add_where([EdgeAlias, $., SubjectColumn, <<" = ">>, SubjectAlias, <<".id">>], Term1),
    {PredicateArg, Term3} = add_arg(PredicateId, Term2),
    Term4 = add_where([EdgeAlias, <<".predicate_id = ">>, PredicateArg], Term3),
    {Term5, State2} = bind_edge_object(Object, EdgeAlias, ObjectColumn, Term4, State1),
    {[Term5], State2}.

category_ids(CategoryId, Context) ->
    [
        proplists:get_value(id, Category)
        || Category <- m_category:tree_flat(CategoryId, Context)
    ].

resource_alias({var, _} = Variable, Term, State) ->
    {Alias, Term1, State1} = resource_binding_alias(Variable, Term, State),
    {Alias, bind_resource_argument(Variable, Alias, Term1, State1), State1};
resource_alias({bnode, _} = BlankNode, Term, State) ->
    resource_binding_alias(BlankNode, Term, State);
resource_alias({iri, Iri}, Term, State0) ->
    {Alias, State1} = new_alias(<<"rsc">>, State0),
    Term1 = add_table(Alias, <<"rsc">>, Term),
    case m_rsc:rid(Iri, State0#sql_state.context) of
        undefined ->
            {Alias, add_where(<<"false">>, Term1), State1};
        RscId ->
            {Arg, Term2} = add_arg(RscId, Term1),
            {Alias, add_where([Alias, <<".id = ">>, Arg], Term2), State1}
    end;
resource_alias(Term, _SqlTerm, _State) ->
    throw({error, {expected_resource, Term}}).

resource_binding_alias(Key, Term, #sql_state{ bindings = Bindings } = State) ->
    case maps:find(Key, Bindings) of
        {ok, {resource, Alias}} ->
            {Alias, Term, State};
        {ok, {value, _Expression}} ->
            throw({error, {incompatible_variable, Key}});
        error ->
            {Alias, State1} = new_alias(<<"rsc">>, State),
            Bindings1 = Bindings#{ Key => {resource, Alias} },
            {Alias, add_table(Alias, <<"rsc">>, Term), State1#sql_state{ bindings = Bindings1 }}
    end.

bind_resource_argument(Variable, Alias, Term0, State) ->
    case argument_resource_id(Variable, State) of
        {ok, undefined} ->
            add_where(<<"false">>, Term0);
        {ok, RscId} ->
            {Arg, Term1} = add_arg(RscId, Term0),
            add_where([Alias, <<".id = ">>, Arg], Term1);
        undefined ->
            Term0
    end.

argument_resource_id(Variable, #sql_state{ arguments = Arguments, context = Context }) ->
    case maps:find(Variable, Arguments) of
        {ok, undefined} ->
            undefined;
        {ok, {resource, Reference}} ->
            {ok, m_rsc:rid(Reference, Context)};
        {ok, {iri, Iri}} ->
            {ok, m_rsc:rid(Iri, Context)};
        {ok, {value, Reference, Type}} when Type =:= integer; Type =:= text ->
            {ok, m_rsc:rid(Reference, Context)};
        {ok, Argument} ->
            throw({error, {expected_resource_argument, Variable, Argument}});
        error ->
            undefined
    end.

argument_value({value, Value, _Type}, _Context) ->
    Value;
argument_value({iri, Iri}, _Context) ->
    Iri;
argument_value({resource, Reference}, Context) ->
    m_rsc:rid(Reference, Context).

property_alias(<<"rsc">>, SubjectAlias, Term, State) ->
    {SubjectAlias, Term, State};
property_alias(Table, SubjectAlias, Term, State0) ->
    {Alias, State1} = new_alias(<<"prop">>, State0),
    On = [Alias, <<".id = ">>, SubjectAlias, <<".id">>],
    JoinInner = (Term#search_sql_term.join_inner)#{ Alias => {Table, On} },
    {Alias, Term#search_sql_term{ join_inner = JoinInner }, State1}.

bind_object({var, _} = Variable, Kind, Expression, Table, Column, Term0, State0) ->
    {Term1, State1} = bind_variable_object(Variable, Kind, Expression, Term0, State0),
    {bind_value_argument(Variable, Expression, Table, Column, Term1, State1), State1};
bind_object(Object, value, Expression, Table, Column, Term0, State) ->
    Value = rdf_value(Object),
    Value1 = column_value(Table, Column, Value, State#sql_state.context),
    {Arg, Term1} = add_arg(Value1, Term0),
    {add_where([expression_sql(Expression), <<" = ">>, Arg], Term1), State};
bind_object(Object, resource, _Expression, _Table, _Column, _Term, _State) ->
    throw({error, {expected_resource, Object}}).

bind_variable_object(Variable, Kind, Expression, Term,
        #sql_state{ bindings = Bindings } = State) ->
    case maps:find(Variable, Bindings) of
        {ok, {Kind, #sql_expression{} = BoundExpression}} ->
            {Where, MergedExpression} = bind_expressions(BoundExpression, Expression),
            Bindings1 = Bindings#{ Variable => {Kind, MergedExpression} },
            {add_where(Where, Term), State#sql_state{ bindings = Bindings1 }};
        {ok, _OtherKind} ->
            throw({error, {incompatible_variable, Variable}});
        error ->
            Bindings1 = Bindings#{ Variable => {Kind, Expression} },
            Where = [expression_sql(Expression), <<" IS NOT NULL">>],
            {add_where(Where, Term), State#sql_state{ bindings = Bindings1 }}
    end.

bind_expressions(#sql_expression{ defined = true } = BoundExpression, Expression) ->
    EqualExpression = binary_expression('=', Expression, BoundExpression),
    {expression_sql(EqualExpression), BoundExpression};
bind_expressions(#sql_expression{ defined = false }, Expression) ->
    {[expression_sql(Expression), <<" IS NOT NULL">>], Expression};
bind_expressions(BoundExpression, Expression) ->
    EqualExpression = binary_expression('=', Expression, BoundExpression),
    Where = [
        $\(, expression_sql(Expression), <<" IS NOT NULL AND ((NOT ">>,
        expression_defined_sql(BoundExpression), <<") OR ">>,
        expression_sql(EqualExpression), <<"))">>
    ],
    {Where, Expression}.

bind_value_argument(Variable, Expression, Table, Column, Term0,
        #sql_state{ arguments = Arguments, context = Context }) ->
    case maps:find(Variable, Arguments) of
        {ok, undefined} ->
            Term0;
        {ok, Argument} ->
            case argument_value(Argument, Context) of
                undefined ->
                    add_where(<<"false">>, Term0);
                Value ->
                    Value1 = column_value(Table, Column, Value, Context),
                    {Arg, Term1} = add_arg(Value1, Term0),
                    add_where([expression_sql(Expression), <<" = ">>, Arg], Term1)
            end;
        error ->
            Term0
    end.

bind_jsonb_object({var, _} = Variable, Expression, Term0, State0) ->
    {Term1, State1} = bind_variable_object(Variable, value, Expression, Term0, State0),
    {bind_jsonb_argument(Variable, Expression, Term1, State1), State1};
bind_jsonb_object(Object, Expression, Term0, State) ->
    Value = ?DB_PROPS_JSON(rdf_json_value(Object)),
    {Arg, Term1} = add_arg(Value, Term0),
    {add_where([expression_sql(Expression), <<" = ">>, Arg, <<"::jsonb">>], Term1), State}.

bind_jsonb_argument(Variable, Expression, Term0,
        #sql_state{ arguments = Arguments, context = Context }) ->
    case maps:find(Variable, Arguments) of
        {ok, undefined} ->
            Term0;
        {ok, Argument} ->
            case argument_value(Argument, Context) of
                undefined ->
                    add_where(<<"false">>, Term0);
                Value ->
                    {Arg, Term1} = add_arg(?DB_PROPS_JSON(Value), Term0),
                    add_where([expression_sql(Expression), <<" = ">>, Arg, <<"::jsonb">>], Term1)
            end;
        error ->
            Term0
    end.

bind_edge_object({var, _} = Variable, EdgeAlias, Column, Term0, State0) ->
    {ObjectAlias, Term1, State1} = resource_alias(Variable, Term0, State0),
    {add_where([EdgeAlias, $., Column, <<" = ">>, ObjectAlias, <<".id">>], Term1), State1};
bind_edge_object({bnode, _} = BlankNode, EdgeAlias, Column, Term, State) ->
    {ObjectAlias, Term1, State1} = resource_alias(BlankNode, Term, State),
    {add_where([EdgeAlias, $., Column, <<" = ">>, ObjectAlias, <<".id">>], Term1), State1};
bind_edge_object({iri, Iri}, EdgeAlias, Column, Term0, State) ->
    case m_rsc:rid(Iri, State#sql_state.context) of
        undefined ->
            {add_where(<<"false">>, Term0), State};
        RscId ->
            {Arg, Term1} = add_arg(RscId, Term0),
            {add_where([EdgeAlias, $., Column, <<" = ">>, Arg], Term1), State}
    end;
bind_edge_object(Object, _EdgeAlias, _Column, _Term, _State) ->
    throw({error, {expected_resource, Object}}).

expression_to_sql({var, _} = Variable, State, Term) ->
    case maps:find(Variable, State#sql_state.bindings) of
        {ok, {resource, Alias}} ->
            {#sql_expression{
                sql = column_expression(Alias, <<"id">>),
                type = id,
                source = column
            }, Term};
        {ok, {value, #sql_expression{} = Expression}} ->
            {Expression, Term};
        error -> argument_to_expression(Variable, State, Term)
    end;
expression_to_sql({Operator, Left, Right}, State, Term0)
    when Operator =:= 'or'; Operator =:= 'and';
         Operator =:= '='; Operator =:= '!=';
         Operator =:= '<'; Operator =:= '>';
         Operator =:= '=<'; Operator =:= '>=';
         Operator =:= '+'; Operator =:= '-';
         Operator =:= '*'; Operator =:= '/' ->
    {Left1, Term1} = expression_to_sql(Left, State, Term0),
    {Right1, Term2} = expression_to_sql(Right, State, Term1),
    {binary_expression(Operator, Left1, Right1), Term2};
expression_to_sql({'not', Expression}, State, Term0) ->
    {Expression1, Term1} = expression_to_sql(Expression, State, Term0),
    {unary_expression('not', Expression1), Term1};
expression_to_sql({'u+', Expression}, State, Term0) ->
    {Expression1, Term1} = expression_to_sql(Expression, State, Term0),
    {unary_expression('u+', Expression1), Term1};
expression_to_sql({'u-', Expression}, State, Term0) ->
    {Expression1, Term1} = expression_to_sql(Expression, State, Term0),
    {unary_expression('u-', Expression1), Term1};
expression_to_sql({aggregate, Function, Distinct, Argument, Separator}, State, Term0) ->
    aggregate_to_sql(Function, Distinct, Argument, Separator, State, Term0);
expression_to_sql({call, Function, [Argument]}, State, Term0)
    when Function =:= isliteral; Function =:= isnumeric ->
    type_test_to_sql(Function, Argument, State, Term0);
expression_to_sql({call, sameterm, [Left, Right]}, State, Term0) ->
    same_term_to_sql(Left, Right, State, Term0);
expression_to_sql({call, Function, Arguments}, State, Term0)
    when Function =:= fulltext; Function =:= fulltext_rank ->
    fulltext_to_sql(Function, Arguments, State, Term0);
expression_to_sql({call, {iri, Iri} = Function, Arguments}, State, Term0) ->
    case z_sparql_sql_datatype:mapping(Iri) of
        {ok, Mapping} ->
            datatype_to_sql(Function, Mapping, Arguments, State, Term0);
        undefined ->
            {Arguments1, Term1} = expression_list_to_sql(Arguments, State, Term0),
            function_to_sql(Function, Arguments1, Term1)
    end;
expression_to_sql({call, Function, Arguments}, State, Term0) ->
    {Arguments1, Term1} = expression_list_to_sql(Arguments, State, Term0),
    function_to_sql(Function, Arguments1, Term1);
expression_to_sql(Value, _State, Term0) ->
    {ArgumentValue, Type} = rdf_typed_value(Value),
    {Arg, Term1} = add_arg(ArgumentValue, Term0),
    {#sql_expression{ sql = Arg, type = Type, source = argument }, Term1}.

argument_to_expression(Variable, #sql_state{ arguments = Arguments, context = Context }, Term0) ->
    case maps:find(Variable, Arguments) of
        {ok, undefined} ->
            {#sql_expression{ sql = <<"NULL">>, type = any, source = expression }, Term0};
        {ok, {value, Value, Type}} ->
            argument_expression(Value, Type, Term0);
        {ok, {iri, Iri}} ->
            argument_expression(Iri, uri, Term0);
        {ok, {resource, Reference}} ->
            case m_rsc:rid(Reference, Context) of
                undefined -> throw({error, {unknown_resource_argument, Variable, Reference}});
                RscId -> argument_expression(RscId, id, Term0)
            end;
        error ->
            throw({error, {unbound_variable, Variable}})
    end.

argument_expression(Value, Type, Term0) ->
    {Arg, Term1} = add_arg(Value, Term0),
    {#sql_expression{ sql = Arg, type = Type, source = argument }, Term1}.

fulltext_to_sql(Function, [Resource, Query], State0, Term0) ->
    fulltext_to_sql(Function, Resource, default, Query, State0, Term0);
fulltext_to_sql(Function, [Resource, Field, Query], State0, Term0) ->
    fulltext_to_sql(Function, Resource, Field, Query, State0, Term0);
fulltext_to_sql(Function, Arguments, _State, _Term) ->
    throw({error, {invalid_function_arity, Function, length(Arguments)}}).

fulltext_to_sql(Function, Resource, Field, Query, State0, Term0) ->
    QueryText = fulltext_query_text(Query, State0),
    {ResourceAlias, Term1} = fulltext_resource_alias(Resource, Term0, State0),
    {SearchExpression, SearchType, NormalizeName, Term2} = fulltext_search_expression(Field, ResourceAlias, Term1),
    fulltext_expression(
        Function, SearchType, SearchExpression, NormalizeName, QueryText,
        State0#sql_state.context, Term2).

fulltext_query_text({literal, QueryText, _Datatype, _Language}, _State) ->
    QueryText;
fulltext_query_text({var, _} = Variable, #sql_state{ arguments = Arguments }) ->
    case maps:find(Variable, Arguments) of
        {ok, undefined} -> <<>>;
        {ok, {value, QueryText, text}} -> QueryText;
        {ok, Argument} -> throw({error, {expected_fulltext_query_string, Argument}});
        error -> throw({error, {unbound_variable, Variable}})
    end;
fulltext_query_text(Query, _State) ->
    throw({error, {expected_fulltext_query_string, Query}}).

fulltext_resource_alias({var, _} = Variable, Term,
        #sql_state{ bindings = Bindings } = State) ->
    case maps:find(Variable, Bindings) of
        {ok, {resource, Alias}} ->
            {Alias, bind_resource_argument(Variable, Alias, Term, State)};
        {ok, _} -> throw({error, {incompatible_variable, Variable}});
        error -> throw({error, {unbound_variable, Variable}})
    end;
fulltext_resource_alias({bnode, _} = BlankNode, Term, #sql_state{ bindings = Bindings }) ->
    case maps:find(BlankNode, Bindings) of
        {ok, {resource, Alias}} -> {Alias, Term};
        {ok, _} -> throw({error, {incompatible_variable, BlankNode}});
        error -> throw({error, {unbound_variable, BlankNode}})
    end;
fulltext_resource_alias({iri, Iri}, Term0, #sql_state{ context = Context }) ->
    Alias = search_alias({rsc, Iri}),
    Term1 = add_table(Alias, <<"rsc">>, Term0),
    case m_rsc:rid(Iri, Context) of
        undefined -> {Alias, add_where(<<"false">>, Term1)};
        RscId ->
            {RscArg, Term2} = add_arg(RscId, Term1),
            {Alias, add_where([Alias, <<".id = ">>, RscArg], Term2)}
    end;
fulltext_resource_alias(Resource, _Term, _State) ->
    throw({error, {expected_resource, Resource}}).

fulltext_search_expression(default, ResourceAlias, Term) ->
    {column_expression(ResourceAlias, <<"pivot_tsv">>), fts, <<"pivot_tsv">>, Term};
fulltext_search_expression(
        #{
            mapping := {search_column, Table, _ValueColumn, SearchColumn, SearchType},
            predicate := Predicate
        },
        ResourceAlias, Term0) ->
    {Alias, Term1} = search_property_alias(Table, SearchColumn, ResourceAlias, Term0),
    NormalizeName = case Predicate of
        <<"facet.", Facet/binary>> -> Facet;
        _ -> Predicate
    end,
    {column_expression(Alias, SearchColumn), SearchType, NormalizeName, Term1};
fulltext_search_expression(
        #{
            mapping := {column, Table, Column, Type},
            predicate := Predicate
        },
        ResourceAlias, Term0)
    when Type =:= fts; Type =:= fulltext; Type =:= text ->
    {Alias, Term1} = search_property_alias(Table, Column, ResourceAlias, Term0),
    SearchType = case Type of
        text -> fulltext;
        _ -> Type
    end,
    NormalizeName = case Predicate of
        <<"facet.", Facet/binary>> -> Facet;
        _ -> Column
    end,
    {column_expression(Alias, Column), SearchType, NormalizeName, Term1};
fulltext_search_expression(#{ mapping := undefined, iri := Iri }, _Alias, _Term) ->
    throw({error, {unknown_predicate, Iri}});
fulltext_search_expression(#{ iri := Iri }, _Alias, _Term) ->
    throw({error, {not_a_fulltext_column, Iri}});
fulltext_search_expression(Field, _Alias, _Term) ->
    throw({error, {expected_fulltext_field, Field}}).

search_property_alias(<<"rsc">>, _Column, ResourceAlias, Term) ->
    {ResourceAlias, Term};
search_property_alias(Table, _Column, ResourceAlias, #search_sql_term{ join_inner = Joins } = Term) ->
    Alias = search_alias({ResourceAlias, Table}),
    On = [Alias, <<".id = ">>, ResourceAlias, <<".id">>],
    {Alias, Term#search_sql_term{ join_inner = Joins#{ Alias => {Table, On} } }}.

%% @doc Make an alias for the table key, as we don't save it in the state we base the alias
%% name of a hash of the key.
search_alias(Key) ->
    <<"sparql_search_", (integer_to_binary(erlang:phash2(Key)))/binary>>.

fulltext_expression(fulltext, fts, SearchExpression, _Name, QueryText, Context, Term0) ->
    case mod_search:to_tsquery(QueryText, Context) of
        <<>> ->
            {constant_expression(false), Term0};
        TsQuery ->
            {QueryArg, Term1} = add_arg(TsQuery, Term0),
            {boolean_expression([$(, SearchExpression, <<" @@ ">>, QueryArg, $)]), Term1}
    end;
fulltext_expression(
        fulltext_rank, fts, SearchExpression, _Name, QueryText, Context, Term0) ->
    case mod_search:to_tsquery(QueryText, Context) of
        <<>> ->
            {float_expression(<<"CAST(0 AS double precision)">>), Term0};
        TsQuery ->
            {QueryArg, Term1} = add_arg(TsQuery, Term0),
            {BehaviourArg, Term2} = add_arg(mod_search:rank_behaviour(Context), Term1),
            {float_expression([
                <<"ts_rank_cd(">>, mod_search:rank_weight(Context), <<", ">>,
                SearchExpression, <<", ">>, QueryArg, <<", ">>, BehaviourArg, $)
            ]), Term2}
    end;
fulltext_expression(fulltext, fulltext, SearchExpression, Name, QueryText, Context, Term0) ->
    Normalized = normalize_fulltext(Name, QueryText, Context),
    case Normalized of
        <<>> ->
            {constant_expression(false), Term0};
        _ ->
            {QueryArg, Term1} = add_arg(Normalized, Term0),
            {boolean_expression([
                $(, QueryArg, <<" OPERATOR(public.<%) ">>, SearchExpression, $)
            ]), Term1}
    end;
fulltext_expression(fulltext_rank, fulltext, SearchExpression, Name, QueryText, Context, Term0) ->
    Normalized = normalize_fulltext(Name, QueryText, Context),
    case Normalized of
        <<>> ->
            % Nothing to rank, fallback to 0.0
            {float_expression(<<"CAST(0 AS double precision)">>), Term0};
        _ ->
            {QueryArg, Term1} = add_arg(Normalized, Term0),
            {float_expression([
                <<"public.word_similarity(">>, QueryArg, <<", ">>, SearchExpression, $)
            ]), Term1}
    end.

normalize_fulltext(Name, QueryText, Context) ->
    z_search:normalize_value(Name, text, QueryText, Context).

%% @doc If we know we have a resource, then we know the type is
%% not a literal and not a number (even when a rsc id is a number).
%% Also short-circuit some constants, then they do not need to go
%% through the jsonb conversions.
type_test_to_sql(Function, {var, _} = Variable, State, Term) ->
    case maps:find(Variable, State#sql_state.bindings) of
        {ok, {resource, _Alias}} ->
            {constant_expression(false), Term};
        {ok, {value, #sql_expression{ source = jsonb } = Expression}} ->
            % JSON properties can still contain a structured value despite a
            % name-derived hint, so inspect their actual JSON scalar type.
            jsonb_type_test_expression(Function, Expression, Term);
        {ok, {value, #sql_expression{ type = Type } = Expression}} ->
            case known_type_test(Function, Type) of
                undefined -> function_to_sql(Function, [Expression], Term);
                Result -> {constant_expression(Result), Term}
            end;
        error ->
            throw({error, {unbound_variable, Variable}})
    end;
type_test_to_sql(_Function, {iri, _Iri}, _State, Term) ->
    {constant_expression(false), Term};
type_test_to_sql(_Function, {bnode, _Name}, _State, Term) ->
    {constant_expression(false), Term};
type_test_to_sql(Function, {literal, _Value, _Datatype, _Language} = Literal, _State, Term) ->
    {_Value1, Type} = rdf_typed_value(Literal),
    {constant_expression(known_type_test(Function, Type)), Term};
type_test_to_sql(isliteral, {Type, _Value}, _State, Term)
    when Type =:= integer; Type =:= decimal; Type =:= double ->
    {constant_expression(true), Term};
type_test_to_sql(isnumeric, {Type, _Value}, _State, Term)
    when Type =:= integer; Type =:= decimal; Type =:= double ->
    {constant_expression(true), Term};
type_test_to_sql(isliteral, Value, _State, Term) when is_boolean(Value) ->
    {constant_expression(true), Term};
type_test_to_sql(isnumeric, Value, _State, Term) when is_boolean(Value) ->
    {constant_expression(false), Term};
type_test_to_sql(Function, Expression, State, Term0) ->
    {Expression1, Term1} = expression_to_sql(Expression, State, Term0),
    type_test_expression(Function, Expression1, Term1).

type_test_expression(Function, #sql_expression{ source = jsonb } = Expression, Term) ->
    jsonb_type_test_expression(Function, Expression, Term);
type_test_expression(Function, #sql_expression{ type = Type } = Expression, Term) ->
    case known_type_test(Function, Type) of
        undefined -> function_to_sql(Function, [Expression], Term);
        Result -> {constant_expression(Result), Term}
    end.

jsonb_type_test_expression(isliteral, Expression, Term) ->
    Sql = expression_sql(Expression),
    {boolean_expression([
        <<"(jsonb_typeof(">>, Sql, <<") IN ('string', 'number', 'boolean'))">>
    ]), Term};
jsonb_type_test_expression(isnumeric, Expression, Term) ->
    Sql = expression_sql(Expression),
    {boolean_expression([
        <<"(jsonb_typeof(">>, Sql, <<") = 'number')">>
    ]), Term}.

same_term_to_sql(Left, Right, State, Term0) ->
    {Left1, Term1} = expression_to_sql(Left, State, Term0),
    {Right1, Term2} = expression_to_sql(Right, State, Term1),
    {same_term_expression(Left1, Right1), Term2}.

same_term_expression(
        #sql_expression{ type = Type, source = jsonb } = Left,
        #sql_expression{ type = Type, source = jsonb } = Right) ->
    jsonb_same_term_expression(Left, Right);
same_term_expression(
        #sql_expression{ type = Type, source = jsonb } = Left,
        #sql_expression{ type = Type } = Right) ->
    jsonb_same_term_expression(Left, scalar_to_jsonb_expression(Right));
same_term_expression(
        #sql_expression{ type = Type } = Left,
        #sql_expression{ type = Type, source = jsonb } = Right) ->
    jsonb_same_term_expression(scalar_to_jsonb_expression(Left), Right);
same_term_expression(
        #sql_expression{ type = Type } = Left,
        #sql_expression{ type = Type } = Right) ->
    boolean_expression([
        <<"(">>, expression_sql(Left), <<" = ">>, expression_sql(Right), <<")">>
    ]);
same_term_expression(#sql_expression{}, #sql_expression{}) ->
    constant_expression(false).

%% @doc Directly compare JSONB scalar values, assume structured values are
%% not the same term.
jsonb_same_term_expression(Left, Right) ->
    LeftSql = expression_sql(Left),
    RightSql = expression_sql(Right),
    boolean_expression([
        <<"(jsonb_typeof(">>, LeftSql, <<") IN ('string', 'number', 'boolean') ">>,
        <<"AND (">>, LeftSql, <<")::text = (">>, RightSql, <<")::text)">>
    ]).

scalar_to_jsonb_expression(#sql_expression{ sql = Sql } = Expression) ->
    Expression#sql_expression{
        sql = [<<"to_jsonb(">>, Sql, $)],
        source = expression
    }.

function_to_sql(Function, Arguments, Term) ->
    case z_sparql_sql_function:type_signature(Function, length(Arguments)) of
        {ok, {ArgumentTypes, ResultType}} ->
            CommonType = common_function_type(ArgumentTypes, Arguments),
            Arguments1 = coerce_arguments(ArgumentTypes, Arguments, CommonType),
            ResultType1 = function_result_type(ResultType, Arguments1, CommonType),
            function_expression(Function, Arguments1, ResultType1, Term);
        {error, Reason} ->
            throw({error, Reason})
    end.

function_expression(Function, Arguments, ResultType, Term) ->
    SqlArguments = [ expression_sql(Argument) || Argument <- Arguments ],
    case z_sparql_sql_function:to_sql(Function, SqlArguments) of
        {ok, SqlExpression} ->
            {#sql_expression{
                sql = SqlExpression,
                type = ResultType,
                source = expression
            }, Term};
        {error, Reason} ->
            throw({error, Reason})
    end.

datatype_to_sql(_Function, {Type, SqlType}, [Argument], State, Term0) ->
    {Argument1, Term1} = expression_to_sql(Argument, State, Term0),
    case is_datatype_argument(Type, Argument1#sql_expression.type) of
        true -> {datatype_expression(Type, SqlType, Argument1), Term1};
        false -> throw({error, {incompatible_types, Type, Argument1#sql_expression.type}})
    end;
datatype_to_sql({iri, Iri}, _Mapping, Arguments, _State, _Term) ->
    throw({error, {invalid_datatype_arity, Iri, length(Arguments)}}).

%% These are the direct and predictable SPARQL constructor conversions. More
%% involved conversions, such as boolean-to-number, need explicit XPath rules.
is_datatype_argument(text, Type)
    when Type =:= text; Type =:= uri; Type =:= boolean;
         Type =:= integer; Type =:= float; Type =:= number;
         Type =:= datetime -> true;
is_datatype_argument(boolean, Type)
    when Type =:= text; Type =:= boolean -> true;
is_datatype_argument(integer, Type)
    when Type =:= text; Type =:= integer -> true;
is_datatype_argument(number, Type)
    when Type =:= text; Type =:= integer; Type =:= float; Type =:= number -> true;
is_datatype_argument(float, Type)
    when Type =:= text; Type =:= integer; Type =:= float; Type =:= number -> true;
is_datatype_argument(datetime, Type)
    when Type =:= text; Type =:= integer; Type =:= datetime -> true;
is_datatype_argument(_Expected, _Actual) -> false.

%% @doc Integers used as datetimes are Unix timestamps in seconds.
%% PostgreSQL's to_timestamp/1 accepts double precision and returns timestamptz.
datatype_expression(datetime, _SqlType, #sql_expression{ source = jsonb, type = integer, sql = Sql }) ->
    #sql_expression{
        sql = jsonb_unix_timestamp_sql(Sql),
        type = datetime,
        source = expression
    };
datatype_expression(datetime, _SqlType, #sql_expression{ source = argument, type = integer, sql = Sql }) ->
    #sql_expression{
        % Ensure bigint before converting to the type for to_timestamp/1.
        sql = unix_timestamp_sql(datatype_cast_sql(Sql, <<"bigint">>)),
        type = datetime,
        source = expression
    };
datatype_expression(datetime, _SqlType, #sql_expression{ type = integer, sql = Sql }) ->
    #sql_expression{
        sql = unix_timestamp_sql(Sql),
        type = datetime,
        source = expression
    };
datatype_expression(Type, SqlType, #sql_expression{ source = jsonb, sql = Sql }) ->
    #sql_expression{
        sql = jsonb_datatype_sql(Sql, Type, SqlType),
        type = Type,
        source = expression
    };
datatype_expression(Type, _SqlType, #sql_expression{ type = Type } = Expression) ->
    Expression;
datatype_expression(Type, SqlType, #sql_expression{ source = argument, type = ArgumentType, sql = Sql }) ->
    ArgumentSqlType = datatype_argument_sql_type(ArgumentType),
    #sql_expression{
        % Convert the parameter to its Erlang type before converting it further.
        % Otherwise the epgsql driver might try to encode a lexical binary as, for example,
        % a native timestamptz or boolean.
        sql = datatype_cast_sql(datatype_cast_sql(Sql, ArgumentSqlType), SqlType),
        type = Type,
        source = expression
    };
datatype_expression(Type, SqlType, #sql_expression{ sql = Sql }) ->
    #sql_expression{
        sql = datatype_cast_sql(Sql, SqlType),
        type = Type,
        source = expression
    }.

%% @doc Use 'CAST', as that is standard SQL and '::' is PostgreSQL only.
datatype_cast_sql(Sql, SqlType) ->
    [<<"CAST(">>, Sql, <<" AS ">>, SqlType, $)].

unix_timestamp_sql(Sql) ->
    [<<"to_timestamp(">>, datatype_cast_sql(Sql, <<"double precision">>), $)].

jsonb_unix_timestamp_sql(Sql) ->
    Scalar = [$(, Sql, <<" #>> '{}')">>],
    [
        <<"(CASE WHEN jsonb_typeof(">>, Sql, <<") = 'number' THEN ">>,
        unix_timestamp_sql(Scalar), <<" ELSE NULL END)">>
    ].

%% @doc Arguments support a different set of types than values in values_sql_type/1
datatype_argument_sql_type(text) -> <<"text">>;
datatype_argument_sql_type(uri) -> <<"text">>;
datatype_argument_sql_type(boolean) -> <<"boolean">>;
datatype_argument_sql_type(integer) -> <<"bigint">>;
datatype_argument_sql_type(float) -> <<"double precision">>;
datatype_argument_sql_type(number) -> <<"numeric">>;
datatype_argument_sql_type(datetime) -> <<"timestamptz">>.

%% @doc JSON properties can contain serialized Zotonic terms. We only
%% want to convert basic (scalar) types, so first we check the type of
%% the JSONB expression, and if it is not scalar then return NULL.
jsonb_datatype_sql(Sql, Type, SqlType) ->
    JsonTypes = jsonb_datatype_types(Type),
    Scalar = [$(, Sql, <<" #>> '{}')">>],
    Conversion = case Type of
        text -> Scalar;
        _ -> datatype_cast_sql(Scalar, SqlType)
    end,
    [
        <<"(CASE WHEN jsonb_typeof(">>, Sql, <<") IN (">>, JsonTypes,
        <<") THEN ">>, Conversion, <<" ELSE NULL END)">>
    ].

jsonb_datatype_types(text) -> <<"'string', 'number', 'boolean'">>;
jsonb_datatype_types(boolean) -> <<"'string', 'boolean'">>;
jsonb_datatype_types(datetime) -> <<"'string'">>;  % datatype_expression handles/3 jsonb integer 
jsonb_datatype_types(integer) -> <<"'string', 'number'">>;
jsonb_datatype_types(number) -> <<"'string', 'number'">>;
jsonb_datatype_types(float) -> <<"'string', 'number'">>.

expression_list_to_sql([], _State, Term) ->
    {[], Term};
expression_list_to_sql([Expression | Rest], State, Term0) ->
    {Expression1, Term1} = expression_to_sql(Expression, State, Term0),
    {Rest1, Term2} = expression_list_to_sql(Rest, State, Term1),
    {[Expression1 | Rest1], Term2}.

aggregate_to_sql(count, distinct, all, undefined, State, Term) ->
    Argument = solution_row_sql(State#sql_state.solution_bindings),
    aggregate_expression(count, distinct, Argument, undefined, integer, State, Term);
aggregate_to_sql(Function, Distinct, all, undefined, State, Term) ->
    aggregate_expression(Function, Distinct, all, undefined, integer, State, Term);
aggregate_to_sql(Function, Distinct, Argument, Separator, State, Term0) ->
    {Argument0, Term1} = expression_to_sql(Argument, State, Term0),
    case z_sparql_sql_aggregate:type_signature(Function) of
        {ok, {ArgumentType, ResultType}} ->
            Argument1 = aggregate_argument(ArgumentType, Argument0),
            ResultType1 = aggregate_result_type(ResultType, Argument1),
            {Separator1, Term2} = aggregate_separator(Separator, State, Term1),
            aggregate_expression(
                Function, Distinct, expression_sql(Argument1), Separator1,
                ResultType1, State, Term2);
        {error, Reason} ->
            throw({error, Reason})
    end.

aggregate_argument(any, Argument) ->
    Argument;
aggregate_argument(common, #sql_expression{ type = Type } = Argument) ->
    coerce_expression(Argument, Type);
aggregate_argument(Type, Argument) ->
    coerce_expression(Argument, Type).

aggregate_result_type(common, #sql_expression{ type = Type }) -> Type;
aggregate_result_type(number, Argument) -> numeric_result_type([Argument]);
aggregate_result_type(Type, _Argument) -> Type.

aggregate_separator(undefined, _State, Term) ->
    {undefined, Term};
aggregate_separator(Separator, State, Term0) ->
    {Separator0, Term1} = expression_to_sql(Separator, State, Term0),
    Separator1 = coerce_expression(Separator0, text),
    {expression_sql(Separator1), Term1}.

aggregate_expression(Function, Distinct, Argument, Separator, Type, State, Term) ->
    case z_sparql_sql_aggregate:to_sql(
        Function, Distinct, Argument, Separator, State#sql_state.context)
    of
        {ok, Sql} ->
            {#sql_expression{
                sql = Sql,
                type = Type,
                source = expression
            }, Term};
        {error, Reason} ->
            throw({error, Reason})
    end.

%% @doc COUNT(DISTINCT *) applies DISTINCT to complete mappings. Use only
%% variables from the graph pattern; SELECT aliases are added after the
%% bindings are done and are not part of the aggregate input.
solution_row_sql(Bindings) ->
    Expressions = [
        solution_binding_sql(Binding)
        || {{var, _Name}, Binding} <- lists:sort(maps:to_list(Bindings))
    ],
    [<<"ROW(">>, lists:join(<<", ">>, Expressions), $)].

solution_binding_sql({resource, Alias}) ->
    column_expression(Alias, <<"id">>);
solution_binding_sql({value, Expression}) ->
    expression_sql(Expression).

%% @doc Operators determine their input types bottom-up. PostgreSQL determines
%% the type of query arguments from the operator and the other operand.
binary_expression(Operator, Left, Right)
    when Operator =:= 'or'; Operator =:= 'and' ->
    Left1 = coerce_expression(Left, boolean),
    Right1 = coerce_expression(Right, boolean),
    operator_expression(Operator, Left1, Right1, boolean);
binary_expression(Operator, Left, Right)
    when Operator =:= '+'; Operator =:= '-';
         Operator =:= '*'; Operator =:= '/' ->
    Left1 = coerce_expression(Left, number),
    Right1 = coerce_expression(Right, number),
    Type = numeric_result_type([Left1, Right1]),
    operator_expression(Operator, Left1, Right1, Type);
binary_expression(Operator, Left, Right)
    when Operator =:= '='; Operator =:= '!=';
         Operator =:= '<'; Operator =:= '>';
         Operator =:= '=<'; Operator =:= '>=' ->
    Type = common_expression_type([Left, Right]),
    Left1 = coerce_expression(Left, Type),
    Right1 = coerce_expression(Right, Type),
    operator_expression(Operator, Left1, Right1, boolean).

operator_expression(Operator, Left, Right, Type) ->
    #sql_expression{
        sql = [
            <<"(">>, expression_sql(Left), sql_operator(Operator), expression_sql(Right), <<")">>
        ],
        type = Type,
        source = expression
    }.

unary_expression('not', Expression) ->
    Expression1 = coerce_expression(Expression, boolean),
    #sql_expression{
        sql = [<<"NOT (">>, expression_sql(Expression1), <<")">>],
        type = boolean,
        source = expression
    };
unary_expression('u+', Expression) ->
    Expression1 = coerce_expression(Expression, number),
    Expression1#sql_expression{ source = expression };
unary_expression('u-', Expression) ->
    Expression1 = coerce_expression(Expression, number),
    #sql_expression{
        sql = [<<"-(">>, expression_sql(Expression1), <<")">>],
        type = Expression1#sql_expression.type,
        source = expression
    }.

coerce_arguments(ArgumentTypes, Arguments, CommonType) ->
    [
        coerce_expression(Argument, resolve_argument_type(Type, CommonType))
        || {Type, Argument} <- lists:zip(ArgumentTypes, Arguments)
    ].

resolve_argument_type(common, CommonType) -> CommonType;
resolve_argument_type(Type, _CommonType) -> Type.

function_result_type(common, _Arguments, any) -> text;
function_result_type(common, _Arguments, CommonType) -> CommonType;
function_result_type(number, Arguments, _CommonType) -> numeric_result_type(Arguments);
function_result_type(Type, _Arguments, _CommonType) -> Type.

common_function_type(ArgumentTypes, Arguments) ->
    CommonArguments = [
        Argument
        || {common, Argument} <- lists:zip(ArgumentTypes, Arguments)
    ],
    common_expression_type(CommonArguments).

common_expression_type([]) -> any;
common_expression_type(Expressions) ->
    Types = [
        Type
        || #sql_expression{ type = Type, source = Source } <- Expressions,
           Source =/= argument
    ],
    common_types(Types).

common_types([]) -> any;
common_types([Type | Rest]) ->
    lists:foldl(fun common_type/2, Type, Rest).

common_type(Type, Type) -> Type;
common_type(Type, any) -> Type;
common_type(any, Type) -> Type;
common_type(float, Type) when Type =:= integer; Type =:= number -> float;
common_type(Type, float) when Type =:= integer; Type =:= number -> float;
common_type(number, integer) -> number;
common_type(integer, number) -> number;
common_type(text, uri) -> text;
common_type(uri, text) -> text;
common_type(integer, id) -> integer;
common_type(id, integer) -> integer;
common_type(Type, Acc) ->
    throw({error, {incompatible_types, Acc, Type}}).

numeric_result_type(Expressions) ->
    Types0 = [
        Type
        || #sql_expression{ type = Type, source = Source } <- Expressions,
           Source =/= argument
    ],
    Types = case Types0 of
        [] -> [ Type || #sql_expression{ type = Type } <- Expressions ];
        _ -> Types0
    end,
    case lists:member(float, Types) of
        true -> float;
        false ->
            case lists:member(number, Types) of
                true -> number;
                false -> integer
            end
    end.

coerce_expression(Expression, any) ->
    Expression;
coerce_expression(#sql_expression{ type = any } = Expression, Type) ->
    Expression#sql_expression{ type = Type };
coerce_expression(#sql_expression{ source = argument } = Expression, _Type) ->
    Expression;
coerce_expression(#sql_expression{ type = Actual } = Expression, Expected) ->
    case compatible_type(Actual, Expected) of
        true -> coerce_expression_1(Expression, coercion_type(Actual, Expected));
        false -> throw({error, {incompatible_types, Expected, Actual}})
    end.

coerce_expression_1(#sql_expression{ source = jsonb, sql = Sql } = Expression, Type) ->
    Expression#sql_expression{
        sql = jsonb_value_sql(Sql, Type),
        type = Type,
        source = expression
    };
coerce_expression_1(Expression, _Type) ->
    Expression.

coercion_type(Type, number) when Type =:= integer; Type =:= float -> Type;
coercion_type(_Type, Expected) -> Expected.

compatible_type(_Actual, any) -> true;
compatible_type(Type, Type) -> true;
compatible_type(Type, number) when Type =:= integer; Type =:= float -> true;
compatible_type(number, Type) when Type =:= integer; Type =:= float -> true;
compatible_type(integer, float) -> true;
compatible_type(float, integer) -> true;
compatible_type(uri, text) -> true;
compatible_type(text, uri) -> true;
compatible_type(id, integer) -> true;
compatible_type(integer, id) -> true;
compatible_type(_, _) -> false.

%% @doc The operator #>> '{}' maps to a text without JSON string quotes. Numeric
%% and boolean JSONB scalars can be cast directly; datetime goes via its text value.
jsonb_value_sql(Sql, text) -> [<<"(">>, Sql, <<" #>> '{}')">>];
jsonb_value_sql(Sql, uri) -> [<<"(">>, Sql, <<" #>> '{}')">>];
jsonb_value_sql(Sql, datetime) -> [<<"(">>, Sql, <<" #>> '{}')::timestamptz">>];
jsonb_value_sql(Sql, integer) -> [<<"(">>, Sql, <<")::bigint">>];
jsonb_value_sql(Sql, id) -> [<<"(">>, Sql, <<")::bigint">>];
jsonb_value_sql(Sql, float) -> [<<"(">>, Sql, <<")::double precision">>];
jsonb_value_sql(Sql, number) -> [<<"(">>, Sql, <<")::numeric">>];
jsonb_value_sql(Sql, boolean) -> [<<"(">>, Sql, <<")::boolean">>];
jsonb_value_sql(Sql, Type) ->
    throw({error, {unsupported_jsonb_type, Type, Sql}}).

known_type_test(isliteral, Type)
    when Type =:= text; Type =:= integer; Type =:= float;
         Type =:= number; Type =:= boolean; Type =:= datetime;
         Type =:= id -> true;
known_type_test(isliteral, Type)
    when Type =:= uri; Type =:= ids; Type =:= list;
         Type =:= fts; Type =:= fulltext -> false;
known_type_test(isnumeric, Type)
    when Type =:= integer; Type =:= float; Type =:= number; Type =:= id -> true;
known_type_test(isnumeric, Type)
    when Type =:= text; Type =:= boolean; Type =:= datetime;
         Type =:= uri; Type =:= ids; Type =:= list;
         Type =:= fts; Type =:= fulltext -> false;
known_type_test(_Function, _Type) -> undefined.

constant_expression(Value) ->
    #sql_expression{
        sql = atom_to_binary(Value, utf8),
        type = boolean,
        source = expression
    }.

boolean_expression(Sql) ->
    #sql_expression{
        sql = Sql,
        type = boolean,
        source = expression
    }.

float_expression(Sql) ->
    #sql_expression{
        sql = Sql,
        type = float,
        source = expression
    }.

expression_sql(#sql_expression{ sql = Sql }) -> Sql.

expression_defined_sql(#sql_expression{ defined = true }) -> <<"true">>;
expression_defined_sql(#sql_expression{ defined = false }) -> <<"false">>;
expression_defined_sql(#sql_expression{ defined = Defined }) -> Defined.

projection_term(#{ select := Select, distinct := Distinct, root := Root } = Plan, State0) ->
    SelectItems0 = case Select of
        all -> lists:sort([
            Variable
            || {var, _} = Variable <- maps:keys(State0#sql_state.bindings)
        ]);
        _ -> Select
    end,
    IsGrouped = is_grouped_query(Plan),
    % For a normal resource query the root is already selected as rsc.id by
    % z_search_terms. Grouped queries have their own root, so we can remove
    % the default rsc.id select.
    % TODO: check if we can remove the rsc.id from the record definition, as
    %       this is a bit of a hack.
    SelectItems = if
        IsGrouped -> SelectItems0;
        true -> lists:delete(Root, SelectItems0)
    end,
    Extra = if
        IsGrouped -> [no_default_select];
        true -> []
    end,
    {SelectExpressions, Term0, State1} = projection_expressions(SelectItems, State0, empty_term(), 1, []),
    Select1 = case {Distinct, SelectExpressions} of
        {distinct, [First | Rest]} -> [[<<"DISTINCT ">>, First] | Rest];
        _ -> SelectExpressions
    end,
    {Term0#search_sql_term{ select = Select1, extra = Extra }, State1}.

projection_expressions([], State, Term, _Nr, Acc) ->
    {lists:reverse(Acc), Term, State};
projection_expressions([{var, _} = Variable | Rest], State, Term0, Nr, Acc) ->
    {Expression, Term1} = projection_variable(Variable, State, Term0),
    ColumnAlias = <<"sparql_", (integer_to_binary(Nr))/binary>>,
    SelectExpression = [Expression, <<" AS ">>, ColumnAlias],
    projection_expressions(Rest, State, Term1, Nr + 1, [SelectExpression | Acc]);
projection_expressions([{as, Expression, Variable} | Rest], State0, Term0, Nr, Acc) ->
    {Expression1, Term1} = expression_to_sql(Expression, State0, Term0),
    State1 = bind_projection(Variable, Expression1, State0),
    ColumnAlias = <<"sparql_", (integer_to_binary(Nr))/binary>>,
    SelectExpression = [expression_sql(Expression1), <<" AS ">>, ColumnAlias],
    projection_expressions(Rest, State1, Term1, Nr + 1, [SelectExpression | Acc]).

projection_variable(Variable, State, Term) ->
    case maps:find(Variable, State#sql_state.bindings) of
        {ok, {resource, Alias}} ->
            {column_expression(Alias, <<"id">>), Term};
        {ok, {value, BoundExpression}} ->
            {expression_sql(BoundExpression), Term};
        error ->
            {Expression, Term1} = argument_to_expression(Variable, State, Term),
            {expression_sql(Expression), Term1}
    end.

bind_projection(Variable, Expression, #sql_state{ bindings = Bindings } = State) ->
    case maps:is_key(Variable, Bindings) of
        true -> throw({error, {variable_already_bound, Variable}});
        false -> State#sql_state{ bindings = Bindings#{ Variable => {value, Expression} } }
    end.

is_grouped_query(#{ group_by := [_ | _] }) ->
    true;
is_grouped_query(Plan) ->
    has_aggregate([
        maps:get(select, Plan),
        maps:get(having, Plan),
        maps:get(order_by, Plan)
    ]).

has_aggregate({aggregate, _Function, _Distinct, _Argument, _Separator}) -> true;
has_aggregate(Value) when is_tuple(Value) -> has_aggregate(tuple_to_list(Value));
has_aggregate(Value) when is_list(Value) -> lists:any(fun has_aggregate/1, Value);
has_aggregate(_Value) -> false.

group_terms([], _State) ->
    [];
group_terms(GroupBy, State) ->
    {Expressions0, Term} = expressions_to_term(GroupBy, State),
    Expressions = [ expression_sql(Expression) || Expression <- Expressions0 ],
    [Term#search_sql_term{ group_by = Expressions }].

having_terms([], _State) ->
    [];
having_terms(Having, State) ->
    {Expressions, Term} = expressions_to_term(Having, State),
    Expressions1 = [
        expression_sql(coerce_expression(Expression, boolean))
        || Expression <- Expressions
    ],
    [Term#search_sql_term{ having = Expressions1 }].

expressions_to_term(Expressions, State) ->
    expressions_to_term(Expressions, State, empty_term(), []).

expressions_to_term([], _State, Term, Acc) ->
    {lists:reverse(Acc), Term};
expressions_to_term([Expression | Rest], State, Term0, Acc) ->
    {Expression1, Term1} = expression_to_sql(Expression, State, Term0),
    expressions_to_term(Rest, State, Term1, [Expression1 | Acc]).

order_terms([], _State) ->
    [];
order_terms(Orders, State) ->
    {Sort, Term} = order_expressions(Orders, State, empty_term(), []),
    [Term#search_sql_term{ sort = Sort }].

order_expressions([], _State, Term, Acc) ->
    {lists:reverse(Acc), Term};
order_expressions([{order, Direction, Expression} | Rest], State, Term0, Acc) ->
    {Expression1, Term1} = expression_to_sql(Expression, State, Term0),
    Sort = [expression_sql(Expression1), order_direction(Direction)],
    order_expressions(Rest, State, Term1, [Sort | Acc]).

order_direction(default) -> <<" ASC">>;
order_direction(asc) -> <<" ASC">>;
order_direction(desc) -> <<" DESC">>.

sql_operator('or') -> <<" OR ">>;
sql_operator('and') -> <<" AND ">>;
sql_operator('=') -> <<" = ">>;
sql_operator('!=') -> <<" <> ">>;
sql_operator('<') -> <<" < ">>;
sql_operator('>') -> <<" > ">>;
sql_operator('=<') -> <<" <= ">>;
sql_operator('>=') -> <<" >= ">>;
sql_operator('+') -> <<" + ">>;
sql_operator('-') -> <<" - ">>;
sql_operator('*') -> <<" * ">>;
sql_operator('/') -> <<" / ">>.


jsonb_expression(Alias, Column, Selector) ->
    Path = jsonb_path(Selector),
    Arguments = lists:join(<<", ">>, [sql_string(PathPart) || PathPart <- Path]),
    [
        column_expression(Alias, Column),
        <<" #> ARRAY[">>, Arguments, <<"]::text[]">>
    ].

jsonb_path(Selector) when is_binary(Selector), Selector =/= <<>> ->
    [Selector];
jsonb_path([PathPart | _] = Selector) when is_binary(PathPart) ->
    Selector;
jsonb_path(Selector) ->
    throw({error, {invalid_jsonb_selector, Selector}}).

sql_string(Value) ->
    Escaped = binary:replace(Value, <<"'">>, <<"''">>, [global]),
    <<"'", Escaped/binary, "'">>.

column_expression(Alias, Column) ->
    [Alias, <<".">>, Column].

column_value(undefined, undefined, Value, _Context) ->
    Value;
column_value(Table, Column, Value, Context) ->
    case z_db:to_column_value(Table, Column, Value, Context) of
        {ok, Value1} -> Value1;
        {error, Reason} -> throw({error, {invalid_column_value, Table, Column, Reason}})
    end.

normalize_type(int) -> integer;
normalize_type(integer) -> integer;
normalize_type(float) -> float;
normalize_type(bool) -> boolean;
normalize_type(boolean) -> boolean;
normalize_type(binary) -> text;
normalize_type(text) -> text;
normalize_type(html) -> text;
normalize_type(language) -> text;
normalize_type(email) -> text;
normalize_type(unsafe) -> text;
normalize_type(datetime) -> datetime;
normalize_type(uri) -> uri;
normalize_type(id) -> id;
normalize_type(ids) -> ids;
normalize_type(list) -> list;
normalize_type(fts) -> fts;
normalize_type(fulltext) -> fulltext;
normalize_type(Type) -> throw({error, {invalid_mapping_type, Type}}).

rdf_typed_value({literal, Value, Datatype, _Language}) ->
    Type = z_sparql_sql_datatype:datatype_type(Datatype),
    {literal_value(Type, Value), Type};
rdf_typed_value({integer, Value}) ->
    {binary_to_integer(Value), integer};
rdf_typed_value({decimal, Value}) ->
    {z_convert:to_float(Value), float};
rdf_typed_value({double, Value}) ->
    {z_convert:to_float(Value), float};
rdf_typed_value(Boolean) when is_boolean(Boolean) ->
    {Boolean, boolean};
rdf_typed_value({iri, Iri}) ->
    {Iri, uri};
rdf_typed_value(Value) ->
    throw({error, {expected_value, Value}}).

literal_value(integer, Value) -> z_convert:to_integer(Value);
literal_value(float, Value) -> z_convert:to_float(Value);
literal_value(datetime, Value) ->
    case z_convert:to_datetime(Value) of
        undefined -> throw({error, {invalid_datetime, Value}});
        DateTime -> DateTime
    end;
literal_value(boolean, <<"true">>) -> true;
literal_value(boolean, <<"1">>) -> true;
literal_value(boolean, <<"false">>) -> false;
literal_value(boolean, <<"0">>) -> false;
literal_value(boolean, Value) -> throw({error, {invalid_boolean, Value}});
literal_value(_Type, Value) -> Value.

rdf_value({literal, Value, _Datatype, _Language}) -> Value;
rdf_value({integer, Value}) -> Value;
rdf_value({decimal, Value}) -> Value;
rdf_value({double, Value}) -> Value;
rdf_value(Boolean) when is_boolean(Boolean) -> Boolean;
rdf_value(Value) -> throw({error, {expected_value, Value}}).

rdf_json_value({literal, Value, _Datatype, _Language}) -> Value;
rdf_json_value({integer, Value}) -> binary_to_integer(Value);
rdf_json_value({decimal, Value}) -> z_convert:to_float(Value);
rdf_json_value({double, Value}) -> z_convert:to_float(Value);
rdf_json_value(Boolean) when is_boolean(Boolean) -> Boolean;
rdf_json_value(Value) -> throw({error, {expected_value, Value}}).

new_alias(Prefix, #sql_state{ alias_nr = AliasNr } = State) ->
    Alias = <<"sparql_", Prefix/binary, "_", (integer_to_binary(AliasNr))/binary>>,
    {Alias, State#sql_state{ alias_nr = AliasNr + 1 }}.

empty_term() ->
    #search_sql_term{ select = [] }.

add_table(<<"rsc">>, _Table, Term) ->
    Term;
add_table(Alias, Table, #search_sql_term{ tables = Tables } = Term) ->
    Term#search_sql_term{ tables = Tables#{ Alias => Table } }.

add_where(Where, #search_sql_term{ where = [] } = Term) ->
    Term#search_sql_term{ where = [Where] };
add_where(Where, #search_sql_term{ where = [Where0] } = Term) ->
    Term#search_sql_term{
        where = [[<<"(">>, Where0, <<" AND ">>, Where, <<")">>]]
    }.

add_arg(Value, #search_sql_term{ args = Args } = Term) ->
    Name = list_to_atom([$$ | integer_to_list(length(Args) + 1)]),
    {Name, Term#search_sql_term{ args = Args ++ [Value] }}.

allof([]) ->
    #search_sql_term{ select = [], where = [<<"true">>] };
allof([Term]) ->
    Term;
allof(Terms) ->
    #search_sql_nested{ operator = <<"allof">>, terms = Terms }.

union_branches_to_sql([], _InitialState, State) ->
    {[], [], State};
union_branches_to_sql([Branch | Rest], InitialState, State0) ->
    BranchState0 = InitialState#sql_state{ alias_nr = State0#sql_state.alias_nr },
    {Terms, BranchState1} = pattern_to_sql(Branch, BranchState0),
    {RestTerms, RestStates, State1} = union_branches_to_sql(Rest, InitialState, BranchState1),
    {[allof(Terms) | RestTerms], [BranchState1 | RestStates], State1}.

common_branch_bindings([], State) ->
    State;
common_branch_bindings(States, #sql_state{ bindings = InitialBindings } = State) ->
    Common = maps:filter(
        fun(Variable, Binding) ->
            lists:all(
                fun(BranchState) ->
                    maps:get(Variable, BranchState#sql_state.bindings, undefined) =:= Binding
                end,
                States)
        end,
        (hd(States))#sql_state.bindings),
    State#sql_state{ bindings = maps:merge(InitialBindings, Common) }.
