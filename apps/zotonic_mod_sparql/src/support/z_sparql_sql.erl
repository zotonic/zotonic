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
    to_sql_term/2
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
    source :: argument | column | expression | jsonb
}).

-export_type([ sql_term/0 ]).

-record(sql_state, {
    bindings = #{} :: map(),
    alias_nr = 1 :: pos_integer(),
    context :: z:context()
}).


%% @doc Map a parsed SPARQL SELECT query to nested Zotonic SQL search terms.
-spec to_sql_term(ParsedQuery, Context) -> {ok, [ sql_term() ]} | {error, Reason} when
    ParsedQuery :: term(),
    Context :: z:context(),
    Reason :: term().
to_sql_term(ParsedQuery, Context) ->
    % First map the parsed query to a query plan, normalizing values and expressions.
    case z_sparql_plan:to_query_plan(ParsedQuery, Context) of
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
    dataset := [],
    group_by := [],
    limit := undefined,
    offset := undefined,
    root := {var, RootName},
    where := Pattern
} = Plan, Context) ->
    State0 = #sql_state{
        bindings = #{{var, RootName} => {resource, <<"rsc">>}},
        context = Context
    },
    try
        {Terms, State1} = pattern_to_sql(Pattern, State0),
        Projection = projection_term(Plan, State1),
        OrderTerms = order_terms(maps:get(order_by, Plan), State1),
        {ok, Terms ++ [Projection] ++ OrderTerms}
    catch
        throw:{error, Reason} ->
            {error, Reason}
    end;
query_plan_to_sql(#{ dataset := [_ | _] }, _Context) ->
    {error, {unsupported, dataset}};
query_plan_to_sql(#{ group_by := [_ | _] }, _Context) ->
    {error, {unsupported, group_by}};
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
pattern_to_sql({values, _, _}, _State) ->
    throw({error, {unsupported, values}});
pattern_to_sql({graph, _, _}, _State) ->
    throw({error, {unsupported, graph}}).

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
            case m_rsc:is_a(CategoryId, category, Context) of
                true ->
                    CategoryIds = category_ids(CategoryId, Context),
                    {CategoryArg, Term1} = add_arg(CategoryIds, Term0),
                    Where = [SubjectAlias, <<".category_id = ANY(">>, CategoryArg, <<"::int[])">>],
                    {[add_where(Where, Term1)], State};
                false ->
                    throw({error, {not_a_category, Iri}})
            end
    end;
mapped_triple_to_sql(category, _SubjectAlias, Object, _Term, _State) ->
    throw({error, {expected_category, Object}});
mapped_triple_to_sql(subclass, SubjectAlias, {iri, Iri}, Term0, State) ->
    Context = State#sql_state.context,
    case m_rsc:uri_lookup(Iri, Context) of
        undefined ->
            {[add_where(<<"false">>, Term0)], State};
        CategoryId ->
            case m_rsc:is_a(CategoryId, category, Context) of
                true ->
                    CategoryIds = lists:delete(CategoryId, category_ids(CategoryId, Context)),
                    {CategoryArg, Term1} = add_arg(CategoryIds, Term0),
                    Where = [SubjectAlias, <<".id = ANY(">>, CategoryArg, <<"::int[])">>],
                    {[add_where(Where, Term1)], State};
                false ->
                    throw({error, {not_a_category, Iri}})
            end
    end;
mapped_triple_to_sql(subclass, _SubjectAlias, Object, _Term, _State) ->
    throw({error, {expected_category, Object}}).

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
    resource_binding_alias(Variable, Term, State);
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

property_alias(<<"rsc">>, SubjectAlias, Term, State) ->
    {SubjectAlias, Term, State};
property_alias(Table, SubjectAlias, Term, State0) ->
    {Alias, State1} = new_alias(<<"prop">>, State0),
    On = [Alias, <<".id = ">>, SubjectAlias, <<".id">>],
    JoinInner = (Term#search_sql_term.join_inner)#{ Alias => {Table, On} },
    {Alias, Term#search_sql_term{ join_inner = JoinInner }, State1}.

bind_object({var, _} = Variable, Kind, Expression, _Table, _Column, Term,
        #sql_state{ bindings = Bindings } = State) ->
    case maps:find(Variable, Bindings) of
        {ok, {Kind, #sql_expression{} = BoundExpression}} ->
            EqualExpression = binary_expression('=', Expression, BoundExpression),
            {add_where(expression_sql(EqualExpression), Term), State};
        {ok, _OtherKind} ->
            throw({error, {incompatible_variable, Variable}});
        error ->
            Bindings1 = Bindings#{ Variable => {Kind, Expression} },
            Where = [expression_sql(Expression), <<" IS NOT NULL">>],
            {add_where(Where, Term), State#sql_state{ bindings = Bindings1 }}
    end;
bind_object(Object, value, Expression, Table, Column, Term0, State) ->
    Value = rdf_value(Object),
    Value1 = column_value(Table, Column, Value, State#sql_state.context),
    {Arg, Term1} = add_arg(Value1, Term0),
    {add_where([expression_sql(Expression), <<" = ">>, Arg], Term1), State};
bind_object(Object, resource, _Expression, _Table, _Column, _Term, _State) ->
    throw({error, {expected_resource, Object}}).

bind_jsonb_object({var, _} = Variable, Expression, Term, State) ->
    bind_object(Variable, value, Expression, undefined, undefined, Term, State);
bind_jsonb_object(Object, Expression, Term0, State) ->
    Value = ?DB_PROPS_JSON(rdf_json_value(Object)),
    {Arg, Term1} = add_arg(Value, Term0),
    {add_where([expression_sql(Expression), <<" = ">>, Arg, <<"::jsonb">>], Term1), State}.

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
        error -> throw({error, {unbound_variable, Variable}})
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
expression_to_sql({call, Function, [Argument]}, State, Term0)
    when Function =:= isliteral; Function =:= isnumeric ->
    type_test_to_sql(Function, Argument, State, Term0);
expression_to_sql({call, sameterm, [Left, Right]}, State, Term0) ->
    same_term_to_sql(Left, Right, State, Term0);
expression_to_sql({call, Function, Arguments}, State, Term0) ->
    {Arguments1, Term1} = expression_list_to_sql(Arguments, State, Term0),
    function_to_sql(Function, Arguments1, Term1);
expression_to_sql(Value, _State, Term0) ->
    {ArgumentValue, Type} = rdf_typed_value(Value),
    {Arg, Term1} = add_arg(ArgumentValue, Term0),
    {#sql_expression{ sql = Arg, type = Type, source = argument }, Term1}.

%% @doc If we know we have a resource, the we know the type is
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

expression_list_to_sql([], _State, Term) ->
    {[], Term};
expression_list_to_sql([Expression | Rest], State, Term0) ->
    {Expression1, Term1} = expression_to_sql(Expression, State, Term0),
    {Rest1, Term2} = expression_list_to_sql(Rest, State, Term1),
    {[Expression1 | Rest1], Term2}.

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
            <<"(">>, expression_sql(Left), sql_operator(Operator),
            expression_sql(Right), <<")">>
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

expression_sql(#sql_expression{ sql = Sql }) -> Sql.

projection_term(#{ select := Select, distinct := Distinct, root := Root }, State) ->
    Variables0 = case Select of
        all -> lists:sort([
            Variable
            || {var, _} = Variable <- maps:keys(State#sql_state.bindings)
        ]);
        _ -> Select
    end,
    % The root resource is already selected as rsc.id by z_search_terms.
    Variables = lists:delete(Root, Variables0),
    SelectExpressions = projection_expressions(Variables, State, 1, []),
    Select1 = case {Distinct, SelectExpressions} of
        {distinct, [First | Rest]} -> [[<<"DISTINCT ">>, First] | Rest];
        _ -> SelectExpressions
    end,
    (empty_term())#search_sql_term{ select = Select1 }.

projection_expressions([], _State, _Nr, Acc) ->
    lists:reverse(Acc);
projection_expressions([{var, _} = Variable | Rest], State, Nr, Acc) ->
    Expression = case maps:find(Variable, State#sql_state.bindings) of
        {ok, {resource, Alias}} -> column_expression(Alias, <<"id">>);
        {ok, {value, BoundExpression}} -> expression_sql(BoundExpression);
        error -> throw({error, {unbound_variable, Variable}})
    end,
    ColumnAlias = <<"sparql_", (integer_to_binary(Nr))/binary>>,
    SelectExpression = [Expression, <<" AS ">>, ColumnAlias],
    projection_expressions(Rest, State, Nr + 1, [SelectExpression | Acc]).

order_terms([], _State) ->
    [];
order_terms(Orders, State) ->
    Sort = [ order_expression(Order, State) || Order <- Orders ],
    [(empty_term())#search_sql_term{ sort = Sort }].

order_expression({order, Direction, {var, _} = Variable}, State) ->
    Expression = case maps:find(Variable, State#sql_state.bindings) of
        {ok, {resource, Alias}} -> column_expression(Alias, <<"id">>);
        {ok, {value, BoundExpression}} -> expression_sql(BoundExpression);
        error -> throw({error, {unbound_variable, Variable}})
    end,
    [Expression, order_direction(Direction)];
order_expression({order, _Direction, Expression}, _State) ->
    throw({error, {unsupported_order_expression, Expression}}).

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
    Type = datatype_type(Datatype),
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

datatype_type(undefined) -> text;
datatype_type(<<"http://www.w3.org/2001/XMLSchema#", Name/binary>>) ->
    xsd_type(Name);
datatype_type(_Datatype) -> text.

xsd_type(Name)
    when Name =:= <<"integer">>;
         Name =:= <<"long">>;
         Name =:= <<"int">>;
         Name =:= <<"short">>;
         Name =:= <<"byte">>;
         Name =:= <<"nonPositiveInteger">>;
         Name =:= <<"negativeInteger">>;
         Name =:= <<"nonNegativeInteger">>;
         Name =:= <<"positiveInteger">>;
         Name =:= <<"unsignedLong">>;
         Name =:= <<"unsignedInt">>;
         Name =:= <<"unsignedShort">>;
         Name =:= <<"unsignedByte">> -> integer;
xsd_type(Name)
    when Name =:= <<"decimal">>;
         Name =:= <<"double">>;
         Name =:= <<"float">> -> float;
xsd_type(<<"boolean">>) -> boolean;
xsd_type(Name)
    when Name =:= <<"dateTime">>;
         Name =:= <<"dateTimeStamp">>;
         Name =:= <<"date">> -> datetime;
xsd_type(<<"anyURI">>) -> uri;
xsd_type(_Name) -> text.

literal_value(integer, Value) -> z_convert:to_integer(Value);
literal_value(float, Value) -> z_convert:to_float(Value);
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
