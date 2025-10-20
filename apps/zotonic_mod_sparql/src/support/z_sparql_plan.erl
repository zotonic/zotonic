%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Normalize a parsed SPARQL SELECT query to a query plan
%% that can then be used to make SQL fragments for a SQL query.
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

-module(z_sparql_plan).

-export([
    to_query_plan/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").
-include_lib("zotonic_rdf/include/zotonic_rdf.hrl").

-type variable() :: {var, binary()}.

-type rdf_term() ::
      variable()
    | {iri, binary()}
    | {literal, binary(), binary() | undefined, binary() | undefined}
    | {integer, binary()}
    | {decimal, binary()}
    | {double, binary()}
    | boolean()
    | {bnode, binary()}
    | nil.

-type predicate_mapping() ::
      category
    | subclass
    | {column, binary(), binary()}
    | {jsonb, binary(), binary(), term()}
    | {edge, term(), boolean()}
    | undefined.

-type predicate_plan() :: #{
    iri := binary(),
    ns := binary(),
    ns_prefix := binary(),
    predicate := binary(),
    mapping := predicate_mapping()
}.

-type triple_plan() :: {triple, rdf_term(), predicate_plan() | variable(), rdf_term()}.

-type query_pattern() ::
      identity
    | triple_plan()
    | {join, query_pattern(), query_pattern()}
    | {left_join, query_pattern(), query_pattern(), none | term()}
    | {union, [ query_pattern() ]}
    | {filter, term(), query_pattern()}
    | {extend, variable(), term(), query_pattern()}
    | {values, variable(), [ rdf_term() ]}
    | {graph, rdf_term(), query_pattern()}.

-type query_plan() :: #{
    type := select,
    distinct := default | distinct | reduced,
    select := all | [ variable() ],
    dataset := list(),
    where := query_pattern(),
    root := variable() | undefined,
    group_by := [ variable() ],
    order_by := list(),
    limit := non_neg_integer() | undefined,
    offset := non_neg_integer() | undefined
}.

-export_type([
    query_plan/0,
    query_pattern/0,
    variable/0,
    triple_plan/0,
    rdf_term/0,
    predicate_mapping/0,
    predicate_plan/0
]).

-record(plan_state, {
    base = undefined :: binary() | undefined,
    namespaces = #{} :: #{ binary() := binary() },
    blank_nr = 0 :: non_neg_integer(),
    context :: z:context()          % just to make the calls a bit more compact
}).


%% @doc Make a normalized query plan from a parsed SPARQL SELECT query.
%% Predicates in the plan contain the result of the sparql_mapping notification.
%% Namespaces are normalized to known prefixes (if any) using the rdf_ns notifications.
-spec to_query_plan(ParsedQuery, Context) -> {ok, QueryPlan} | {error, Reason} when
    ParsedQuery :: term(),
    Context :: z:context(),
    QueryPlan :: query_plan(),
    Reason :: term().
to_query_plan({query, Prologue, {select, Distinct, Select, Dataset, Where, SolutionModifier}}, Context) ->
    try
        State0 = map_prologue(Prologue, #plan_state{ context = Context }),
        {Dataset1, State1} = map_dataset(Dataset, State0),
        {Where1, State2} = map_group(Where, State1),
        {GroupBy, OrderBy, Limit, Offset, _State3} = map_solution_modifier(SolutionModifier, State2),
        Root = find_root_variable(Select, Where1),
        {ok, #{
            type => select,
            distinct => Distinct,
            select => Select,
            dataset => Dataset1,
            where => Where1,
            root => Root,
            group_by => GroupBy,
            order_by => OrderBy,
            limit => Limit,
            offset => Offset
        }}
    catch
        throw:{error, Reason} ->
            {error, Reason}
    end;
to_query_plan({query, _Prologue, Query}, _Context) when is_tuple(Query), tuple_size(Query) > 0 ->
    {error, {unsupported_query, element(1, Query)}};
to_query_plan(Query, _Context) ->
    {error, {invalid_query, Query}}.


%% @doc Collect all namespaces from the prologue.
map_prologue([], State) ->
    State;
map_prologue([{base, Iri} | Rest], State) ->
    Base = resolve_iri(Iri, State#plan_state.base),
    map_prologue(Rest, State#plan_state{ base = Base });
map_prologue([{prefix, Prefix, Iri} | Rest], State) ->
    Namespace = resolve_iri(Iri, State#plan_state.base),
    Prefix1 = trim_prefix(Prefix),
    Namespaces = (State#plan_state.namespaces)#{ Prefix1 => Namespace },
    map_prologue(Rest, State#plan_state{ namespaces = Namespaces });
map_prologue([Declaration | _], _State) ->
    throw({error, {invalid_prologue, Declaration}}).

map_dataset([], State) ->
    {[], State};
map_dataset([{from, Iri} | Rest], State0) ->
    {Iri1, State1} = map_term(Iri, State0),
    {Rest1, State2} = map_dataset(Rest, State1),
    {[{from, Iri1} | Rest1], State2};
map_dataset([{from_named, Iri} | Rest], State0) ->
    {Iri1, State1} = map_term(Iri, State0),
    {Rest1, State2} = map_dataset(Rest, State1),
    {[{from_named, Iri1} | Rest1], State2}.

map_solution_modifier({solution_modifier, GroupBy, OrderBy, LimitOffset}, State0) ->
    {GroupBy1, State1} = map_expressions(GroupBy, State0),
    {OrderBy1, State2} = map_order(OrderBy, State1),
    {GroupBy1, OrderBy1,
     limit_offset(limit, LimitOffset),
     limit_offset(offset, LimitOffset), State2};
map_solution_modifier(SolutionModifier, _State) ->
    throw({error, {invalid_solution_modifier, SolutionModifier}}).

map_order([], State) ->
    {[], State};
map_order([{order, Direction, Expression} | Rest], State0) ->
    {Expression1, State1} = map_expression(Expression, State0),
    {Rest1, State2} = map_order(Rest, State1),
    {[{order, Direction, Expression1} | Rest1], State2}.

limit_offset(Name, LimitOffset) ->
    case proplists:get_value(Name, LimitOffset) of
        undefined -> undefined;
        Value -> to_non_negative_integer(Name, Value)
    end.

to_non_negative_integer(_Name, Value) when is_integer(Value), Value >= 0 ->
    Value;
to_non_negative_integer(Name, Value) when is_binary(Value) ->
    try
        z_convert:to_integer(Value)
    of
        Integer when Integer >= 0 -> Integer;
        _ -> throw({error, {invalid_modifier, Name, Value}})
    catch
        error:badarg -> throw({error, {invalid_modifier, Name, Value}})
    end;
to_non_negative_integer(Name, Value) ->
    throw({error, {invalid_modifier, Name, Value}}).

map_group({group, Patterns}, State0) ->
    {Pattern, Filters, State1} = map_patterns(Patterns, identity, [], State0),
    {add_filters(Filters, Pattern), State1};
map_group(Group, _State) ->
    throw({error, {invalid_group, Group}}).

map_patterns([], Plan, Filters, State) ->
    {Plan, lists:reverse(Filters), State};
map_patterns([{triple_pattern, Triple} | Rest], Plan, Filters, State0) ->
    {Triples, State1} = map_triple_pattern(Triple, State0),
    Plan1 = lists:foldl(fun join_pattern/2, Plan, Triples),
    map_patterns(Rest, Plan1, Filters, State1);
map_patterns([{optional, Group} | Rest], Plan, Filters, State0) ->
    {Optional, State1} = map_group(Group, State0),
    Plan1 = {left_join, Plan, Optional, none},
    map_patterns(Rest, Plan1, Filters, State1);
map_patterns([{union, Groups} | Rest], Plan, Filters, State0) ->
    {Branches, State1} = map_groups(Groups, State0),
    Plan1 = join_pattern({union, Branches}, Plan),
    map_patterns(Rest, Plan1, Filters, State1);
map_patterns([{group, _} = Group | Rest], Plan, Filters, State0) ->
    {Group1, State1} = map_group(Group, State0),
    map_patterns(Rest, join_pattern(Group1, Plan), Filters, State1);
map_patterns([{filter, Expression} | Rest], Plan, Filters, State0) ->
    {Expression1, State1} = map_expression(Expression, State0),
    map_patterns(Rest, Plan, [Expression1 | Filters], State1);
map_patterns([{bind, Expression, Variable} | Rest], Plan, Filters, State0) ->
    {Expression1, State1} = map_expression(Expression, State0),
    Plan1 = {extend, Variable, Expression1, Plan},
    map_patterns(Rest, Plan1, Filters, State1);
map_patterns([{values, Variable, Values} | Rest], Plan, Filters, State0) ->
    {Values1, State1} = map_terms(Values, State0),
    Plan1 = join_pattern({values, Variable, Values1}, Plan),
    map_patterns(Rest, Plan1, Filters, State1);
map_patterns([{graph, Graph, Group} | Rest], Plan, Filters, State0) ->
    {Graph1, State1} = map_term(Graph, State0),
    {Group1, State2} = map_group(Group, State1),
    Plan1 = join_pattern({graph, Graph1, Group1}, Plan),
    map_patterns(Rest, Plan1, Filters, State2);
map_patterns([Pattern | _], _Plan, _Filters, _State) ->
    throw({error, {invalid_graph_pattern, Pattern}}).

map_groups([], State) ->
    {[], State};
map_groups([Group | Rest], State0) ->
    {Group1, State1} = map_group(Group, State0),
    {Rest1, State2} = map_groups(Rest, State1),
    {[Group1 | Rest1], State2}.

join_pattern(identity, Plan) -> Plan;
join_pattern(Pattern, identity) -> Pattern;
join_pattern(Pattern, Plan) -> {join, Plan, Pattern}.

add_filters([], Plan) ->
    Plan;
add_filters([Filter | Rest], Plan) ->
    add_filters(Rest, {filter, Filter, Plan}).

map_triple_pattern({subject, Subject, Predicates}, State0) ->
    {Subject1, State1} = map_term(Subject, State0),
    map_predicates(Predicates, Subject1, State1);
map_triple_pattern(Triple, _State) ->
    throw({error, {invalid_triple, Triple}}).

map_predicates([], _Subject, State) ->
    {[], State};
map_predicates([{predicate, Predicate, Objects} | Rest], Subject, State0) ->
    Predicate1 = map_predicate(Predicate, State0),
    {Objects1, State1} = map_terms(Objects, State0),
    Triples = [ {triple, Subject, Predicate1, Object} || Object <- Objects1 ],
    {Rest1, State2} = map_predicates(Rest, Subject, State1),
    {Triples ++ Rest1, State2}.

map_predicate({var, _} = Variable, _State) ->
    Variable;
map_predicate(rdf_type, State) ->
    map_predicate_iri(?NS_RDF, <<"type">>, State);
map_predicate({pname, PName}, State) ->
    {Prefix, LocalName} = split_pname(PName),
    case maps:find(Prefix, State#plan_state.namespaces) of
        {ok, Namespace} ->
            map_predicate_iri(Namespace, LocalName, State);
        error ->
            throw({error, {unknown_prefix, Prefix}})
    end;
map_predicate({iri, Iri}, State) ->
    FullIri = resolve_iri(Iri, State#plan_state.base),
    {Namespace, LocalName} = split_namespace(FullIri),
    map_predicate_iri(Namespace, LocalName, State);
map_predicate(Predicate, _State) ->
    throw({error, {invalid_predicate, Predicate}}).

map_predicate_iri(Namespace, LocalName, State) ->
    Context = State#plan_state.context,
    NamespacePrefix = namespace_prefix(Namespace, Context),
    Mapping = predicate_mapping(Namespace, NamespacePrefix, LocalName, Context),
    #{
        iri => <<Namespace/binary, LocalName/binary>>,
        ns => Namespace,
        ns_prefix => NamespacePrefix,
        predicate => LocalName,
        mapping => Mapping
    }.

namespace_prefix(Namespace, Context) ->
    case z_notifier:first(#rdf_ns{ ns = Namespace }, Context) of
        {ok, Prefix} when is_binary(Prefix) ->
            Prefix;
        {error, Reason} ->
            throw({error, Reason});
        undefined ->
            Namespace
    end.

predicate_mapping(Namespace, NamespacePrefix, LocalName, Context) ->
    Notification = #sparql_mapping{
        ns = Namespace,
        ns_prefix = NamespacePrefix,
        predicate = LocalName
    },
    case z_notifier:first(Notification, Context) of
        {ok, category} ->
            category;
        {ok, subclass} ->
            subclass;
        {ok, {column, Table, Column}} ->
            z_db:assert_table_name(Table),
            z_db:assert_column_name(Column),
            {column, Table, Column};
        {ok, {jsonb, Table, Column, Selector}} ->
            z_db:assert_table_name(Table),
            z_db:assert_column_name(Column),
            % TODO: sanitize selector???
            {jsonb, Table, Column, Selector};
        {ok, {edge, Predicate, IsReversed}} when is_boolean(IsReversed) ->
            {edge, Predicate, IsReversed};
        {error, Reason} ->
            throw({error, Reason});
        undefined ->
            undefined
    end.

map_terms([], State) ->
    {[], State};
map_terms([Term | Rest], State0) ->
    {Term1, State1} = map_term(Term, State0),
    {Rest1, State2} = map_terms(Rest, State1),
    {[Term1 | Rest1], State2}.

map_term({var, _} = Variable, State) ->
    {Variable, State};
map_term({iri, Iri}, State) ->
    {{iri, resolve_iri(Iri, State#plan_state.base)}, State};
map_term({pname, PName}, State) ->
    {{iri, expand_pname(PName, State#plan_state.namespaces)}, State};
map_term({literal, Value}, State) ->
    {{literal, Value, undefined, undefined}, State};
map_term({literal_lang, Value, Language}, State) ->
    {{literal, Value, undefined, normalize_language(Language)}, State};
map_term({literal_dt, Value, Datatype}, State0) ->
    {Datatype1, State1} = map_term(Datatype, State0),
    case Datatype1 of
        {iri, DatatypeIri} ->
            {{literal, Value, DatatypeIri, undefined}, State1};
        _ ->
            throw({error, {invalid_datatype, Datatype}})
    end;
map_term({integer, _} = Integer, State) ->
    {Integer, State};
map_term({decimal, _} = Decimal, State) ->
    {Decimal, State};
map_term({double, _} = Double, State) ->
    {Double, State};
map_term(Boolean, State) when is_boolean(Boolean) ->
    {Boolean, State};
map_term({bnode, Name}, State) ->
    {{bnode, Name}, State};
map_term(anon, #plan_state{ blank_nr = BlankNr } = State) ->
    BlankNr1 = BlankNr + 1,
    Name = <<"anon", (integer_to_binary(BlankNr1))/binary>>,
    {{bnode, Name}, State#plan_state{ blank_nr = BlankNr1 }};
map_term(nil, State) ->
    {{iri, <<?NS_RDF/binary, "nil">>}, State};
map_term(Term, _State) ->
    throw({error, {invalid_term, Term}}).

map_expressions([], State) ->
    {[], State};
map_expressions([Expression | Rest], State0) ->
    {Expression1, State1} = map_expression(Expression, State0),
    {Rest1, State2} = map_expressions(Rest, State1),
    {[Expression1 | Rest1], State2}.

map_expression({Operator, Left, Right}, State0)
    when Operator =:= 'or'; Operator =:= 'and';
         Operator =:= '='; Operator =:= '!=';
         Operator =:= '<'; Operator =:= '>';
         Operator =:= '=<'; Operator =:= '>=';
         Operator =:= '+'; Operator =:= '-';
         Operator =:= '*'; Operator =:= '/' ->
    {Left1, State1} = map_expression(Left, State0),
    {Right1, State2} = map_expression(Right, State1),
    {{Operator, Left1, Right1}, State2};
map_expression({Operator, Expression}, State0)
    when Operator =:= 'not'; Operator =:= 'u+'; Operator =:= 'u-' ->
    {Expression1, State1} = map_expression(Expression, State0),
    {{Operator, Expression1}, State1};
map_expression({call, Function, Arguments}, State0) when is_atom(Function) ->
    {Arguments1, State1} = map_expressions(Arguments, State0),
    {{call, Function, Arguments1}, State1};
map_expression({call, Function, Arguments}, State0) ->
    {Function1, State1} = map_term(Function, State0),
    {Arguments1, State2} = map_expressions(Arguments, State1),
    {{call, Function1, Arguments1}, State2};
map_expression(Expression, State) ->
    map_term(Expression, State).


find_root_variable(all, Pattern) ->
    first(resource_variables(Pattern));
find_root_variable(Select, Pattern) ->
    ResourceVariables = resource_variables(Pattern),
    first_selected_resource(Select, ResourceVariables).

first_selected_resource([], ResourceVariables) ->
    first(ResourceVariables);
first_selected_resource([Variable | Rest], ResourceVariables) ->
    case lists:member(Variable, ResourceVariables) of
        true -> Variable;
        false -> first_selected_resource(Rest, ResourceVariables)
    end.

first([Value | _]) -> Value;
first([]) -> undefined.

resource_variables(identity) -> [];
resource_variables({triple, Subject, Predicate, Object}) ->
    SubjectVariables = term_variables(Subject),
    ObjectVariables = case Predicate of
        #{ mapping := {edge, _, _} } ->
            term_variables(Object);
        #{ iri := Iri } ->
            RdfType = <<?NS_RDF/binary, "type">>,
            if
                Iri =:= RdfType -> term_variables(Object);
                true -> []
            end;
        _ ->
            []
    end,
    SubjectVariables ++ ObjectVariables;
resource_variables({join, Left, Right}) ->
    resource_variables(Left) ++ resource_variables(Right);
resource_variables({left_join, Left, Right, _Expression}) ->
    resource_variables(Left) ++ resource_variables(Right);
resource_variables({union, Branches}) ->
    lists:append([ resource_variables(Branch) || Branch <- Branches ]);
resource_variables({filter, _Expression, Pattern}) ->
    resource_variables(Pattern);
resource_variables({extend, _Variable, _Expression, Pattern}) ->
    resource_variables(Pattern);
resource_variables({values, _Variable, _Values}) -> [];
resource_variables({graph, Graph, Pattern}) ->
    term_variables(Graph) ++ resource_variables(Pattern).

term_variables({var, _} = Variable) -> [Variable];
term_variables(_) -> [].

%% @doc Resove an IRI against a base url, remove '..' etc.
resolve_iri(Iri, Base) when is_binary(Iri) ->
    try uri_string:parse(Iri) of
        #{ scheme := _ } ->
            Iri;
        _ when Base =:= undefined ->
            throw({error, {relative_iri, Iri}});
        _ ->
            unicode:characters_to_binary(uri_string:resolve(Iri, Base))
    catch
        error:Reason -> throw({error, {invalid_iri, Iri, Reason}})
    end.

%% @doc Expand a prefixed name using the namespaces defined in the query.
expand_pname(PName, Namespaces) ->
    {Prefix, LocalName} = split_pname(PName),
    case maps:find(Prefix, Namespaces) of
        {ok, Namespace} -> <<Namespace/binary, LocalName/binary>>;
        error -> throw({error, {unknown_prefix, Prefix}})
    end.

%% @doc Split a prefixed name into its namespace and local (predicate) name.
split_pname(PName) ->
    case binary:split(PName, <<":">>) of
        [Prefix, LocalName] -> {Prefix, LocalName};
        [_] -> throw({error, {invalid_prefixed_name, PName}})
    end.

%% @doc Remove the trailing ':' from a prefix, throw if missing.
trim_prefix(Prefix) ->
    case binary:last(Prefix) of
        $: -> binary:part(Prefix, 0, size(Prefix) - 1);
        _ -> throw({error, {invalid_prefix, Prefix}})
    end.

%% @doc Lowercase the language, as Zotonic uses lowercased language strings.
%% @todo: we should normalize the language to one configured (if present)
normalize_language(<<$@, Language/binary>>) -> z_string:to_lower(Language);
normalize_language(Language) -> z_string:to_lower(Language).


split_namespace(Iri) ->
    Matches = binary:matches(Iri, [<<"#">>, <<"/">>, <<":">>]),
    case Matches of
        [] ->
            {Iri, <<>>};
        _ ->
            {Position, Length} = lists:last(Matches),
            NamespaceLength = Position + Length,
            {
                binary:part(Iri, 0, NamespaceLength),
                binary:part(Iri, NamespaceLength, size(Iri) - NamespaceLength)
            }
    end.
