%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Project RDF bindings and serialize SPARQL Results JSON.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_sparql_results).
-moduledoc("Endpoint projections and SPARQL 1.1 Results JSON serialization.").

-export([projection/2, document/3, binding/5]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/z_sparql_sql.hrl").

%% Select only requested variables, including the root in its requested position.
%% Four SQL columns per variable retain dynamic RDF metadata without changing
%% the existing Zotonic search projection contract.
-spec projection(Plan, State) -> {#search_sql_term{}, #sql_state{}, [binary()]}
    when Plan :: map(), State :: #sql_state{}.
projection(#{select := Select, distinct := Distinct}, State) ->
    Items = case Select of
        all -> lists:sort([V || {var, _} = V <- maps:keys(State#sql_state.bindings)]);
        _ -> Select
    end,
    {Columns, Term, State1, Names} = project(Items, State, z_sparql_sql:empty_term(), 1),
    Columns1 = case {Distinct, Columns} of
        {distinct, [First | Rest]} -> [[<<"DISTINCT ">>, First] | Rest];
        _ -> Columns
    end,
    {Term#search_sql_term{select = Columns1, extra = [no_default_select]}, State1, Names}.

project([], State, Term, _Nr) -> {[], Term, State, []};
project([Item | Rest], State, Term, Nr) ->
    {Ast, {var, Name} = Variable} = case Item of
        {as, Expr, Var} -> {Expr, Var};
        {var, _} -> {Item, Item}
    end,
    ok = z_sparql_sql:validate_projection(Ast, State),
    {Expression, Term1} = z_sparql_sql:expression_to_sql(Ast, State, Term),
    State1 = case Item of
        {as, _, _} -> z_sparql_sql:bind_projection(Variable, Expression, Term1, State);
        _ -> State
    end,
    Metadata = z_sparql_sql:expression_metadata(Expression),
    Suffix = integer_to_binary(Nr),
    Columns = [[Expression#sql_expression.sql, <<" AS sparql_">>, Suffix]] ++ [
        [maps:get(Key, Metadata), <<" AS rdf_">>, atom_to_binary(Key, utf8), $_, Suffix]
        || Key <- [kind, datatype, language]
    ],
    {Tail, Term2, State2, Names} = project(Rest, State1, Term1, Nr + 1),
    {Columns ++ Tail, Term2, State2, [Name | Names]}.

-spec document(Variables, Rows, Context) -> map()
    when Variables :: [binary()], Rows :: [tuple()], Context :: z:context().
document(Variables, Rows, Context) ->
    #{<<"head">> => #{<<"vars">> => Variables},
      <<"results">> => #{<<"bindings">> => [row(Variables, tuple_to_list(R), Context) || R <- Rows]}}.

row([], [], _Context) -> #{};
row([Name | Names], [Value, Kind, Datatype, Language | Rest], Context) ->
    Tail = row(Names, Rest, Context),
    case binding(Value, Kind, Datatype, Language, Context) of
        undefined -> Tail;
        Binding -> Tail#{Name => Binding}
    end.

-spec binding(Value, Kind, Datatype, Language, Context) -> map() | undefined
    when Value :: term(), Kind :: binary() | undefined,
         Datatype :: binary() | undefined, Language :: binary() | undefined,
         Context :: z:context().
binding(undefined, _Kind, _Datatype, _Language, _Context) -> undefined;
binding(null, _Kind, _Datatype, _Language, _Context) -> undefined;
binding(#trans{tr = []}, _Kind, _Datatype, _Language, _Context) -> undefined;
binding(#trans{tr = Translations} = Trans, _Kind, _Datatype, _Language, Context) ->
    % Resolve the language first so the tag describes the actual fallback text.
    Language = z_trans:lookup_fallback_languages([Lang || {Lang, _} <- Translations], Context),
    Text = z_trans:lookup_fallback(Trans, Language, Context),
    binding(Text, <<"literal">>, undefined, atom_to_binary(Language, utf8), Context);
binding(Value, <<"iri">>, _Datatype, _Language, Context) when is_integer(Value) ->
    #{<<"type">> => <<"uri">>, <<"value">> => m_rsc:uri(Value, z_context:set_language('x-default', Context))};
binding(Value, <<"iri">>, _Datatype, _Language, _Context) when is_binary(Value) ->
    #{<<"type">> => <<"uri">>, <<"value">> => Value};
binding(Value, <<"bnode">>, _Datatype, _Language, _Context) ->
    % Match the compiler's structural identity for untagged JSON objects.
    Label = binary:encode_hex(crypto:hash(sha256, term_to_binary(Value, [deterministic]))),
    #{<<"type">> => <<"bnode">>, <<"value">> => <<"b", Label/binary>>};
binding(Value, <<"literal">>, Datatype, Language, _Context) ->
    Base = #{<<"type">> => <<"literal">>, <<"value">> => lexical(Value, Datatype)},
    case {Language, Datatype} of
        {Lang, _} when is_binary(Lang), Lang =/= <<>> -> Base#{<<"xml:lang">> => Lang};
        {_, Type} when is_binary(Type) -> Base#{<<"datatype">> => Type};
        _ -> Base
    end;
binding(_Value, _Kind, _Datatype, _Language, _Context) ->
    throw({error, unsupported_result_term}).

%% SQL can promote a mixed VALUES column to float while its individual terms
%% retain integer/decimal datatypes. Emit a lexical form valid for that datatype.
lexical(Value, <<"http://www.w3.org/2001/XMLSchema#decimal">>) when is_float(Value) ->
    decimal(float_to_binary(Value, [short]));
lexical(Value, Datatype) when is_float(Value) ->
    case z_sparql_sql_datatype:datatype_type(Datatype) of
        integer -> integer_to_binary(trunc(Value));
        _ -> lexical(Value)
    end;
lexical(Value, _Datatype) -> lexical(Value).

decimal(<<$-, Rest/binary>>) -> <<$-, (decimal(Rest))/binary>>;
decimal(Number) ->
    case binary:split(Number, <<"e">>) of
        [Mantissa, Exponent] ->
            [Whole, Fraction] = binary:split(Mantissa, <<".">>),
            Digits = <<Whole/binary, Fraction/binary>>,
            Point = byte_size(Whole) + binary_to_integer(Exponent),
            Size = byte_size(Digits),
            if
                Point =< 0 ->
                    <<"0.", (binary:copy(<<"0">>, -Point))/binary, Digits/binary>>;
                Point >= Size ->
                    <<Digits/binary, (binary:copy(<<"0">>, Point - Size))/binary>>;
                true ->
                    <<Left:Point/binary, Right/binary>> = Digits,
                    <<Left/binary, $., Right/binary>>
            end;
        [_] -> Number
    end.

lexical(Value) when is_binary(Value) -> Value;
lexical(Value) when is_integer(Value) -> integer_to_binary(Value);
lexical(Value) when is_float(Value) -> float_to_binary(Value, [short]);
lexical(true) -> <<"true">>;
lexical(false) -> <<"false">>;
lexical({{Y, M, D}, {H, I, S}}) ->
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, I, S]));
lexical({Y, M, D}) ->
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D]));
lexical(_) -> throw({error, unsupported_result_term}).
