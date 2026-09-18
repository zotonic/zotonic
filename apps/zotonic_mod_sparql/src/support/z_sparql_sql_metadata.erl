%% @copyright 2026 Marc Worrell
%% @doc RDF term metadata carried beside SQL values. Each component is a SQL
%% text expression; NULL means unknown or not applicable. It is deliberately
%% independent of the storage/evaluation type used for SQL coercion.
-module(z_sparql_sql_metadata).

-export([unknown/0, iri/0, literal/2, from_term/1, from_type/1, jsonb/2,
    choose/3, map/2, is_static/1, numeric/3]).
-export_type([metadata/0]).

-type metadata() :: #{kind := term(), datatype := term(), language := term()}.
-define(XSD, "http://www.w3.org/2001/XMLSchema#").
-define(LANG_STRING, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#langString">>).

-spec unknown() -> metadata().
unknown() -> #{kind => <<"NULL">>, datatype => <<"NULL">>, language => <<"NULL">>}.

-spec iri() -> metadata().
iri() -> (unknown())#{kind => quote(<<"iri">>)}.

-spec literal(Datatype, Language) -> metadata() when
    Datatype :: binary() | undefined, Language :: binary() | undefined.
literal(_Datatype, Language) when is_binary(Language), Language =/= <<>> ->
    #{kind => quote(<<"literal">>), datatype => quote(?LANG_STRING),
        language => quote(z_string:to_lower(Language))};
literal(undefined, _Language) -> literal(<<?XSD, "string">>, undefined);
literal(Datatype, _Language) ->
    #{kind => quote(<<"literal">>), datatype => quote(Datatype), language => <<"NULL">>}.

-spec from_term(Term) -> metadata() when Term :: term().
from_term({literal, _, Datatype, Language}) -> literal(Datatype, Language);
from_term({integer, _}) -> from_type(integer);
from_term({decimal, _}) -> from_type(number);
from_term({double, _}) -> from_type(float);
from_term({iri, _}) -> iri();
from_term({bnode, _}) -> (unknown())#{kind => quote(<<"bnode">>)};
from_term(Value) when is_boolean(Value) -> from_type(boolean);
from_term(_) -> unknown().

%% Mapped scalar values and Erlang arguments use canonical RDF datatypes.
%% A resource id binding explicitly uses iri/0, not from_type(id).
-spec from_type(Type) -> metadata() when Type :: atom().
from_type(integer) -> literal(<<?XSD, "integer">>, undefined);
from_type(id) -> from_type(integer);
from_type(number) -> literal(<<?XSD, "decimal">>, undefined);
from_type(float) -> literal(<<?XSD, "double">>, undefined);
from_type(boolean) -> literal(<<?XSD, "boolean">>, undefined);
from_type(datetime) -> literal(<<?XSD, "dateTime">>, undefined);
from_type(text) -> literal(<<?XSD, "string">>, undefined);
from_type(fulltext) -> from_type(text);
from_type(uri) -> iri();
from_type(_) -> unknown().

%% JSONB has no original RDF datatype. Scalar shape takes precedence over
%% name-derived hints. Numeric/date hints refine compatible scalar shapes;
%% otherwise JSON numbers use xsd:decimal. Tagged objects/arrays stay unknown.
-spec jsonb(Sql, Type) -> metadata() when Sql :: term(), Type :: atom().
jsonb(Sql, Type) ->
    Shape = [<<"jsonb_typeof(">>, Sql, $)],
    String = case Type of uri -> iri(); datetime -> from_type(datetime); _ -> from_type(text) end,
    Number = case Type of
        integer -> jsonb_integer(Sql);
        id -> jsonb_integer(Sql);
        float -> from_type(float);
        _ -> from_type(number)
    end,
    Object = [$(, Shape, <<" = 'object' AND NOT (">>, Sql, <<" ? '_type'))">>],
    choose([Shape, <<" = 'string'">>], String,
        choose([Shape, <<" = 'number'">>], Number,
            choose([Shape, <<" = 'boolean'">>], from_type(boolean),
                choose(Object, (unknown())#{kind => quote(<<"bnode">>)}, unknown())))).

jsonb_integer(Sql) ->
    Scalar = [<<"(">>, Sql, <<" #>> '{}')::numeric">>],
    choose([Scalar, <<" = trunc(">>, Scalar, $)], from_type(integer), from_type(number)).

-spec choose(Condition, Left, Right) -> metadata() when
    Condition :: term(), Left :: metadata(), Right :: metadata().
choose(Condition, Left, Right) ->
    maps:map(fun(Key, Value) ->
        case maps:get(Key, Right) of
            Value -> Value;
            Other -> [<<"CASE WHEN ">>, Condition, <<" THEN ">>, Value,
                <<" ELSE ">>, Other, <<" END">>]
        end
    end, Left).

-spec map(Fun, Metadata) -> metadata() when
    Fun :: fun((term()) -> term()), Metadata :: metadata().
map(Fun, Metadata) -> maps:map(fun(_Key, Value) -> Fun(Value) end, Metadata).

-spec is_static(Metadata) -> boolean() when Metadata :: metadata().
is_static(Metadata) -> lists:all(fun is_binary/1, maps:values(Metadata)).

%% Arithmetic promotes RDF numeric types independently of PostgreSQL storage.
%% Unknown/custom operands remain unknown instead of acquiring a false type.
-spec numeric(Operator, Left, Right) -> metadata() when
    Operator :: atom(), Left :: metadata(), Right :: metadata().
numeric(Operator, Left, Right) ->
    L = maps:get(datatype, Left),
    R = maps:get(datatype, Right),
    case {constant_rank(L), constant_rank(R)} of
        {LR, RR} when is_integer(LR), is_integer(RR) ->
            numeric_result(Operator, max(LR, RR));
        {unknown, _} -> unknown();
        {_, unknown} -> unknown();
        _ -> numeric_expression(Operator, L, R)
    end.

numeric_result(_Operator, 4) -> from_type(float);
numeric_result(_Operator, 3) -> literal(<<?XSD, "float">>, undefined);
numeric_result(_Operator, 2) -> from_type(number);
numeric_result('/', 1) -> from_type(number);
numeric_result(_Operator, 1) -> from_type(integer).

constant_rank(Datatype) when is_binary(Datatype) ->
    case Datatype of
        <<"'", ?XSD, "double'">> -> 4;
        <<"'", ?XSD, "float'">> -> 3;
        <<"'", ?XSD, "decimal'">> -> 2;
        _ ->
            case lists:member(Datatype, integer_datatypes()) of
                true -> 1;
                false -> unknown
            end
    end;
constant_rank(_Datatype) -> dynamic.

numeric_expression(Operator, Left, Right) ->
    % Bind operand metadata once. Repeating a nested datatype expression in
    % each CASE branch would make a chain of arithmetic expressions grow
    % exponentially. These are private aliases in a scalar subquery.
    L = <<"rdf_numeric.l">>,
    R = <<"rdf_numeric.r">>,
    Rank = [<<"GREATEST(">>, L, <<", ">>, R, $)],
    IntegerType = maps:get(datatype, numeric_result(Operator, 1)),
    Datatype = [<<"(SELECT CASE WHEN ">>, L, <<" IS NULL OR ">>, R,
        <<" IS NULL THEN NULL WHEN ">>, Rank, <<" = 4 THEN '", ?XSD,
          "double' WHEN ">>, Rank, <<" = 3 THEN '", ?XSD,
          "float' WHEN ">>, Rank, <<" = 2 THEN '", ?XSD,
          "decimal' ELSE ">>, IntegerType,
        <<" END FROM (VALUES (">>, numeric_rank(Left), <<", ">>, numeric_rank(Right),
        <<")) AS rdf_numeric(l, r))">>],
    #{kind => [<<"CASE WHEN ">>, Datatype, <<" IS NOT NULL THEN 'literal' ELSE NULL END">>],
      datatype => Datatype, language => <<"NULL">>}.

numeric_rank(Datatype) ->
    [<<"(SELECT CASE rdf_operand.datatype",
        " WHEN '", ?XSD, "double' THEN 4 WHEN '", ?XSD, "float' THEN 3 WHEN '",
        ?XSD, "decimal' THEN 2 ELSE CASE WHEN rdf_operand.datatype IN (">>,
        lists:join(<<", ">>, integer_datatypes()),
        <<") THEN 1 ELSE NULL END END FROM (VALUES (">>, Datatype,
        <<")) AS rdf_operand(datatype))">>].

integer_datatypes() ->
    Names = [<<"integer">>, <<"long">>, <<"int">>, <<"short">>, <<"byte">>,
        <<"nonPositiveInteger">>, <<"negativeInteger">>, <<"nonNegativeInteger">>,
        <<"positiveInteger">>, <<"unsignedLong">>, <<"unsignedInt">>,
        <<"unsignedShort">>, <<"unsignedByte">>],
    [quote(<<?XSD, Name/binary>>) || Name <- Names].

quote(Value) ->
    Escaped = binary:replace(Value, <<"'">>, <<"''">>, [global]),
    <<"'", Escaped/binary, "'">>.
