%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2025 Marc Worrell
%% @doc Expression parsing and evaluation.
%% @end

%% Copyright 2011-2025 Marc Worrell
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

-module(z_expression).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    parse/1,
    eval/3,
    eval/4
]).

-type tree() :: number()
              | binary()
              | {find_value, [ tree() ]}
              | {expr, op(), tree(), tree()}
              | {expr, op(), tree()}
              | {variable, binary()}
              | {attribute, atom(), tree()}
              | {index_value, tree(), tree()}
              | {apply_filter, atom(), atom(), tree(), [ tree() ]}.

-type op() :: atom().

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Parse an expression to an expression tree.  Uses the template_compiler parser.
-spec parse(Expr) -> {ok, ParseTree} | {error, Reason} when
    Expr :: binary(),
    ParseTree :: tree(),
    Reason :: term().
parse(Expr) when is_binary(Expr) ->
    case template_compiler_scanner:scan(<<"{{", Expr/binary, "}}">>) of
        {ok, Tokens} ->
            case template_compiler_parser:parse(Tokens) of
                {ok, {base, [Tree|_]}} ->
                    {ok, simplify(Tree)};
                Err ->
                    Err
            end;
        {error, _} = Error ->
            Error
    end;
parse(Expr) ->
    parse(z_convert:to_binary(Expr)).


simplify({find_value, [Value]}) ->
    simplify(Value);
simplify({find_value, Vs}) ->
    {find_value, lists:map(fun simplify/1, Vs)};
simplify({expr, {Op, _}, Left, Right}) ->
    {expr, Op, simplify(Left), simplify(Right)};
simplify({expr, {Op, _}, Expr}) ->
    {expr, Op, simplify(Expr)};
simplify({expr, E}) ->
    simplify(E);
simplify({identifier,_,Name}) ->
    {variable, Name};
simplify({number_literal, _, Val}) ->
    z_convert:to_integer(Val);
simplify({string_literal, _, Val}) ->
    Val;
simplify({trans_literal, _, Val}) ->
    {trans_literal, Val};
simplify({attribute, {identifier,_,Attr}, From}) ->
    {attribute, Attr, simplify(From)};
simplify({index_value, Array, Index}) ->
    {index_value, simplify(Array), simplify(Index)};
simplify({value_list, List}) ->
    {value_list, [ simplify(Elt) || Elt <- List ]};
simplify({apply_filter, Expr, {filter, {identifier,_,Filter}, Args}}) ->
    try
        {apply_filter,
            binary_to_existing_atom(<<"filter_", Filter/binary>>, utf8),
            binary_to_existing_atom(Filter, utf8),
            simplify(Expr),
            [ simplify(Arg) || Arg <- Args ]}
    catch
        _:Reason:Stack ->
            ?LOG_WARNING(#{
                in => zotonic_mod_base,
                text => <<"Expression filter unknown, or error in filter">>,
                result => error,
                reason => Reason,
                filter => Filter,
                stack => Stack
            }),
            undefined
    end;
simplify({value, _, Expr, []}) ->
    simplify(Expr).



%% @doc Evaluate a parsed expression tree.
-spec eval(Tree, Vars, Context) -> Value when
    Tree :: tree(),
    Vars :: proplists:proplist()
          | #{ binary() => term() }
          | fun( (binary() ) -> term() )
          | fun( (binary(), z:context() ) -> term() ),
    Context :: z:context(),
    Value :: term().
eval(Tree, Vars, Context) ->
    eval(Tree, Vars, [], Context).

-spec eval(Tree, Vars, Options, Context) -> Value when
    Tree :: tree(),
    Vars :: proplists:proplist()
          | #{ binary() => term() }
          | fun( (binary()|atom()) -> term() ),
    Options :: [ Option ],
    Option :: {filters_allowed, [ atom() ]}
            | {p, fun( (term(), binary(), z:context()) -> term() )},
    Context :: z:context(),
    Value :: term().
eval(Tree, Vars, Options, Context) ->
    eval1(Tree, Vars, Options, Context).

eval1({expr, Op, Left, Right}, Vars, Options, Context) ->
    template_compiler_operators:Op(
        eval1(Left, Vars, Options, Context),
        eval1(Right, Vars, Options, Context),
        z_template_compiler_runtime,
        Context);
eval1({expr, Op, Expr}, Vars, Options, Context) ->
    template_compiler_operators:Op(eval1(Expr, Vars, Options, Context), z_template_compiler_runtime, Context);
eval1({variable, Name}, Vars, Options, Context) ->
    case z_template_compiler_runtime:find_value(Name, Vars, #{}, Context) of
        undefined ->
            RscId = z_template_compiler_runtime:find_value(<<"id">>, Vars, #{}, Context),
            Id = m_rsc:rid(RscId, Context),
            p(Id, Name, Options, Context);
        Value ->
            Value
    end;
eval1({index_value, Array, Index}, Vars, Options, Context) ->
    z_template_compiler_runtime:find_value(
        eval1(Index, Vars, Options, Context),
        eval1(Array, Vars, Options, Context),
        #{},
        Context);
eval1({attribute, Attr, From}, Vars, Options, Context) ->
    z_template_compiler_runtime:find_value(
        Attr,
        eval1(From, Vars, Options, Context),
        #{},
        Context);
eval1({value_list, List}, Vars, Options, Context) ->
    [ eval1(Elt, Vars, Options, Context) || Elt <- List ];
eval1({apply_filter, filter_default, _Func, Expr, Args}, Vars, Options, Context) ->
    A = eval1(Expr, Vars, Options, Context),
    case A of
        Empty when Empty =:= undefined; Empty =:= []; Empty =:= <<>> ->
            case Args of
                [B|_] -> eval1(B, Vars, Options, Context);
                _ -> undefined
            end;
        _ -> A
    end;
eval1({apply_filter, IfNone, _Func, Expr, Args}, Vars, Options, Context)
    when IfNone =:= filter_if_undefined; IfNone =:= filter_if_none ->
    case eval1(Expr, Vars, Options, Context) of
        undefined ->
            case Args of
                [B|_] -> eval1(B, Vars, Options, Context);
                _ -> undefined
            end;
        A -> A
    end;
eval1({apply_filter, Mod, Func, Expr, Args}, Vars, Options, Context) ->
    case is_filter_allowed(Func, Options) of
        true ->
            EvalArgs = [ eval1(Arg, Vars, Options, Context) || Arg <- Args],
            EvalExpr = eval1(Expr, Vars, Options, Context),
            erlang:apply(Mod, Func, [EvalExpr | EvalArgs] ++[Context]);
        false ->
            ?LOG_WARNING(#{
                in => zotonic_mod_base,
                text => <<"Filter not allowed">>,
                result => error,
                filter => Func
            }),
            undefined
    end;
eval1({find_value, Ks}, Vars, Options, Context) ->
    find_value(Ks, Vars, Options, Context);
eval1({trans_literal, Text}, _Vars, _Options, Context) ->
    z_trans:trans(Text, Context);
eval1(Val, _Vars, _Options, _Context) ->
    Val.

find_value([ K | Ks ], Vars, Options, Context) ->
    V = eval1(K, Vars, Options, Context),
    find_value_1(V, Ks, Vars, Options, Context).

find_value_1(V, [], _Vars, _Options, _Context) ->
    V;
find_value_1(V, Ks, Vars, Options, Context) when is_integer(V); is_binary(V); is_atom(V) ->
    find_rsc_prop(V, Ks, Vars, Options, Context);
find_value_1([ V | _ ], [ {variable, _} | _ ] = Ks, Vars, Options, Context) ->
    find_value_1(V, Ks, Vars, Options, Context);
find_value_1([ _ | _ ] = V, [ {expr, _} = E | Ks ], Vars, Options, Context) ->
    V1 = case eval1(E, Vars, Options, Context) of
        N when is_integer(N) -> nth(N, V);
        Index -> z_template_compiler_runtime:find_value(Index, V, #{}, Context)
    end,
    find_value_1(V1, Ks, Vars, Options, Context);
find_value_1([ _ | _ ] = V, [ N | Ks ], Vars, Options, Context) when is_integer(N) ->
    V1 = nth(N, V),
    find_value_1(V1, Ks, Vars, Options, Context);
find_value_1(#{} = V, [ {variable, Var} | Ks ], Vars, Options, Context) ->
    V1 = z_template_compiler_runtime:find_value(Var, V, #{}, Context),
    find_value_1(V1, Ks, Vars, Options, Context);
find_value_1(#{} = V, [ Index | _ ] = Ks, Vars, Options, Context) ->
    V1 = z_template_compiler_runtime:find_value(Index, V, #{}, Context),
    find_value_1(V1, Ks, Vars, Options, Context);
find_value_1(_, _, _Vars, _Options, _Context) ->
    undefined.


find_rsc_prop(V, [ {variable, <<"o">>}, {variable, Pred} | Ks ], Vars, Options, Context) ->
    V1 = m_edge:objects(V, Pred, Context),
    find_value_1(V1, Ks, Vars, Options, Context);
find_rsc_prop(V, [ {variable, <<"s">>}, {variable, Pred} | Ks ], Vars, Options, Context) ->
    V1 = m_edge:subjects(V, Pred, Context),
    find_value_1(V1, Ks, Vars, Options, Context);
find_rsc_prop(V, [ {variable, Var} | Ks ], Vars, Options, Context) ->
    V1 = p(V, Var, Options, Context),
    find_value_1(V1, Ks, Vars, Options, Context);
find_rsc_prop(V, [ {expr, _} = E | Ks ], Vars, Options, Context) ->
    V1 = case eval1(E, Vars, Options, Context) of
        1 -> V;
        N when is_integer(N) -> undefined;
        P -> p(V, P, Options, Context)
    end,
    find_value_1(V1, Ks, Vars, Options, Context).

p(undefined, _Prop, _Options, _Context) ->
    undefined;
p(Id, Prop, Options, Context) ->
    case proplists:get_value(p, Options) of
        F when is_function(F, 3) ->
            F(Id, Prop, Context);
        undefined ->
            m_rsc:p(Id, Prop, Context)
    end.

is_filter_allowed(<<"as_atom">>, _Options) ->
    ?LOG_WARNING(#{
        in => zotonic_mod_base,
        text => <<"Filter as_atom is not allowed in free expressions">>,
        result => error,
        filter => <<"as_atom">>
    }),
    undefined;
is_filter_allowed(Filter, Options) ->
    case proplists:get_value(filters_allowed, Options) of
        undefined -> true;
        Allowed when is_list(Allowed) -> lists:member(Filter, Allowed);
        _ -> false
    end.

nth(1, [V|_]) -> V;
nth(N, [_|Vs]) when N > 1 -> nth(N-1, Vs);
nth(_, _) -> undefined.
