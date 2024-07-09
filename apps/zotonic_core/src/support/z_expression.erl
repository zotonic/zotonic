%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Expression parsing and evaluation.

-module(z_expression).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    parse/1,
    eval/3
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
    {expr, z_convert:to_atom(Op), simplify(Left), simplify(Right)};
simplify({expr, {Op, _}, Expr}) ->
    {expr, z_convert:to_atom(Op), simplify(Expr)};
simplify({expr, E}) ->
    simplify(E);
% simplify({identifier, _, <<"m">>}) ->
%     m;
simplify({identifier,_,Name}) ->
    {variable, Name};
simplify({number_literal, _, Val}) ->
    z_convert:to_integer(Val);
simplify({string_literal, _, Val}) ->
    Val;
simplify({attribute, {identifier,_,Attr}, From}) ->
    {attribute, z_convert:to_atom(Attr), simplify(From)};
simplify({index_value, Array, Index}) ->
    {index_value, simplify(Array), simplify(Index)};
simplify({value_list, List}) ->
    {value_list, [ simplify(Elt) || Elt <- List ]};
simplify({apply_filter, Expr, {filter, {identifier,_,Filter}, Args}}) ->
    {apply_filter,
        z_convert:to_atom(<<"filter_", Filter/binary>>),
        z_convert:to_atom(Filter),
        simplify(Expr),
        [ simplify(Arg) || Arg <- Args ]};
simplify({value, _, Expr, []}) ->
    simplify(Expr).



%% @doc Evaluate a parsed expression tree.
-spec eval(Tree, Vars, Context) -> Value when
    Tree :: tree(),
    Vars :: proplists:proplist()
          | #{ binary() => term() }
          | fun( (binary()|atom()) -> term() ),
    Context :: z:context(),
    Value :: term().
eval(Tree, Vars, Context) ->
    eval1(Tree, Vars, Context).

eval1({expr, Op, Left, Right}, Vars, Context) ->
    template_compiler_operators:Op(eval1(Left, Vars, Context), eval1(Right, Vars, Context), z_template_compiler_runtime, Context);
eval1({expr, Op, Expr}, Vars, Context) ->
    template_compiler_operators:Op(eval1(Expr, Vars, Context), z_template_compiler_runtime, Context);
eval1({variable, Name}, Vars, Context) ->
    z_template_compiler_runtime:find_value(Name, Vars, #{}, Context);
eval1({index_value, Array, Index}, Vars, Context) ->
    z_template_compiler_runtime:find_value(eval1(Index, Vars, Context), eval1(Array, Vars, Context), #{}, Context);
eval1({attribute, Attr, From}, Vars, Context) ->
    z_template_compiler_runtime:find_value(Attr, eval1(From, Vars, Context), #{}, Context);
eval1({value_list, List}, Vars, Context) ->
    [ eval1(Elt, Vars, Context) || Elt <- List ];
eval1({apply_filter, filter_default, _Func, Expr, Args}, Vars, Context) ->
    A = eval1(Expr, Vars, Context),
    case A of
        Empty when Empty =:= undefined; Empty =:= []; Empty =:= <<>> ->
            case Args of
                [B|_] -> eval1(B, Vars, Context);
                _ -> undefined
            end;
        _ -> A
    end;
eval1({apply_filter, IfNone, _Func, Expr, Args}, Vars, Context)
    when IfNone =:= filter_if_undefined; IfNone =:= filter_if_none ->
    case eval1(Expr, Vars, Context) of
        undefined ->
            case Args of
                [B|_] -> eval1(B, Vars, Context);
                _ -> undefined
            end;
        A -> A
    end;
eval1({apply_filter, Mod, Func, Expr, Args}, Vars, Context) ->
    EvalArgs = [ eval1(Arg, Vars, Context) || Arg <- Args],
    EvalExpr = eval1(Expr, Vars, Context),
    erlang:apply(Mod, Func, [EvalExpr | EvalArgs] ++[Context]);
eval1({find_value, Ks}, Vars, Context) ->
    find_value(Ks, Vars, Context);
eval1(Val, _Vars, _Context) ->
    Val.

find_value([ K | Ks ], Vars, Context) ->
    V = eval1(K, Vars, Context),
    find_value_1(V, Ks, Vars, Context).

find_value_1(V, [], _Vars, _Context) ->
    V;
find_value_1(V, Ks, Vars, Context) when is_integer(V); is_binary(V); is_atom(V) ->
    find_rsc_prop(V, Ks, Vars, Context);
find_value_1([ V | _ ], [ {variable, _} | _ ] = Ks, Vars, Context) ->
    find_value_1(V, Ks, Vars, Context);
find_value_1([ _ | _ ] = V, [ {expr, _} = E | Ks ], Vars, Context) ->
    V1 = case eval1(E, Vars, Context) of
        N when is_integer(N) -> nth(N, V);
        Index -> z_template_compiler_runtime:find_value(V, Index, #{}, Context)
    end,
    find_value_1(V1, Ks, Vars, Context);
find_value_1([ _ | _ ] = V, [ N | Ks ], Vars, Context) when is_integer(N) ->
    V1 = nth(N, V),
    find_value_1(V1, Ks, Vars, Context);
find_value_1(#{} = V, [ Index | _ ] = Ks, Vars, Context) ->
    Index1 = eval1(Index, Vars, Context),
    V1 = z_template_compiler_runtime:find_value(V, Index1, #{}, Context),
    find_value_1(V1, Ks, Vars, Context);
find_value_1(_, _, _Vars, _Context) ->
    undefined.


find_rsc_prop(V, [ {variable, <<"o">>}, {variable, Pred} | Ks ], Vars, Context) ->
    V1 = m_edge:objects(V, Pred, Context),
    find_value_1(V1, Ks, Vars, Context);
find_rsc_prop(V, [ {variable, <<"s">>}, {variable, Pred} | Ks ], Vars, Context) ->
    V1 = m_edge:subjects(V, Pred, Context),
    find_value_1(V1, Ks, Vars, Context);
find_rsc_prop(V, [ {variable, Var} | Ks ], Vars, Context) ->
    V1 = m_rsc:p(V, Var, Context),
    find_value_1(V1, Ks, Vars, Context);
find_rsc_prop(V, [ {expr, _} = E | Ks ], Vars, Context) ->
    V1 = case eval1(E, Vars, Context) of
        1 -> V;
        N when is_integer(N) -> undefined;
        P -> m_rsc:p(V, P, Context)
    end,
    find_value_1(V1, Ks, Vars, Context).


nth(1, [V|_]) -> V;
nth(N, [_|Vs]) when N > 1 -> nth(N-1, Vs);
nth(_, _) -> undefined.
