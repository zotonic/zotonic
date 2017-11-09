%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Expression parsing and evaluation.

-module(z_expression).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    parse/1,
    eval/3
]).

-include("zotonic.hrl").


%% @doc Parse an expression to an expression tree.  Uses the template_compiler parser.
parse(Expr) when is_binary(Expr) ->
    case template_compiler_scanner:scan(<<"{{", Expr/binary, "}}">>) of
        {ok, Tokens} ->
            case template_compiler_parser:parse(Tokens) of
                {ok, {base, [Tree|_]}} -> {ok, simplify(Tree)};
                Err -> Err
            end;
        {error, _} = Error ->
            Error
    end;
parse(Expr) ->
    parse(z_convert:to_binary(Expr)).


simplify({find_value, [Value]}) ->
    simplify(Value);
simplify({expr, Op, Left, Right}) ->
    {expr, z_convert:to_atom(Op), simplify(Left), simplify(Right)};
simplify({expr, Op, Expr}) ->
    {expr, z_convert:to_atom(Op), simplify(Expr)};
simplify({identifier, _, <<"m">>}) ->
    m;
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
simplify({value, Expr, []}) ->
    simplify(Expr).



%% @doc Evaluate a parsed expression tree.
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
eval1(Val, _Vars, _Context) ->
    Val.
