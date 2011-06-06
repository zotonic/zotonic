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


%% @doc Parse an expression to an expression tree.  Uses the erlydtl parser.
parse(Expr) when is_binary(Expr) ->
    parse(binary_to_list(Expr));
parse(Expr) ->
    case erlydtl_scanner:scan("{{" ++ Expr ++ "}}") of
        {ok, Tokens} ->
            case erlydtl_parser:parse(Tokens) of
                {ok, [Tree|_]} -> {ok, simplify(Tree)};
                Err -> Err
            end;
        Err ->
            Err
    end.        

    simplify({expr, Op, Left, Right}) ->
        {expr, list_to_atom(Op), simplify(Left), simplify(Right)};
    simplify({expr, Op, Expr}) ->
        {expr, list_to_atom(Op), simplify(Expr)};
    simplify({variable, {identifier,_,"m"}}) ->
        m;
    simplify({variable, {identifier,_,Name}}) ->
        {variable, Name};
    simplify({number_literal, _, Val}) ->
        list_to_integer(Val);
    simplify({string_literal, _, Val}) ->
        Val;
    simplify({attribute, {identifier,_,Attr}, From}) ->
        {attribute, list_to_atom(Attr), simplify(From)};
    simplify({index_value, Array, Index}) ->
        {index_value, simplify(Array), simplify(Index)};
    simplify({value_list, List}) ->
        {value_list, [ simplify(Elt) || Elt <- List ]};
    simplify({apply_filter, Expr, {filter, {identifier,_,Filter}, Args}}) ->
        {apply_filter, list_to_atom("filter_"++Filter), list_to_atom(Filter), simplify(Expr), [ simplify(Arg) || Arg <- Args ]}.


%% @doc Evaluate a parsed expression tree.
eval(Tree, Vars, Context) ->
    eval1(Tree, Vars, Context).

eval1({expr, Op, Left, Right}, Vars, Context) ->
    erlydtl_operators:Op(eval1(Left, Vars, Context), eval1(Right, Vars, Context), Context);
eval1({expr, Op, Expr}, Vars, Context) ->
    erlydtl_operators:Op(eval1(Expr, Vars, Context), Context);
eval1({variable, Name}, Vars, Context) ->
    erlydtl_runtime:find_value(Name, Vars, Context);
eval1({index_value, Array, Index}, Vars, Context) ->
    erlydtl_runtime:find_value(eval1(Index, Vars, Context), eval1(Array, Vars, Context), Context);
eval1({attribute, Attr, From}, Vars, Context) ->
    erlydtl_runtime:find_value(Attr, eval1(From, Vars, Context), Vars, Context);
eval1({value_list, List}, Vars, Context) ->
    [ eval1(Elt, Vars, Context) || Elt <- List ];
eval1({apply_filter, Mod, Func, Expr, Args}, Vars, Context) ->
    EvalArgs = [ eval1(Arg, Vars, Context) || Arg <- Args],
    EvalExpr = eval1(Expr, Vars, Context),
    erlang:apply(Mod, Func, [EvalExpr | EvalArgs] ++[Context]);
eval1(m, _Vars, _Context) ->
    #m{};
eval1(Val, _Vars, _Context) ->
    Val.
