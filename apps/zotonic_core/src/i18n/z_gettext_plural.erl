%% @doc Parse and validate GNU gettext Plural-Forms headers.
%% @copyright 2026 Marc Worrell
%% @end
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
-module(z_gettext_plural).

-export([parse/1]).

%% @doc Parse the value of a Plural-Forms header once, when loading a catalog.
%% Only integer expressions over n are accepted. Limits bound catalog input size.
-spec parse(Header) -> Result when
    Header :: binary(),
    Result :: {ok, template_compiler_plural:rule()} | {error, invalid_plural_forms}.
parse(Header) when is_binary(Header), byte_size(Header) =< 4096 ->
    try
        {match, [Count, Expr]} = re:run(Header,
            <<"^\\s*nplurals\\s*=\\s*([0-9]+)\\s*;\\s*plural\\s*=\\s*([^;]+);?\\s*$">>,
            [{capture, all_but_first, binary}]),
        N = binary_to_integer(Count),
        true = N > 0 andalso N =< 100,
        {Ast, []} = conditional(tokens(binary_to_list(Expr))),
        {ok, {N, Ast}}
    catch
        _:_ -> {error, invalid_plural_forms}
    end;
parse(_) ->
    {error, invalid_plural_forms}.

tokens([]) -> [];
tokens([C|Rest]) when C =< 32 -> tokens(Rest);
tokens([C|_] = Input) when C >= $0, C =< $9 ->
    {Digits, Rest} = lists:splitwith(fun(D) -> D >= $0 andalso D =< $9 end, Input),
    [list_to_integer(Digits)|tokens(Rest)];
tokens([$n|Rest]) -> [n|tokens(Rest)];
tokens([$&, $&|Rest]) -> ['&&'|tokens(Rest)];
tokens([$|, $||Rest]) -> ['||'|tokens(Rest)];
tokens([$=, $=|Rest]) -> ['=='|tokens(Rest)];
tokens([$!, $=|Rest]) -> ['!='|tokens(Rest)];
tokens([$<, $=|Rest]) -> ['<='|tokens(Rest)];
tokens([$>, $=|Rest]) -> ['>='|tokens(Rest)];
tokens([C|Rest]) when C =:= $(; C =:= $); C =:= $?; C =:= $:;
        C =:= $+; C =:= $-; C =:= $*; C =:= $/; C =:= $%;
        C =:= $!; C =:= $<; C =:= $> ->
    [{char, C}|tokens(Rest)].

conditional(Tokens) ->
    case binary_expr(Tokens, 1) of
        {Condition, [{char, $?}|Rest]} ->
            {Yes, [{char, $:}|Rest1]} = conditional(Rest),
            {No, Rest2} = conditional(Rest1),
            {{conditional, Condition, Yes, No}, Rest2};
        Result -> Result
    end.

binary_expr(Tokens, Min) ->
    {Left, Rest} = unary(Tokens),
    binary_rest(Left, Rest, Min).

binary_rest(Left, [Op|Rest] = Tokens, Min) ->
    case precedence(Op) of
        P when P >= Min ->
            {Right, Rest1} = binary_expr(Rest, P + 1),
            binary_rest({Op, Left, Right}, Rest1, Min);
        _ -> {Left, Tokens}
    end;
binary_rest(Left, [], _) -> {Left, []}.

precedence('||') -> 1;
precedence('&&') -> 2;
precedence('==') -> 3;
precedence('!=') -> 3;
precedence({char, $<}) -> 4;
precedence({char, $>}) -> 4;
precedence('<=') -> 4;
precedence('>=') -> 4;
precedence({char, $+}) -> 5;
precedence({char, $-}) -> 5;
precedence({char, $*}) -> 6;
precedence({char, $/}) -> 6;
precedence({char, $%}) -> 6;
precedence(_) -> 0.

unary([{char, C}|Rest]) when C =:= $!; C =:= $+; C =:= $- ->
    {Expr, Rest1} = unary(Rest),
    {{unary, C, Expr}, Rest1};
unary([{char, $(}|Rest]) ->
    {Expr, [{char, $)}|Rest1]} = conditional(Rest),
    {Expr, Rest1};
unary([n|Rest]) -> {n, Rest};
unary([N|Rest]) when is_integer(N) -> {N, Rest}.
