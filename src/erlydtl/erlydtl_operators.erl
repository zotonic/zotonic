%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Operators for expression evaluation in templates

%% Copyright 2010 Marc Worrell
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

-module(erlydtl_operators).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    'and'/3,
    'not'/2,
    'or'/3,
    'xor'/3,

    concat/3,
    add/3,
    sub/3,
    divide/3,
    multiply/3,
    modulo/3,

    negate/2,
    
    ge/3,
    le/3,
    gt/3,
    lt/3,
    eq/3,
    ne/3
]).


'and'(A, B, Context) ->
    erlydtl_runtime:is_true(A, Context) and erlydtl_runtime:is_true(B, Context).

'not'(A, Context) ->
    erlydtl_runtime:is_false(A, Context).

'or'(A, B, Context) ->
    erlydtl_runtime:is_true(A, Context) or erlydtl_runtime:is_true(B, Context).

'xor'(A, B, Context) ->
    erlydtl_runtime:is_true(A, Context) xor erlydtl_runtime:is_true(B, Context).


concat(A, B, _Context) when is_list(A), is_list(B) ->
    A++B;
concat(A, B, Context) ->
    ABin = z_convert:to_binary(A, Context),
    BBin = z_convert:to_binary(B, Context),
    <<ABin/binary, BBin/binary>>.


add(A, B, _Context) ->
    case to_numbers(A, B) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A1, B1} -> A1 + B1
    end.

sub(A, B, _Context) ->
    case to_numbers(A, B) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A1, B1} -> A1 - B1
    end.

divide(A, B, _Context) ->
    case to_numbers(A, B) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {_, 0} -> undefined;
        {_, 0.0} -> undefined;
        {A1, B1} -> A1 / B1
    end.

multiply(A, B, _Context) ->
    case to_numbers(A, B) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A1, B1} -> A1 * B1
    end.

modulo(A, B, _Context) ->
    case to_numbers(A, B) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {_, 0} -> undefined;
        {_, 0.0} -> undefined;
        {A1, B1} -> A1 rem B1
    end.


negate(undefined, _Context) ->
    undefined;
negate(A, _Context) when is_number(A) ->
    0 - A;
negate(A, _Context) ->
    0 - z_convert:to_integer(A).


ge(Input, Value, _Context) ->
    case to_values(Input, Value) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A, B} -> A >= B
    end.

le(Input, Value, _Context) ->
    case to_values(Input, Value) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A, B} -> A =< B
    end.

gt(Input, Value, _Context) ->
    case to_values(Input, Value) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A, B} -> A > B
    end.

lt(Input, Value, _Context) ->
    case to_values(Input, Value) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A, B} -> A < B
    end.

eq(Input, Value, _Context) ->
    {A, B} = to_values(Input, Value),
    A == B.

ne(Input, Value, _Context) ->
    {A, B} = to_values(Input, Value),
    A /= B.


%% @doc Convert the two parameters to compatible values
to_values(undefined, B) ->
    {undefined, B};
to_values(A, undefined) ->
    {A, undefined};
to_values(A, B) when is_number(A), is_number(B) -> 
    {A,B};
to_values(A, B) when is_boolean(A); is_boolean(B) -> 
    {z_convert:to_bool(A), z_convert:to_bool(B)};
to_values(A, B) when is_integer(A); is_integer(B) -> 
    {z_convert:to_integer(A), z_convert:to_integer(B)};
to_values(A, B) when is_float(A); is_float(B) -> 
    {z_convert:to_float(A), z_convert:to_float(B)};
to_values(A, B) when is_binary(A), is_binary(B) -> 
    {A,B};
to_values(A, B) when is_tuple(A), is_tuple(B) -> 
    {A, B};
to_values(A, B) -> 
    {z_convert:to_list(A), z_convert:to_list(B)}.


%% @doc Convert the two parameters to compatible numerical values
to_numbers(undefined, B) ->
    {undefined, B};
to_numbers(A, undefined) ->
    {A, undefined};
to_numbers(A, B) when is_number(A), is_number(B) -> 
    {A,B};
to_numbers(A, B) when is_integer(A); is_integer(B) -> 
    {z_convert:to_integer(A), z_convert:to_integer(B)};
to_numbers(A, B) when is_float(A); is_float(B) -> 
    {z_convert:to_float(A), z_convert:to_float(B)};
to_numbers(A, B) -> 
    {z_convert:to_integer(A), z_convert:to_integer(B)}.
