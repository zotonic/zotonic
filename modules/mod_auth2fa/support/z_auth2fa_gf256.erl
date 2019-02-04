%% Copyright 2011 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

%% NOTE: This module implements finite field arithmetic over the galois field
% GF(256) with a specified prime modulus.

-module(z_auth2fa_gf256).

-export([field/1, add/3, subtract/3, multiply/3]).
-export([exponent/2, log/2, inverse/2, value/3]).
-export([monomial_product/4, polynomial_product/3, divide/3]).

-record(gf256, {exponent, log}).

% UNUSED
%-record(gf256poly, {field, coefficients}).
% NOTE: Implementation and use are greatly simplified by expressing polynomials
% simply as lists of coefficient values,  rather than explicit reification of
% polynomial "objects".

-define(RANGE, 255).

%%
field(PrimeModulus) ->
	Exponent = exponent_table(1, PrimeModulus, []),
	Log = log_table(Exponent, 1, [0]),
	#gf256{exponent = Exponent, log = Log}.
%
exponent_table(X, Modulus, Acc) when length(Acc) =< ?RANGE ->
	case X bsl 1 of
	V when V > ?RANGE ->
		X0 = V bxor Modulus;
	V ->
		X0 = V
	end,
	exponent_table(X0, Modulus, [X|Acc]);
exponent_table(_, _, Acc) ->
	lists:reverse(Acc).
%
log_table(E, Count, Acc) when Count =< ?RANGE ->
	X = index_of(Count, 0, E),
	log_table(E, Count + 1, [X|Acc]);
log_table(_, _, Acc) ->
	lists:reverse(Acc).
%
index_of(X, Count, [X|_]) ->
	Count;
index_of(X, Count, [_|T]) ->
	index_of(X, Count + 1, T).

%%
add(#gf256{}, A, B) when is_integer(A), is_integer(B) ->
	A bxor B;
add(#gf256{}, [0], B) when is_list(B) ->
	B;
add(#gf256{}, A, [0]) when is_list(A) ->
	A;
add(F = #gf256{}, A, B) when is_list(A), is_list(B) ->
	add(F, lists:reverse(A), lists:reverse(B), []).

add(F, [H|T], [H0|T0], Acc) ->
	add(F, T, T0, [H bxor H0 | Acc]);
add(F, [H|T], [], Acc) ->
	add(F, T, [], [H|Acc]);
add(F, [], [H|T], Acc) ->
	add(F, [], T, [H|Acc]);
add(_, [], [], Acc) ->
	Acc.

%% NOTE: Subtraction is the same as addition over a galois field.
subtract(F = #gf256{}, A, B) ->
	add(F, A, B).

%%
multiply(#gf256{}, 0, _) ->
	0;
multiply(#gf256{}, _, 0) ->
	0;
multiply(F = #gf256{}, A, B) ->
	X = (log(F, A) + log(F, B)) rem ?RANGE,
	exponent(F, X).

%%
exponent(#gf256{exponent = E}, X) ->
	lists:nth(X + 1, E).

%%
log(#gf256{log = L}, X) ->
	lists:nth(X + 1, L).

%%
inverse(F = #gf256{}, X) ->
	exponent(F, ?RANGE - log(F, X)).

%%
value(#gf256{}, Poly, 0) ->
	lists:last(Poly);
value(F = #gf256{}, Poly, 1) ->
	lists:foldl(fun(X, Sum) -> z_auth2fa_gf256:add(F, X, Sum) end, 0, Poly);
value(F = #gf256{}, [H|T], X) ->
	value(F, T, X, H).
%
value(F, [H|T], X, Acc) ->
	Acc0 = multiply(F, X, Acc),
	Acc1 = add(F, Acc0, H),
	value(F, T, X, Acc1);
value(_, [], _, Acc) ->
	Acc.

%
monomial(#gf256{}, 0, Degree) when Degree >= 0 ->
	[0];
monomial(#gf256{}, Coeff, Degree) when Degree >= 0 ->
	[Coeff|lists:duplicate(Degree, 0)].

%%
monomial_product(F, Poly, Coeff, Degree) ->
	monomial_product(F, Poly, Coeff, Degree, []).
%
monomial_product(F, [H|T], C, D, Acc) ->
	P = z_auth2fa_gf256:multiply(F, H, C),
	monomial_product(F, T, C, D, [P|Acc]);
monomial_product(F, [], C, D, Acc) when D > 0 ->
	monomial_product(F, [], C, D - 1, [0|Acc]);
monomial_product(_, [], _, 0, Acc) ->
	lists:reverse(Acc).

%%
polynomial_product(_, [0], _) ->
	[0];
polynomial_product(_, _, [0]) ->
	[0];
polynomial_product(F, P0, P1) ->
	polynomial_product0(F, P0, P1, [], []).
%
polynomial_product0(F, [H|T], P1, P2, Acc) ->
	[H0|T0] = polynomial_product1(F, H, P1, P2, []),
	polynomial_product0(F, T, P1, T0, [H0|Acc]);
polynomial_product0(F, [], P1, [H|T], Acc) ->
	polynomial_product0(F, [], P1, T, [H|Acc]);
polynomial_product0(_, [], _, [], Acc) ->
	lists:reverse(Acc).
%
polynomial_product1(_, _, [], [], Acc) ->
	lists:reverse(Acc);
polynomial_product1(F, X, [H|T], [], Acc) ->
	Coeff = polynomial_product2(F, X, H, 0),
	polynomial_product1(F, X, T, [], [Coeff|Acc]);
polynomial_product1(F, X, [H|T], [H0|T0], Acc) ->
	Coeff = polynomial_product2(F, X, H, H0),
	polynomial_product1(F, X, T, T0, [Coeff|Acc]).

polynomial_product2(F, X, H, H0) ->
	Coeff = multiply(F, X, H),
	add(F, H0, Coeff).

%%
divide(F = #gf256{}, A, B = [H|_]) when B =/= [0] ->
	IDLT = inverse(F, H),
	divide(F, IDLT, B, [0], A).
%
divide(F, IDLT, B, Q, R = [H|_]) when length(R) >= length(B), R =/= [0] ->
	Diff = length(R) - length(B),
	Scale = multiply(F, H, IDLT),
	M = monomial(F, Scale, Diff),
	Q0 = add(F, Q, M),
	Coeffs = monomial_product(F, B, Scale, Diff),
	[_|R0] = add(F, R, Coeffs),
	divide(F, IDLT, B, Q0, R0);
divide(_, _, _, Q, R) ->
	{Q, R}.
