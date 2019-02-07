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

-module(z_auth2fa_qrcode_mask).

-include("z_auth2fa_qrcode_params.hrl").

-export([generate/2, select/1]).

-define(PENALTY_RULE_1, 3).
-define(PENALTY_RULE_2, 3).
-define(PENALTY_RULE_3, 40).
-define(PENALTY_RULE_4, 10).

%% Generates all eight masked versions of the bit matrix
generate(#qr_params{dimension = Dim}, Matrix) ->
	Sequence = lists:seq(0, 7),
	Functions = [mask(X) || X <- Sequence],
	Masks = [generate_mask(Dim, MF) || MF <- Functions],
	[apply_mask(Matrix, Mask, []) || Mask <- Masks].
	
%% Selects the lowest penalty candidate from a list of bit matrices
select([H|T]) ->
	Score = score_candidate(H),
	select_candidate(T, 0, 0, Score, H).

%% Internal

%
generate_mask(Max, MF) ->
	Sequence = lists:seq(0, Max - 1),
	[generate_mask(Sequence, Y, MF) || Y <- Sequence].
generate_mask(Sequence, Y, MF) ->
	[case MF(X, Y) of true -> 1; false -> 0 end || X <- Sequence].

apply_mask([H|T], [H0|T0], Acc) ->
	Row = apply_mask0(H, H0, []),
	apply_mask(T, T0, [Row|Acc]);
apply_mask([], [], Acc) ->
	lists:reverse(Acc).
	
apply_mask0([H|T], [H0|T0], Acc) when is_integer(H) ->
	apply_mask0(T, T0, [H bxor H0|Acc]);
apply_mask0([H|T], [_|T0], Acc) ->
	apply_mask0(T, T0, [H|Acc]);
apply_mask0([], [], Acc) ->
	lists:reverse(Acc).

% (i + j) mod 2 = 0
mask(0) -> 
	fun(X, Y) -> (X + Y) rem 2 =:= 0 end;
% i mod 2 = 0
mask(1) -> 
	fun(_X, Y) -> Y rem 2 =:= 0 end;
% j mod 3 = 0
mask(2) -> 
	fun(X, _Y) -> X rem 3 =:= 0 end;
% (i + j) mod 3 = 0
mask(3) -> 
	fun(X, Y) -> (X + Y) rem 3 =:= 0 end;
% ((i div 2) + (j div 3)) mod 2 = 0
mask(4) -> 
	fun(X, Y) -> (X div 3 + Y div 2) rem 2 =:= 0 end;
%101 (i * j) mod 2 + (i *j) mod 3 = 0
mask(5) -> 
	fun(X, Y) -> Sum = X * Y, Sum rem 2 + Sum rem 3 =:= 0 end;
% ((i * j) mod 2 + (i* j) mod 3) mod 2 = 0
mask(6) -> 
	fun(X, Y) -> Sum = X * Y, (Sum rem 2 + Sum rem 3) rem 2 =:= 0 end;
%((i * j) mod 3 + (i + j) mod 2) mod 2 = 0
mask(7) -> 
	fun(X, Y) -> ((X * Y rem 3) + ((X + Y) rem 2)) rem 2 =:= 0 end.
	
select_candidate([H|T], Count, Mask, Score, C) ->
	case score_candidate(H) of
	X when X < Score ->
		select_candidate(T, Count + 1, Count + 1, X, H);
	_ ->
		select_candidate(T, Count + 1, Mask, Score, C)
	end;
select_candidate([], _, Mask, _Score, C) ->
	%?TTY({selected, Mask, {score, Score}}),
	{Mask, C}.

score_candidate(C) ->
	Rule1 = apply_penalty_rule_1(C),
	Rule2 = apply_penalty_rule_2(C),
	Rule3 = apply_penalty_rule_3(C),
	Rule4 = apply_penalty_rule_4(C),
	Total = Rule1 + Rule2 + Rule3 + Rule4,
	%?TTY({score, Total, [Rule1, Rule2, Rule3, Rule4]}),
	Total.
	
%% Section 8.2.2
apply_penalty_rule_1(Candidate) ->
	ScoreRows = rule1(Candidate, 0),
	ScoreCols = rule1(rows_to_columns(Candidate), 0),
	ScoreRows + ScoreCols.
%
rule1([Row|T], Score) ->
	Score0 = rule1_row(Row, Score),
	rule1(T, Score0);
rule1([], Score) ->
	Score.
%
rule1_row(L = [H|_], Score) ->
	F = fun
		(1) when H =:= 1 ->
			true;
		(1) ->
			false;
		(_) when H =:= 0 orelse is_integer(H) =:= false ->
			true;
		(_) ->
			false
		end,
	{H0,T0} = lists:splitwith(F, L),
	case length(H0) of 
	Repeats when Repeats >= 5 ->
		Penalty = ?PENALTY_RULE_1 + Repeats - 5,
		rule1_row(T0, Score + Penalty);
	_ ->
		rule1_row(T0, Score)
	end;
rule1_row([], Score) ->
	Score.

%%
apply_penalty_rule_2(_M = [H, H0|T]) ->
%	?TTY(M),
	Blocks = rule2(1, 1, H, H0, [H0|T], []),
	Blocks0 = composite_blocks(Blocks, []),
	Blocks1 = composite_blocks(Blocks0, []),
%	?TTY(Blocks1),
	score_blocks(Blocks1, 0).

score_blocks([{_, {M, N}, _}|T], Acc) ->
	Score = ?PENALTY_RULE_2 * (M - 1) * (N - 1),
	score_blocks(T, Acc + Score);
score_blocks([], Acc) ->
	Acc.
	
rule2(X, Y, [H, H|T], [H, H|T0], Rows, Acc) ->
	rule2(X + 1, Y, [H|T], [H|T0], Rows, [{{X, Y}, {2, 2}, H}|Acc]);
rule2(X, Y, [_|T], [_|T0], Rows, Acc) ->
	rule2(X + 1, Y, T, T0, Rows, Acc);
rule2(_, Y, [], [], [H, H0|T], Acc) ->
	rule2(1, Y + 1, H, H0, [H0|T], Acc);
rule2(_, _, [], [], [_], Acc) ->
	lists:reverse(Acc).

composite_blocks([H|T], Acc) ->
	{H0, T0} = composite_block(H, T, []),
	composite_blocks(T0, [H0|Acc]);
composite_blocks([], Acc) ->
	lists:reverse(Acc).

composite_block(B, [H|T], Acc) ->
	case combine_block(B, H) of
	false ->
		composite_block(B, T, [H|Acc]);
	B0 ->
		composite_block(B0, T, Acc)
	end;
composite_block(B, [], Acc) ->
	{B, lists:reverse(Acc)}.

% Does Block 0 contain the Block 1 coordinate?
combine_block(B = {{X, Y}, {SX, SY}, _}, B0 = {{X0, Y0}, _, _}) 
		when X0 < X + SX orelse Y0 < Y + SY  ->
	combine_block0(B, B0);
combine_block(_, _) ->
	false.
	
% are they same valued?
combine_block0(B = {_, _, V}, B0 = {_, _, V0}) 
	when V =:= V0 orelse (V =/= 1 andalso V0 =/= 1) ->
	combine_block1(B, B0);
combine_block0(_, _) ->
	false.
	
% is B extended by B0 horizontally?
combine_block1({{X, Y}, {SX, SY}, V}, {{X0, Y}, {SX0, SY}, _}) when X0 =:= X + SX - 1 ->
	{{X, Y}, {SX + SX0 - 1, SY}, V};
% is B extended by B0 vertically?
combine_block1({{X, Y}, {SX, SY}, V}, {{X, Y0}, {SX, SY0}, _}) when Y0 =:= Y + SY - 1 ->
	{{X, Y}, {SX, SY + SY0 - 1}, V};
combine_block1(_, _) ->
	false.

%%
apply_penalty_rule_3(Candidate) -> 
	RowScores = [rule3(Row, 0) || Row <- Candidate],
	ColumnScores = [rule3(Col, 0) || Col <- rows_to_columns(Candidate)],
	lists:sum(RowScores) + lists:sum(ColumnScores).
%
rule3(Row = [1|T], Score) ->
	Ones = lists:takewhile(fun(X) -> X =:= 1 end, Row),
	Scale = length(Ones),
	case Scale * 7 of
	Length when Length > length(Row) ->
		rule3(T, Score);
	Length ->
		case is_11311_pattern(lists:sublist(Row, Length), Scale) of
		true ->
			rule3(T, Score + ?PENALTY_RULE_3);
		false ->
			rule3(T, Score)
		end
	end;
rule3([_|T], Score) ->
	rule3(T, Score);
rule3([], Acc) ->
	Acc.
%
is_11311_pattern(List, Scale) ->
	List0 = lists:map(fun(X) when X =:= 1 -> 1; (_) -> 0 end, List), 
	Result = condense(List0, Scale, []),
	Result =:= [1,0,1,1,1,0,1].
%
condense([], _, Acc) ->
	lists:reverse(Acc);
condense(L, Scale, Acc) ->
	{H, T} = lists:split(Scale, L),
	case lists:sum(H) of
	Scale ->
		condense(T, Scale, [1|Acc]);
	0 ->
		condense(T, Scale, [0|Acc]);
	_ ->
		undefined
	end.
	
%%
apply_penalty_rule_4(Candidate) ->
	Proportion = rule4(Candidate, 0, 0),
	%?TTY({proportion, Proportion}),
	?PENALTY_RULE_4 * (trunc(abs(Proportion * 100 - 50)) div 5).
%	
rule4([H|T], Dark, All) ->
	All0 = All + length(H),
	Dark0 = Dark + length([X || X <- H, X =:= 1]),
	rule4(T, Dark0, All0);
rule4([], Dark, All) ->
	Dark / All.
	
%
rows_to_columns(L) ->
	rows_to_columns(L, []).
rows_to_columns([[]|_], Acc) ->
	lists:reverse(Acc);
rows_to_columns(L, Acc) ->
	Heads = [H || [H|_] <- L],
	Tails = [T || [_|T] <- L],
	rows_to_columns(Tails, [Heads|Acc]).

