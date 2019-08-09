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

-module(z_auth2fa_qrcode_matrix).

-include("z_auth2fa_qrcode_params.hrl").

-export([dimension/1, template/1, embed_data/2, overlay_static/2, finalize/5]).

-define(FINDER_BITS, <<6240274796270654599595212063015969838585429452563217548030:192>>).

%%
dimension(Version) 
		when Version > 0 
		andalso Version < 41 ->
	17 + (Version * 4).

%%
template(#qr_params{version = Version, align_coords = AC}) ->
	template(Version, AC).

%%
embed_data(#qr_params{version = Version, align_coords = AC, remainder = Rem}, Codewords) ->
	FlippedTemplate = flip(template(Version, AC)),
	FlippedMatrix = embed_data(FlippedTemplate, <<Codewords/binary, 0:Rem>>, []),
	flip(FlippedMatrix).
	
%%
overlay_static(#qr_params{version = Version, align_coords = AC}, Matrix) ->
	F = finder_bits(),
	T = timing_bits(Version, AC),
	A = alignment_bits(AC),
	overlay_static(Matrix, F, T, A, []).

%%
finalize(Dim, FMT, VSN, QZ, Matrix) ->
	M = format_bits(FMT),
	V = version_bits(VSN),
	FinalMatrix = overlay_format(Matrix, M, V, []),
	QBitLength = (Dim + QZ * 2) * QZ,
	Q = <<0:QBitLength>>,
	Bin = encode_bits(FinalMatrix, QZ, Q),
	<<Bin/bits, Q/bits>>.

%% Internal

%% 
template(Version, AC) ->
	Dim = dimension(Version),
	template(1, Dim, AC, []).
%
template(Y, Max, AC, Acc) when Y =< Max->
	Row = template_row(1, Y, Max, AC, []),
	template(Y + 1, Max, AC, [Row|Acc]);
template(_, _, _, Acc) ->
	lists:reverse(Acc).
%
template_row(X, Y, Max, AC, Acc) when X =< Max ->
	Ref = template_ref(X, Y, Max, AC),
	template_row(X + 1, Y, Max, AC, [Ref|Acc]);
template_row(_, _, _, _, Acc) ->
	lists:reverse(Acc).
%	
template_ref(X, Y, Max, _AC) 
		when (X =< 8 andalso Y =< 8)
		orelse (X =< 8 andalso Y > Max - 8)
		orelse (X > Max - 8 andalso Y =< 8) ->
	f;
template_ref(X, Y, Max, _AC) 
		when (X =:= 9 andalso Y =/= 7 andalso (Y =< 9 orelse Max - Y =< 7))
		orelse (Y =:= 9 andalso X =/= 7 andalso (X =< 9 orelse Max - X =< 7)) ->
	m;
template_ref(X, Y, Max, _AC) 
		when Max >= 45 
		andalso ((X < 7 andalso Max - Y =< 10) 
		orelse (Max - X =< 10 andalso Y < 7)) ->
	v;
template_ref(X, Y, Max, AC) ->
	case is_alignment_bit(X, Y, AC) of
	true -> 
		a;
	false ->
		template_ref0(X, Y, Max)
	end.
%
template_ref0(X, Y, _)
		when X =:= 7 
		orelse Y =:= 7 ->
	t;
template_ref0(_, _, _) ->
	d.

%%
is_alignment_bit(X, Y, [{Xa, Ya}|_]) 
		when (X >= Xa - 2 
		andalso X =< Xa + 2 
		andalso Y >= Ya - 2 
		andalso Y =< Ya + 2) ->
	true;
is_alignment_bit(X, Y, [_|T]) ->
	is_alignment_bit(X, Y, T);
is_alignment_bit(_X, _Y, []) ->
	false.
	
% deal with row 7 exceptional case
embed_data([HA, HB, H, HC, HD|T], Codewords, Acc) when length(T) =:= 4 -> % skip row 7
	{HA0, HB0, Codewords0} = embed_data(HA, HB, Codewords, [], []),	
	{HC0, HD0, Codewords1} = embed_data_reversed(HC, HD, Codewords0),	
	embed_data(T, Codewords1, [HD0, HC0, H, HB0, HA0|Acc]);
% normal case
embed_data([HA, HB, HC, HD|T], Codewords, Acc) ->
	{HA0, HB0, Codewords0} = embed_data(HA, HB, Codewords, [], []),	
	{HC0, HD0, Codewords1} = embed_data_reversed(HC, HD, Codewords0),	
	embed_data(T, Codewords1, [HD0, HC0, HB0, HA0|Acc]);
embed_data([], <<>>, Acc) ->
	lists:reverse(Acc).
	
embed_data([d|T0], [d|T1], <<A:1, B:1, Codewords/bits>>, StreamA, StreamB) ->
	embed_data(T0, T1, Codewords, [A|StreamA], [B|StreamB]);
embed_data([d|T0], [B|T1], <<A:1, Codewords/bits>>, StreamA, StreamB) ->
	embed_data(T0, T1, Codewords, [A|StreamA], [B|StreamB]);
embed_data([A|T0], [d|T1], <<B:1, Codewords/bits>>, StreamA, StreamB) ->
	embed_data(T0, T1, Codewords, [A|StreamA], [B|StreamB]);
embed_data([A|T0], [B|T1], Codewords, StreamA, StreamB) ->
	embed_data(T0, T1, Codewords, [A|StreamA], [B|StreamB]);
embed_data([], [], Codewords, StreamA, StreamB) ->
	{lists:reverse(StreamA), lists:reverse(StreamB), Codewords}.
	
embed_data_reversed(A, B, Codewords) ->
	{A0, B0, Codewords0} = embed_data(lists:reverse(A), lists:reverse(B), Codewords, [], []),
	{lists:reverse(A0), lists:reverse(B0), Codewords0}.

%
overlay_static([H|L], F, T, A, Acc) ->
	{F0, T0, A0, Row} = overlay0(H, F, T, A, []),
	overlay_static(L, F0, T0, A0, [Row|Acc]);
overlay_static([], <<>>, <<>>, <<>>, Acc) ->
	lists:reverse(Acc).
%
overlay0([f|L], <<F0:1, F/bits>>, T, A, Acc) ->
	overlay0(L, F, T, A, [F0|Acc]);	
overlay0([t|L], F, <<T0:1, T/bits>>, A, Acc) ->
	overlay0(L, F, T, A, [T0|Acc]);
overlay0([a|L], F, T, <<A0:1, A/bits>>, Acc) ->
	overlay0(L, F, T, A, [A0|Acc]);
overlay0([H|L], F, T, A, Acc) ->
	overlay0(L, F, T, A, [H|Acc]);
overlay0([], F, T, A, Acc) ->
	{F, T, A, lists:reverse(Acc)}.

%
encode_bits([H|T], QZ, Acc) ->
	Acc0 = encode_bits0(H, <<Acc/bits, 0:QZ>>),
	encode_bits(T, QZ, <<Acc0/bits, 0:QZ>>);
encode_bits([], _, Acc) ->
	Acc.
	
encode_bits0([H|T], Acc) when is_integer(H) ->
	encode_bits0(T, <<Acc/bits, H:1>>);
encode_bits0([], Acc) ->
	Acc.
%
overlay_format([H|L], M, V, Acc) ->
	{M0, V0, Row} = overlay1(H, M, V, []),
	overlay_format(L, M0, V0, [Row|Acc]);
overlay_format([], <<>>, <<>>, Acc) ->
	lists:reverse(Acc).
%
overlay1([m|L], <<M0:1, M/bits>>, V, Acc) ->
	overlay1(L, M, V, [M0|Acc]);	
overlay1([v|L], M, <<V0:1, V/bits>>, Acc) ->
	overlay1(L, M, V, [V0|Acc]);
overlay1([H|L], M, V, Acc) ->
	overlay1(L, M, V, [H|Acc]);
overlay1([], M, V, Acc) ->
	{M, V, lists:reverse(Acc)}.

%
flip(L) ->
	flip(L, []).
flip([[]|T], Acc) ->
	[[] || [] <- T], % guard check
	[lists:reverse(L) || L <- Acc];
flip(L, Acc) ->
	Heads = [H || [H|_] <- L],
	Tails = [T || [_|T] <- L],
	flip(Tails, [Heads|Acc]).

%%
finder_bits() ->
	?FINDER_BITS.
%%	
alignment_bits(AC) ->
	Repeats = composite_ac(AC, []),
	alignment_bits(Repeats, <<>>).
alignment_bits([H|T], Acc) ->
	Bits0 = z_auth2fa_bits:duplicate(<<31:5>>, H),
	Bits1 = z_auth2fa_bits:duplicate(<<17:5>>, H),
	Bits2 = z_auth2fa_bits:duplicate(<<21:5>>, H),
	Bits = z_auth2fa_bits:append([Bits0, Bits1, Bits2, Bits1, Bits0]),
	alignment_bits(T, <<Acc/bits, Bits/bits>>);
alignment_bits([], Acc) ->
	Acc.
%
composite_ac([{_, Row}|T], Acc) ->
	N = 1 + length([{X, Y} || {X, Y} <- T, Y =:= Row]),
	T0 = [{X, Y} || {X, Y} <- T, Y =/= Row],
	composite_ac(T0, [N|Acc]);
composite_ac([], Acc) ->
	lists:reverse(Acc).

%%
timing_bits(Version, AC) ->
	Length = dimension(Version) - 16,
	% alignment pattern start coordinates, to trigger bit skipping
	TH = timing_bits(1, Length, [X - 8 - 2 || {X, 7} <- AC], <<>>),
	TV = timing_bits(1, Length, [Y - 8 - 2 || {7, Y} <- AC], <<>>),
	<<TH/bits, TV/bits>>.
%	
timing_bits(N, Max, A, Acc) when N =< Max ->
	case lists:member(N, A) of
	true -> % skip the alignment pattern
		timing_bits(N + 5, Max, A, Acc);
	false ->
		Bit = N band 1,
		timing_bits(N + 1, Max, A, <<Acc/bits, Bit:1>>)
	end;
timing_bits(_, _, _, Acc) ->
	Acc.

%%
format_bits(Bin) ->
	<<A:7, C:1, E:7>> = z_auth2fa_bits:reverse(Bin),
	<<B:8, D:7>> = Bin,
	<<A:7, B:8, C:1, D:7, 1:1, E:7>>.

%%
version_bits(Bin) ->
	VTop = z_auth2fa_bits:reverse(Bin),
	VLeft = version_bits(VTop, []),
	<<VTop/bits, VLeft/bits>>.
%
version_bits(<<X:3/bits, Bin/bits>>, Acc) ->
	version_bits(Bin, [X|Acc]);
version_bits(<<>>, Acc) ->
	version_bits(lists:reverse(Acc), <<>>, <<>>, <<>>).
%
version_bits([<<A:1, B:1, C:1>>|T], RowA, RowB, RowC) ->
	version_bits(T, <<RowA/bits, A:1>>, <<RowB/bits, B:1>>, <<RowC/bits, C:1>>);
version_bits([], RowA, RowB, RowC) ->
	z_auth2fa_bits:append([RowA, RowB, RowC]).

