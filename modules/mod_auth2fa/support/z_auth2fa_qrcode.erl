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

-module(z_auth2fa_qrcode).

-include("z_auth2fa_qrcode.hrl").
-include("z_auth2fa_qrcode_params.hrl").

-export([encode/1, encode/2, decode/1]).

%%
decode(_Bin) ->
	{error, not_implemented}.

%%
encode(Bin) ->
	encode(Bin, 'M').
%
encode(Bin, ECC) when is_binary(Bin) ->
	Params = choose_qr_params(Bin, ECC),
	Content = encode_content(Params, Bin),
	BlocksWithECC = generate_ecc_blocks(Params, Content),
	Codewords = interleave_blocks(BlocksWithECC),
	Matrix = z_auth2fa_qrcode_matrix:embed_data(Params, Codewords),
	MaskedMatrices = z_auth2fa_qrcode_mask:generate(Params, Matrix),
	Candidates = [z_auth2fa_qrcode_matrix:overlay_static(Params, M) || M <- MaskedMatrices],
	{MaskType, SelectedMatrix} = z_auth2fa_qrcode_mask:select(Candidates),
	Params0 = Params#qr_params{mask = MaskType},
	FMT = format_info_bits(Params0),
	VSN = version_info_bits(Params0),
	#qr_params{version = Version, dimension = Dim, ec_level = _ECC} = Params0,
	QRCode = z_auth2fa_qrcode_matrix:finalize(Dim, FMT, VSN, ?QUIET_ZONE, SelectedMatrix),
	%% NOTE: Added "API" record
	#qrcode{version = Version, ecc = ECC, dimension = Dim + ?QUIET_ZONE * 2, data = QRCode}.

%%
choose_qr_params(Bin, ECLevel) ->
	Mode = choose_encoding(Bin),
	{Mode, Version, ECCBlockDefs, Remainder} = choose_version(Mode, ECLevel, byte_size(Bin)),
	AlignmentCoords = alignment_patterns(Version),
	Dim = z_auth2fa_qrcode_matrix:dimension(Version),
	#qr_params{mode = Mode, version = Version, dimension = Dim, ec_level = ECLevel,
		block_defs = ECCBlockDefs, align_coords = AlignmentCoords, remainder = Remainder, data = Bin}.

%% NOTE: byte mode only (others removed)
choose_encoding(_Bin) ->
	byte.

%%
choose_version(Type, ECC, Length) ->
	choose_version(Type, ECC, Length, ?TABLES).
%
choose_version(byte, ECC, Length, [{{ECC, Version}, {_, _, Capacity, _}, ECCBlocks, Remainder}|_])
		when Capacity >= Length ->
	{byte, Version, ECCBlocks, Remainder};
choose_version(Type, ECC, Length, [_|T]) ->
	choose_version(Type, ECC, Length, T).

%%
encode_content(#qr_params{mode = Mode, version = Version}, Bin) ->
	encode_content(Mode, Version, Bin).
%
encode_content(byte, Version, Bin) ->
	encode_bytes(Version, Bin).

%%
generate_ecc_blocks(#qr_params{block_defs = ECCBlockDefs}, Bin) ->
	Bin0 = pad_data(Bin, ECCBlockDefs),
	generate_ecc(Bin0, ECCBlockDefs, []).


pad_data(Bin, ECCBlockDefs) ->
	DataSize = byte_size(Bin),
	TotalSize = get_ecc_size(ECCBlockDefs),
	PaddingSize = TotalSize - DataSize,
	Padding = binary:copy(<<?DATA_PAD_0, ?DATA_PAD_1>>, PaddingSize bsr 1),
	case PaddingSize band 1 of
	0 ->
		<<Bin/binary, Padding/binary>>;
	1 ->
		<<Bin/binary, Padding/binary, ?DATA_PAD_0>>
	end.


get_ecc_size(ECCBlockDefs) ->
	get_ecc_size(ECCBlockDefs, 0).
get_ecc_size([{C, _, D}|T], Acc) ->
	get_ecc_size(T, C * D + Acc);
get_ecc_size([], Acc) ->
	Acc.


generate_ecc(Bin, [{C, L, D}|T], Acc) ->
	{Result, Bin0} = generate_ecc0(Bin, C, L, D, []),
	generate_ecc(Bin0, T, [Result|Acc]);
generate_ecc(<<>>, [], Acc) ->
	lists:flatten(lists:reverse(Acc)).


generate_ecc0(Bin, Count, TotalLength, BlockLength, Acc) when byte_size(Bin) >= BlockLength, Count > 0 ->
	<<Block:BlockLength/binary, Bin0/binary>> = Bin,
	EC = z_auth2fa_qrcode_reedsolomon:encode(Block, TotalLength - BlockLength),
	generate_ecc0(Bin0, Count - 1, TotalLength, BlockLength, [{Block, EC}|Acc]);
generate_ecc0(Bin, 0, _, _, Acc) ->
	{lists:reverse(Acc), Bin}.

%%
interleave_blocks(Blocks) ->
	Data = interleave_data(Blocks, <<>>),
	interleave_ecc(Blocks, Data).

interleave_data(Blocks, Bin) ->
	Data = [X || {X, _} <- Blocks],
	interleave_blocks(Data, [], Bin).

interleave_ecc(Blocks, Bin) ->
	Data = [X || {_, X} <- Blocks],
	interleave_blocks(Data, [], Bin).

interleave_blocks([], [], Bin) ->
	Bin;
interleave_blocks([], Acc, Bin) ->
	Acc0 = [X || X <- Acc, X =/= <<>>],
	interleave_blocks(lists:reverse(Acc0), [], Bin);
interleave_blocks([<<X, Data/binary>>|T], Acc, Bin) ->
	interleave_blocks(T, [Data|Acc], <<Bin/binary, X>>).

%
encode_bytes(Version, Bin) when is_binary(Bin) ->
	Size = size(Bin),
	CharacterCountBitSize = cci(?BYTE_MODE, Version),
	<<?BYTE_MODE:4, Size:CharacterCountBitSize, Bin/binary, 0:4>>.


%% Table 25. Error correction level indicators
ecc('L') -> 1;
ecc('M') -> 0;
ecc('Q') -> 3;
ecc('H') -> 2.

% Table 5. Charset encoder
% NOTE: removed

%%
alignment_patterns(Version) ->
	D = z_auth2fa_qrcode_matrix:dimension(Version),
	L = element(Version, ?ALIGNMENT_COORDINATES),
	L0 = [{X, Y} || X <- L, Y <- L],
	L1 = [{X, Y} || {X, Y} <- L0, is_finder_region(D, X, Y) =:= false],
	% Change the natural sort order so that rows have greater weight than columns
	F = fun
		({_, Y}, {_, Y0}) when Y < Y0 ->
			true;
		({X, Y}, {X0, Y0}) when Y =:= Y0 andalso X =< X0 ->
			true;
		(_, _) ->
			false
		end,
	lists:sort(F, L1).
%
is_finder_region(D, X, Y)
		when (X =< 8 andalso Y =< 8)
		orelse (X =< 8 andalso Y >= D - 8)
		orelse (X >= D - 8 andalso Y =< 8) ->
	true;
is_finder_region(_, _, _) ->
	false.

%% Table 3. Number of bits in Character Count Indicator
cci(Mode, Version) when Version >= 1 andalso Version =< 40->
	{Mode, CC} = lists:keyfind(Mode, 1, ?CCI_BITSIZE),
	cci0(CC, Version).
%
cci0([X, _, _], Version) when Version =< 9 ->
	X;
cci0([_, X, _], Version) when Version =< 26 ->
	X;
cci0([_, _, X], _) ->
	X.

version_info_bits(#qr_params{version = Version}) when Version < 7 ->
	<<>>;
version_info_bits(#qr_params{version = Version}) when Version =< 40 ->
	BCH = z_auth2fa_qrcode_reedsolomon:bch_code(Version, ?VERSION_INFO_POLY),
	<<Version:6, BCH:12>>.

format_info_bits(#qr_params{ec_level = ECLevel, mask = MaskType}) ->
	Info = (ecc(ECLevel) bsl 3) bor MaskType,
	BCH = z_auth2fa_qrcode_reedsolomon:bch_code(Info, ?FORMAT_INFO_POLY),
	InfoWithEC = (Info bsl 10) bor BCH,
	Value = InfoWithEC bxor ?FORMAT_INFO_MASK,
	<<Value:15>>.
