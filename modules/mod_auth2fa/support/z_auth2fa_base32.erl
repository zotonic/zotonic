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

-module(z_auth2fa_base32).

-export([encode/1, decode/1]).

-define(BASE32_ALPHABET, {
	$A, $B, $C, $D, $E, $F, $G, $H, 
	$I, $J, $K, $L, $M, $N, $O, $P, 
	$Q, $R, $S, $T, $U, $V, $W, $X, 
	$Y, $Z, $2, $3, $4, $5, $6, $7
}).

%% RFC 4648

%%
encode(Bin) when is_binary(Bin) ->
    Split = 5 * (byte_size(Bin) div 5),
    <<Main0:Split/binary, Rest/binary>> = Bin,
    Main = << <<(b32e(C))>> || <<C:5>> <= Main0 >>,
	encode0(Rest, Main).

encode0(<<>>, Acc) ->
	Acc;
encode0(<<A:5, B:3>>, Acc) ->
	<<Acc/binary, (b32e(A)), (b32e(B bsl 2)), "======">>;
encode0(<<A:5, B:5, C:5, D:1>>, Acc) ->
	<<Acc/binary, (b32e(A)), (b32e(B)), (b32e(C)), (b32e(D bsl 4)), "====">>;
encode0(<<A:5, B:5, C:5, D:5, E:4>>, Acc) ->
	<<Acc/binary, (b32e(A)), (b32e(B)), (b32e(C)), (b32e(D)), (b32e(E bsl 1)), "===">>;
encode0(<<A:5, B:5, C:5, D:5, E:5, F:5, G:2>>, Acc) ->
	<<Acc/binary, (b32e(A)), (b32e(B)), (b32e(C)), (b32e(D)), (b32e(E)), (b32e(F)), (b32e(G bsl 3)), "=">>.
	
%%
decode(Bin) when is_binary(Bin) ->
	Result = decode(Bin, <<>>),
	true = is_binary(Result),
	Result.
	
decode(<<X, "======">>, Acc) ->
	Bits = decode0(X) bsr 2,
	<<Acc/bits, Bits:3>>;
decode(<<X, "====">>, Acc) ->
	Bits = decode0(X) bsr 4,
	<<Acc/bits, Bits:1>>;
decode(<<X, "===">>, Acc) ->
	Bits = decode0(X) bsr 1,
	<<Acc/bits, Bits:4>>;
decode(<<X, "=">>, Acc) ->
	Bits = decode0(X) bsr 3,
	<<Acc/bits, Bits:2>>;
decode(<<A, Bin/binary>>, Acc) ->
	Bits = decode0(A),
	decode(Bin, <<Acc/bits, Bits:5>>);
decode(<<>>, Acc) ->
	true = is_binary(Acc),
	Acc.

decode0(X) when X >= $A, X =< $Z ->
	X - $A;
decode0(X) when X >= $2, X =< $7 ->
	X - $2 + 26.

b32e(X) ->
    element(X + 1, ?BASE32_ALPHABET).

			
