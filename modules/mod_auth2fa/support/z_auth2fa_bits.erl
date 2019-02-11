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

-module(z_auth2fa_bits).

-export([reverse/1, duplicate/2, append/1, bitlist/1, bitstring/1, stringbits/1]).
% -compile(export_all).

reverse(Bin) ->
	reverse(Bin, <<>>).
reverse(<<X:1, Bin/bits>>, Acc) ->
	reverse(Bin, <<X:1, Acc/bits>>);
reverse(<<>>, Acc) ->
	Acc.

%%
duplicate(Bin, N) ->
	duplicate(Bin, N, <<>>).
duplicate(Bin, N, Acc) when N > 0 ->
	duplicate(Bin, N - 1, <<Acc/bits, Bin/bits>>);
duplicate(_, 0, Acc) ->
	Acc.

%%
append(List) ->
	append(List, <<>>).
append([H|T], Acc) ->
	append(T, <<Acc/bits, H/bits>>);
append([], Acc) ->
	Acc.

%%
bitlist(Bin) ->
	bitlist(Bin, []).
bitlist(<<X:1, Bin/bits>>, Acc) ->
	bitlist(Bin, [X|Acc]);
bitlist(<<>>, Acc) ->
	lists:reverse(Acc).

%%
bitstring(Bin) ->
	bitstring(Bin, <<>>).
bitstring(<<0:1, Bin/bits>>, Acc) ->
	bitstring(Bin, <<Acc/binary, $0>>);
bitstring(<<1:1, Bin/bits>>, Acc) ->
	bitstring(Bin, <<Acc/binary, $1>>);
bitstring(<<>>, Acc) ->
	Acc.

%%
stringbits(Bin) ->
	stringbits(Bin, <<>>).
stringbits(<<$0, Bin/binary>>, Acc) ->
	stringbits(Bin, <<Acc/bits, 0:1>>);
stringbits(<<$1, Bin/binary>>, Acc) ->
	stringbits(Bin, <<Acc/bits, 1:1>>);
stringbits(<<>>, Acc) ->
	Acc.
