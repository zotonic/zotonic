%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%% @copyright 2011 Dmitrii Dimandt
%% @doc 'slice' filter, get a range of elements from a list

%% Copyright 2011 Dmitrii Dimandt
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

%
% Given a list = [1,2,3,4,5,6,7,8,9,0]
%
% Get all elements from element M to element N:
% {{ list|slice:[3,7] }} -> [3,4,5,6,7]
% {{ list|slice:[3,-3] }} -> [3,4,5,6,7]
% {{ list|slice:[-7,-3] }} -> [4,5,6,7]
% {{ list|slice:[-7,7] }} -> [4,5,6,7]
%
% Get all elements except the first N:
% {{ list|slice:[3,] }} -> [3,4,5,6,7,8,9,0]
% {{ list|slice:[-7,] }} -> [4,5,6,7,8,9,0]
%
% Get all elements up to element N
% {{ list|slice:[,3] }} -> [1,2,3]
% {{ list|slice:[3] }} -> [1,2,3]
%
% Get all elements except the last N:
% {{ list|slice:[,-3] }} -> [1,2,3,4,5,6,7]
% {{ list|slice:[-3] }} -> [1,2,3,4,5,6,7]
%
% {{ list|slice:[M,N] }}, where N < M will return []
% {{ list|slice:[,] }}, will return [1,2,3,4,5,6,7,8,9,0]
%

-module(filter_slice).
-export([slice/3]).

slice(undefined, _, _Context) ->
    undefined;
slice(List, Slice, _Context) when is_list(List) ->
    slice1(List, Slice);
slice({trans, _} = Tr, Slice, Context) ->
    slice(z_trans:lookup_fallback(Tr, Context), Slice, Context);
slice(Binary, Slice, _Context) when is_binary(Binary) ->
    slice1(Binary, Slice);
slice(MaybeList, Slice, Context) ->
    slice1(z_template_compiler_runtime:to_list(MaybeList, Context), Slice).

slice1([], _) -> [];
slice1(<<>>, _) -> <<>>;
slice1(List, [undefined, undefined]) -> slice2(List, 1, strlen(List));
slice1(List, [M, undefined]) -> slice2(List, M, strlen(List));
slice1(List, [undefined, N]) -> slice2(List, 1, N);
slice1(List, [M, N]) -> slice2(List, z_convert:to_integer(M), z_convert:to_integer(N));
slice1(List, [M]) -> slice1(List, [undefined, M]);
slice1(List, M) -> slice1(List, [undefined, z_convert:to_integer(M)]).

slice2(List, 0, _N) when is_list(List) -> [];
slice2(List, _M, 0) when is_list(List) -> [];
slice2(List, 0, _N) when is_binary(List) -> <<>>;
slice2(List, _M, 0) when is_binary(List) -> <<>>;
slice2(List, M, N) when M < 0 -> slice2(List, strlen(List) + M + 1, N);
slice2(List, M, N) when N < 0 -> slice2(List, M, strlen(List)+N);
slice2(List, M, N) when N < M, is_list(List) -> [];
slice2(List, M, N) when N < M, is_binary(List) -> <<>>;
slice2(List, M, N) when is_list(List) ->
    lists:sublist(List, M, N - M + 1);
slice2(B, M, N) when is_binary(B) ->
    substring(B, M, N - M + 1).

substring(<<>>, _, _) -> <<>>;
substring(B, 1, N) -> substring_1(B, N, <<>>);
substring(<<_/utf8, B/binary>>, M, N) -> substring(B, M - 1, N);
substring(<<_, B/binary>>, M, N) -> substring(B, M - 1, N).

substring_1(_, 0, Acc) -> Acc;
substring_1(<<>>, _, Acc) -> Acc;
substring_1(<<C/utf8, B/binary>>, N, Acc) -> substring_1(B, N - 1, <<Acc/binary, C/utf8>>);
substring_1(<<C, B/binary>>, N, Acc) -> substring_1(B, N - 1, <<Acc/binary, C/utf8>>).

strlen(L) when is_list(L) -> length(L);
strlen(S) when is_binary(S) -> z_string:len(S).
