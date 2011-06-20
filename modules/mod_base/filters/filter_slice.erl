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
% {{ list|slice:[-7,-3] }} -> [3,4,5,6,7]
% {{ list|slice:[-7,7] }} -> [3,4,5,6,7]
%
% Get all elements except the first N:
% {{ list|slice:[3,] }} -> [3,4,5,6,7,8,9,0]
% {{ list|slice:[-7,] }} -> [3,4,5,6,7,8,9,0]
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
% {{ list|slice:[,] }}, where N < M will return [1,2,3,4,5,6,7,8,9,0]
%

-module(filter_slice).
-export([slice/3]).


slice(undefined, _, _Context) ->
    undefined;

slice(List, Slice, _Context) when is_list(List) ->
    slice1(List, Slice);
slice(MaybeList, Slice, Context) ->
    slice1(erlydtl_runtime:to_list(MaybeList, Context), Slice).

slice1(List, [undefined, undefined]) ->
    slice2(List, 1, length(List));

slice1(List, [M, undefined]) ->
    slice2(List, M, length(List));

slice1(List, [undefined, N]) ->
    N1 = if
            N < 0 -> length(List) + N;
            true -> N
    end,
    slice2(List, 1, N1);

slice1(List, [M, N]) ->
    slice2(List, M, N);

slice1(List, [M]) ->
    slice1(List, [undefined, M]);

slice1(List, M) ->
    slice1(List, [undefined, z_convert:to_integer(M)]).


slice2(List, M, N) ->
    M1 = if
            M =:= 0 -> throw({error, invalid_index});
            M < 0 -> length(List) + M;
            true -> M
    end,
    N1 = if
            N =:= 0 -> throw({error, invalid_index});
            N < 0 -> length(List) + N;
            true -> N
    end,
    if
        N1 < M1 -> [];
        true  -> lists:sublist(List, M1, N1 - M1 + 1)
    end.
