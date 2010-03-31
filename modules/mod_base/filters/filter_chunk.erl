%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'chunk' filter, split a list in sublists of a maximum length

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

-module(filter_chunk).
-export([chunk/3]).


chunk(undefined, _N, _Context) ->
    undefined;
chunk(In, N, Context) ->
    chunk1(erlydtl_runtime:to_list(In, Context), N, []).
    
    chunk1([], _, Acc) ->
        lists:reverse(Acc);
    chunk1(List, N, Acc) ->
        {Chunk, List1} = chunk_take(List, N, []),
        chunk1(List1, N, [Chunk|Acc]).

    chunk_take([], _, Acc) ->
        {lists:reverse(Acc), []};
    chunk_take(L, 0, Acc) ->
        {lists:reverse(Acc), L};
    chunk_take([H|T], N, Acc) ->
        chunk_take(T, N-1, [H|Acc]).
