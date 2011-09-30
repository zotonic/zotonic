%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-09-29

%% @doc Returns the index the given item is in the given list.

%% Copyright 2011 Arjan Scherpenisse
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

-module(filter_index_of).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("include/zotonic.hrl").

-export([index_of/3]).


index_of(undefined, _Value, _Context) ->
    undefined;
index_of(<<>>, _Value, _Context) -> 
    undefined;
index_of(Input, Value, _Context) when is_list(Input) ->
    get_index_of(Value, Input, 1);
index_of(Input, Value, Context) ->
    index_of(erlydtl_runtime:to_list(Input, Context), Value, Context).

get_index_of(_, [], _)  -> undefined;
get_index_of(A, [B|Tail], Index) -> 
    case z_utils:are_equal(A, B) of
        true -> Index;
        false ->
            get_index_of(A, Tail, Index+1)
    end.
