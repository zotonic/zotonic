%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'after' filter, return the element after another element in a list

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

-module(filter_after).
-export(['after'/3]).


'after'(undefined, _, _Context) ->
    undefined;
'after'(_, undefined, _Context) ->
    undefined;
'after'(L, V, Context) ->
    next_of1(erlydtl_runtime:to_list(L, Context), V).

    next_of1([], _V) ->
        undefined;
    next_of1([V,N|_T], V) ->
        N;
    next_of1([_|T], V) ->
        next_of1(T, V).

