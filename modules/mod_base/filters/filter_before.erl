%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'before' filter, return the element before another element in a list

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

-module(filter_before).
-export([before/3]).


before(undefined, _, _Context) ->
    undefined;
before(_, undefined, _Context) ->
    undefined;
before(L, V, Context) ->
    prev_of1(erlydtl_runtime:to_list(L, Context), V).
    
    prev_of1([], _V) ->
        undefined;
    prev_of1([P,V|_T], V) ->
        P;
    prev_of1([_|T], V) ->
        prev_of1(T, V).


