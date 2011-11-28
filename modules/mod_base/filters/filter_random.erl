%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'random' filter, return a random element/elements of a list

%% Copyright 2010-2011 Marc Worrell, Konstantin Nikiforov
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

-module(filter_random).
-export([random/2, random/3]).


%% @doc Get random element from list.
random([], _Context) ->
    undefined;
random(undefined, _Context) ->
    undefined;
random(In, Context) when is_list(In) ->
    M = filter_rand:rand(length(In), Context),
    hd(lists:nthtail(M-1, In));
random(In, Context) ->
    random(erlydtl_runtime:to_list(In, Context), Context).


%% @doc create a sublist of using n random elements from source list
random(N, _Count, _Context) when N == [] orelse N == undefined ->
    [];
random(In, Count, _Context) when is_list(In) ->
    L = length(In),
    RandomizedIn = z_utils:randomize(In),
    if
	Count >= L -> RandomizedIn;
	true	   -> lists:nthtail(L - Count, RandomizedIn)
    end;
random(In, Count, Context) ->
    random(erlydtl_runtime:to_list(In, Context), Count, Context).
