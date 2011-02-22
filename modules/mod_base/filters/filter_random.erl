%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'random' filter, return a random element of a list

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

-module(filter_random).
-export([random/2]).


random([], _Context) ->
    undefined;
random(undefined, _Context) ->
    undefined;
random(In, Context) when is_list(In) ->
    M = filter_rand:rand(length(In), Context),
    hd(lists:nthtail(M-1, In));
random(In, Context) ->
    random(erlydtl_runtime:to_list(In, Context), Context).
