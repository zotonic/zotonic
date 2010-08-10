%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'length' filter, return the number of elements

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

-module(filter_length).
-export([length/2]).


length(undefined, _Context) ->
    undefined;
length([], _Context) -> 
    "0";
length(<<>>, _Context) -> 
    "0";
length(Input, _Context) when is_list(Input) ->
    integer_to_list(erlang:length(Input));
length(Input, _Context) when is_binary(Input) ->
    integer_to_list(size(Input));
length(Input, Context) ->
    erlang:length(erlydtl_runtime:to_list(Input, Context)).
