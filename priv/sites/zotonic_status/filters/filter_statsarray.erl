%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-12-27

%% @doc jQuery compatible key for statistics

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

-module(filter_statsarray).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("include/zotonic.hrl").

-export([statsarray/4]).

statsarray(Stats, Type, Which, _Context) ->
    [
     "[",
     [ ["{label: \"", atom_to_list(N), "\", data: ", resultarray(Type, proplists:get_value(list_to_existing_atom(Which), 
                                             proplists:get_value(Type, S), [])),
        "},"
       ]
       || {N, S} <- Stats],
     "]"].


resultarray(Type, Values) ->
    [ "[", 
     lists:flatten(
      [ ["[", integer_to_list(K), ",", io_lib:format("~.2f", [map(Type, V)]), "],"]
       || {K, {V,_N}} <- lists:zip(lists:seq(1, length(Values)), Values)]), 
      "],"
    ].
        
map({_, _, duration}, V) -> V/10.0; %% show duration in ms.
map(_, V) -> V/1.0.
