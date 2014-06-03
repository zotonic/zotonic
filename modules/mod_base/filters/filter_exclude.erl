%% @author Mawuli Adzaku <mawuli@mawuli.me>
%% @copyright 2014 Mawuli Adzaku
%% @doc 'filter_exclude' filter_exclude, filters a list to only display elements without a certain property

%% Copyright 2014 Mawuli Adzaku
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

-module(filter_exclude).
-export([
	exclude/3,
	exclude/4
	]).


exclude(_, undefined, _Context) -> [];
exclude(undefined, _, _Context) -> [];
exclude(In, Prop, Context) ->
    lists:filter(fun(Elt) -> 
                         z_convert:to_bool(find_value(Prop, Elt, Context)) =:= false
                 end,
                 erlydtl_runtime:to_list(In, Context)).

exclude(_, undefined, _, _Context) -> [];
exclude(undefined, _, _, _Context) -> [];
exclude(In, Prop, Value, Context) ->
    lists:filter(fun(Elt) -> 
                         erlydtl_operators:ne(find_value(Prop, Elt, Context), Value, Context)
                 end,
                 erlydtl_runtime:to_list(In, Context)).


find_value(Prop, Elt, Context) ->
    erlydtl_runtime:find_value(Prop, Elt, Context).

