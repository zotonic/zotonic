%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%% @doc 'filter' filter, filters a list to only display elements with a certain property

%% Copyright 2013 Marc Worrell
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

-module(filter_filter).
-export([
	filter/2,
	filter/3,
	filter/4
	]).

filter(undefined, _Context) -> [];
filter(In, Context) ->
	lists:filter(fun(Elt) ->
					not z_utils:is_empty(Elt)
			 	 end,
			 	 z_template_compiler_runtime:to_list(In, Context)).

filter(_, undefined, _Context) -> [];
filter(undefined, _, _Context) -> [];
filter(In, Prop, Context) ->
	lists:filter(fun(Elt) ->
					z_convert:to_bool(find_value(Prop, Elt, Context))
			 	 end,
			 	 z_template_compiler_runtime:to_list(In, Context)).

filter(_, undefined, _, _Context) -> [];
filter(undefined, _, _, _Context) -> [];
filter(In, Prop, Value, Context) ->
	lists:filter(fun(Elt) ->
					template_compiler_operators:eq(find_value(Prop, Elt, Context), Value, z_template_compiler_runtime, Context)
			 	 end,
			 	 z_template_compiler_runtime:to_list(In, Context)).


find_value(Prop, Elt, Context) ->
	z_template_compiler_runtime:find_value(Prop, Elt, #{}, Context).

