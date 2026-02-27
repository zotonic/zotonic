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
-moduledoc("
Filters a list on the value of a property, either on presence or equality.



Testing presence
----------------

To filter a list of values:


```django
{% print somelist|filter:`p` %}
```

Results in a list where all elements have the property `p` defined and where the property (after conversion to boolean)
is `true`.

This can be used to filter a list of resource ids on the presence of a property. For example, to see all published
elements in a list of resource ids:


```django
{% print [1,2,3,4,5,6]|filter:`is_published` %}
```

To find all pages from page connection `hasdocument` that have an image:


```django
{% print id.o.hasdocument|filter:`depiction` %}
```



Testing equality
----------------

A second argument can be added to test on equality:


```django
{% print somelist|filter:`title`:\"Untitled\" %}
```

Shows all elements whose `title` property is “Untitled”.

See also

[is_visible](/id/doc_template_filter_filter_is_visible), [is_a](/id/doc_template_filter_filter_is_a), [exclude](/id/doc_template_filter_filter_exclude)").
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

