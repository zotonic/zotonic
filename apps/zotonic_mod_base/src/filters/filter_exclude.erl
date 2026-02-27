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
-moduledoc("
Filters a list on the value of a property, either on absence or inequality.

This is the inverse of [filter](/id/doc_template_filter_filter_filter).



Testing presence
----------------

To filter a list of values:


```django
{% print somelist|exclude:`p` %}
```

Results in a list where all elements **do not have** the property `p` defined and where the property (after conversion
to boolean) is `false`.

This can be used to filter a list of resource ids on the absence of a property. For example, to see all unpublished
elements in a list of resource ids:


```django
{% print [1,2,3,4,5,6]|exclude:`is_published` %}
```

To find all pages from page connection `hasdocument` that **do not have** an image:


```django
{% print id.o.hasdocument|exclude:`depiction` %}
```



Testing equality
----------------

A second argument can be added to test on inequality:


```django
{% print somelist|exclude:`title`:\"Untitled\" %}
```

Shows all elements whose `title` property **is not** “Untitled”.

Below is another example of inversely filtering a list:


```django
{% with m.search[{latest cat='gallery'}] as result %}
  {% if result.total > 0 %}
    {%  with result|exclude:`name`:\"page_home_gallery\"|random as gallery_rsc_id %}
      {% include \"_gallery_widget.tpl\" id=gallery_rsc_id %}
    {% endwith %}
  {% endif %}
{% endwith %}
```

The example above filters against a search result and returns only elements whose `name` **is not** “page_home_gallery”.

See also

[is_visible](/id/doc_template_filter_filter_is_visible), [is_a](/id/doc_template_filter_filter_is_a), [filter](/id/doc_template_filter_filter_filter)").
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
                 z_template_compiler_runtime:to_list(In, Context)).

exclude(_, undefined, _, _Context) -> [];
exclude(undefined, _, _, _Context) -> [];
exclude(In, Prop, Value, Context) ->
    lists:filter(fun(Elt) ->
                         template_compiler_operators:ne(find_value(Prop, Elt, Context), Value, z_template_compiler_runtime, Context)
                 end,
                 z_template_compiler_runtime:to_list(In, Context)).


find_value(Prop, Elt, Context) ->
    z_template_compiler_runtime:find_value(Prop, Elt, #{}, Context).

