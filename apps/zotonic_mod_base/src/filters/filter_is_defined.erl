%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'is_defined' filter, test if a value is defined

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

-module(filter_is_defined).
-moduledoc("
See also

[is\\_undefined](/id/doc_template_filter_filter_is_undefined), [if\\_undefined](/id/doc_template_filter_filter_if_undefined), [if](/id/doc_template_filter_filter_if)

Tests if a value is defined.

Checks if the value is not empty and outputs a boolean true or false. This is useful in combination with the
[if](/id/doc_template_tag_tag_if) tag.

For example:


```django
{% if value|is_defined %}The value was defined{% endif %}
```

When the value is “foo” then the output “The value was defined”.
").
-export([is_defined/2]).


is_defined(undefined, _Context) ->
    false;
is_defined({{9999,_,_},{_,_,_}}, _Context) ->
	false;
is_defined(_V, _Context) ->
    true.
