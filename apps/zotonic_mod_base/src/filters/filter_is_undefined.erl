%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'is_defined' filter, test if a value is not defined

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

-module(filter_is_undefined).
-moduledoc("
Tests if a value is undefined.

Checks if the value is empty and outputs a boolean true or false. This is useful in combination with the
[if](/id/doc_template_tag_tag_if) tag.

For example:


```django
{% if value[1]|is_undefined %}The first elemeent of value was undefined{% endif %}
```

If the value is `[]` then the output is `The first elemeent of value was undefined`.

See also

[is_defined](/id/doc_template_filter_filter_is_defined), [if_undefined](/id/doc_template_filter_filter_if_undefined), [if](/id/doc_template_filter_filter_if)").
-export([is_undefined/2]).

is_undefined(V, Context) ->
    not(filter_is_defined:is_defined(V, Context)).

