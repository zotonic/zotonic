%% @author Dirk Geurs <dirk@driebit.nl>
%% @copyright 2015 Driebit BV
%% @doc Force the input to a value
%% @end

%% Copyright 2015 Driebit BV
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

-module(filter_make_value).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "template_filter", "structured_data", "transform"]
}).
-moduledoc("
Convert a template value to its simple value representation.

This resolves values such as model lookups and converts template runtime
wrappers before they are passed to code that expects a plain Erlang value.

For example:

```django
{% with value|make_value as plain_value %}
    {{ plain_value }}
{% endwith %}
```
").
-export([make_value/2]).

make_value(In, Context) ->
    z_template_compiler_runtime:to_simple_value(In, Context).
