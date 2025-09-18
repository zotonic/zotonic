%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Default filter. Place holder for the documentation, as the
%% filter is built-in in the template compiler.
%% @end

%% Copyright 2025 Marc Worrell
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

-module(filter_default).
-moduledoc("
See also

[default_if_none](/id/doc_template_filter_filter_default_if_none), [if](/id/doc_template_filter_filter_if),
[is\\_defined](/id/doc_template_filter_filter_is_defined), [is\\_undefined](/id/doc_template_filter_filter_is_undefined),
[if\\_undefined](/id/doc_template_filter_filter_if_undefined)

Provide an alternative value in case a value has a falsy value (0, `false`, `undefined` or empty string).

For example:


```django
{{ value|default:1 }}
```

This filter is built-in in the template compiler and inlined when compiling templates.

").
-export([
    default/3
    ]).

%% @doc Default filter, this code is the same as what the template compiler
%% inlines when compiling templates.
default(V, Default, Context) ->
    case z_template_compiler_runtime:to_bool(V, Context) of
        true -> V;
        false -> Default
    end.
