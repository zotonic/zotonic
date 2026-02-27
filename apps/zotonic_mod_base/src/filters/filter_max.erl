%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2026 Marc Worrell
%% @doc Take maximum value.

%% Copyright 2010-2026 Marc Worrell
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

-module(filter_max).
-moduledoc("
Take the maximum of the filter value and its first argument.

Usage with two values
---------------------

Take the maximum of the filter value and its first argument.

```django
   {% print 102 | to_integer | max:103 %}
```

Prints `103`.

Usage with two values
---------------------

Take the maximum of the filter value and its first argument::

   {{ 102|max:103 }}

Prints ``103``.

Usage with lists
----------------

Find the maximum value in a list::

```django

   {% print [1, 5, 3, 9, 2] | max %}
```

Prints `9`.

Edge cases
----------

- `undefined | max` returns `undefined`
- `undefined | max:1000` returns `undefined`
- `[] | max` (empty list) returns `undefined`
- Works with translation tuple values.

See also

[min](/id/doc_template_filter_filter_min), [minmax](/id/doc_template_filter_filter_minmax)").

-compile({no_auto_import, [max/2]}).

-export([max/2, max/3]).

max(undefined, _Context) ->
    undefined;
max([], _Context) ->
    undefined;
max(List, Context) when is_list(List) ->
    lists:max( [ z_template_compiler_runtime:to_simple_value(E, Context) || E <- List ]).

max(undefined, _Arg, _Context) ->
    undefined;
max(_Value, undefined, _Context) ->
    undefined;
max(Value, Arg, Context) ->
    max([Value, Arg], Context).

