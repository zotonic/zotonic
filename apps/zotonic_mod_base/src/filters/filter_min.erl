%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2026 Marc Worrell
%% @doc Take minimum value.

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

-module(filter_min).
-moduledoc("
See also

[max](/id/doc_template_filter_filter_max)

Take the minimum of the filter value and its first argument.

Usage with two values
---------------------

Take the maximum of the filter value and its first argument.

```django
   {% print 102 | to_integer | max:103 %}
```

Prints `102`.

Usage with two values
---------------------

Take the minimum of the filter value and its first argument::

   {{ 102 | min:103 }}

Prints `103`.

Usage with lists
----------------

Find the minimum value in a list::

```django

   {% print [1, 5, 3, 9, 2] | min %}
```

Prints `1`.

Edge cases
----------

- `undefined | min` returns `undefined`
- `undefined | min:1000` returns `undefined`
- `[] | max` (empty list) returns `undefined`
- Works with translation tuple values.
").

-compile({no_auto_import, [min/2]}).

-export([min/2, min/3]).

min(undefined, _Context) ->
    undefined;
min([], _Context) ->
    undefined;
min(List, Context) ->
    lists:min([ z_template_compiler_runtime:to_simple_value(E, Context) || E <- List ]).

min(undefined, _Arg, _Context) ->
    undefined;
min(_Value, undefined, _Context) ->
    undefined;
min(Value, Arg, Context) ->
    min([Value, Arg], Context).

