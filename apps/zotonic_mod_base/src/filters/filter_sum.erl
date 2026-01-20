%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2026 Maas-Maarten Zeeman
%% @doc Sum a list of numbers.

%% Copyright 2026 Maas-Maarten Zeeman
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

-module(filter_sum).
-moduledoc("
Sum a list of numbers.

This filter calculates the sum of all numeric values in a list.  Non-numeric values are
automatically filtered out.

For example:

```django
    {{ [1, 2, 3, 4, 5] | sum }}
```

Results in `15`.

When applied to a list with mixed types:

```django
    {{ [10, 20, \"text\", 30] | sum }}
```

Results in `60`. The string \"text\" is ignored.

If the input is `undefined`, the filter returns `undefined`.

This filter is useful for analytics and reporting, such as summing values
extracted from lists of maps or resources.
").

-export([sum/2]).

sum(undefined, _Context) ->
    undefined;
sum(Values, _Context) ->
    lists:sum([ V || V <- Values, is_number(V) ]).
