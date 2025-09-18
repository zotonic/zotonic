%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2024 Marc Worrell
%% @doc 'add_month' filter, add one or more months to a date
%% @enddoc

%% Copyright 2010-2024 Marc Worrell
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

-module(filter_add_month).
-moduledoc("
Adds a month to a date. The value must be of the form `{{Y,M,D},{H,I,S}}`.

For example:


```django
{{ value|add_month }}
```

When the value is `{{2008,12,10},{15,30,0}}`, the output is `{{2009,1,10},{15,30,0}}`.

The filter has an optional argument which defines the number of months to add:


```django
{{ value|add_month:3 }}
```

When the value is `{{2008,12,10},{15,30,0}}`, the output is `{{2009,3,10},{15,30,0}}`.
").
-export([add_month/2, add_month/3]).

add_month(undefined, _Context) ->
	undefined;
add_month(Date, _Context) ->
	z_datetime:next_month(Date).

add_month(undefined, _N, _Context) ->
	undefined;
add_month(Date, 0, _Context) ->
	Date;
add_month(Date, N, _Context) when is_integer(N) ->
	z_datetime:next_month(Date, N).
