%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'add_week' filter, add one or more weeks to a date

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

-module(filter_add_week).
-moduledoc("
Adds a week to a date. The value must be of the form `{{Y,M,D},{H,I,S}}`.

For example:


```django
{{ value|add_week }}
```

When the value is `{{2008,12,10},{15,30,0}}`, the output is `{{2008,12,17},{15,30,0}}`.

The filter has an optional argument which defines the number of weeks to add:


```django
{{ value|add_week:4 }}
```

When the value is `{{2008,12,10},{15,30,0}}`, the output is `{{2009,1,7},{15,30,0}}`.
").
-export([add_week/2, add_week/3]).

add_week(undefined, _Context) ->
	undefined;
add_week(Date, _Context) ->
	z_datetime:next_week(Date).

add_week(undefined, _N, _Context) ->
	undefined;
add_week(Date, N, _Context) ->
	z_datetime:next_week(Date, N).

