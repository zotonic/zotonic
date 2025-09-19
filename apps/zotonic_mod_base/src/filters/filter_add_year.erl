%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'add_year' filter, add one or more years to a date

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

-module(filter_add_year).
-moduledoc("
Adds a year to a date. The value must be of the form `{{Y,M,D},{H,I,S}}`.

For example:


```django
{{ value|add_year }}
```

When the value is `{{2008,12,10},{15,30,0}}`, the output is `{{2009,12,10},{15,30,0}}`.

The filter has an optional argument which defines the number of years to add:


```django
{{ value|add_year:3 }}
```

When the value is `{{2008,12,10},{15,30,0}}`, the output is `{{2011,12,10},{15,30,0}}`.
").
-export([add_year/2, add_year/3]).

add_year(undefined, _Context) ->
	undefined;
add_year(Date, _Context) ->
	z_datetime:next_year(Date).

add_year(undefined, _N, _Context) ->
	undefined;
add_year(Date, N, _Context) ->
	z_datetime:next_year(Date, N).

