%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2021 Maas-Maarten Zeeman
%% @doc 'add_hour' filter, add hours to a date

%% Copyright 2021 Maas-Maarten Zeeman
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

-module(filter_add_hour).
-moduledoc("
Adds an hour to a date. The value must be of the form `{{Y,M,D},{H,I,S}}`.

For example:


```django
{{ value|add_hour }}
```

When the value is `{{2008,12,10},{15,30,0}}`, the output is `{{2008,12,10},{16,30,0}}`.

The filter has an optional argument which defines the number of hours to add:


```django
{{ value|add_hour:3 }}
```

When the value is `{{2008,12,10},{15,30,0}}`, the output is `{{2008,12,10},{18,30,0}}`.

See also

[sub_hour](/id/doc_template_filter_filter_sub_hour), [add_day](/id/doc_template_filter_filter_add_day), [add_week](/id/doc_template_filter_filter_add_week), [add_month](/id/doc_template_filter_filter_add_month), [add_year](/id/doc_template_filter_filter_add_year)").
-export([add_hour/2, add_hour/3]).

add_hour(undefined, _Context) ->
	undefined;
add_hour(Date, Context) ->
	add_hour(Date, 1, Context).

add_hour(undefined, _N, _Context) ->
	undefined;
add_hour(Date, N, _Context) ->
	z_datetime:next_hour(Date, N).
