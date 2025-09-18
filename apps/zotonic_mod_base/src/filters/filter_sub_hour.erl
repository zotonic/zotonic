%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2021 Maas-Maarten Zeeman
%% @doc 'sub_hour' filter, subtract hours from a date

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

-module(filter_sub_hour).
-moduledoc("
See also

[add\\_hour](/id/doc_template_filter_filter_add_hour), [sub\\_day](/id/doc_template_filter_filter_sub_day), [sub\\_week](/id/doc_template_filter_filter_sub_week), [sub\\_month](/id/doc_template_filter_filter_sub_month), [sub\\_year](/id/doc_template_filter_filter_sub_year)

Subtracts an hour from a date. The value must be of the form `{{Y,M,D},{H,I,S}}`.

For example:


```django
{{ value|sub_hour }}
```

When the value is `{{2008,12,10},{15,30,0}}` then the output is `{{2008,12,10},{14,30,0}}`.

The filter has an optional argument which defines the number of hours to subtract:

For example:


```django
{{ value|sub_hour:3 }}
```

When the value is `{{2008,12,10},{15,30,0}}` then the output is `{{2008,12,10},{12,30,0}}`.
").
-export([sub_hour/2, sub_hour/3]).

sub_hour(undefined, _Context) ->
	undefined;
sub_hour(Date, _Context) ->
	z_datetime:prev_hour(Date).

sub_hour(undefined, _N, _Context) ->
	undefined;
sub_hour(Date, N, _Context) ->
	z_datetime:prev_hour(Date, N).

