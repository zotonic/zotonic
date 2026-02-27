%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'sub_week' filter, subtract one or more weeks from a date

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

-module(filter_sub_week).
-moduledoc("
Subtracts a week from a date. The value must be of the form `{{Y,M,D},{H,I,S}}`.

For example:


```django
{{ value|sub_week }}
```

When the value is `{{2008,12,10},{15,30,0}}` then the output is `{{2008,12,3},{15,30,0}}`.

The filter has an optional argument which defines the number of weeks to subtract:

For example:


```django
{{ value|sub_week:3 }}
```

When the value is `{{2008,12,10},{15,30,0}}` then the output is `{{2008,11,19},{15,30,0}}`.

See also

[sub_day](/id/doc_template_filter_filter_sub_day), [add_week](/id/doc_template_filter_filter_add_week), [sub_month](/id/doc_template_filter_filter_sub_month), [sub_year](/id/doc_template_filter_filter_sub_year)").
-export([sub_week/2, sub_week/3]).

sub_week(undefined, _Context) ->
	undefined;
sub_week(Date, _Context) ->
	z_datetime:prev_week(Date).

sub_week(undefined, _N, _Context) ->
	undefined;
sub_week(Date, N, _Context) ->
	z_datetime:prev_week(Date, N).
