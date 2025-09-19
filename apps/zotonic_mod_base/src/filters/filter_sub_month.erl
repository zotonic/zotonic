%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'sub_month' filter, subtract one or more months from a date

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

-module(filter_sub_month).
-moduledoc("
See also

[sub\\_day](/id/doc_template_filter_filter_sub_day), [sub\\_week](/id/doc_template_filter_filter_sub_week), [add\\_month](/id/doc_template_filter_filter_add_month), [sub\\_year](/id/doc_template_filter_filter_sub_year)

Subtracts a month from a date. The value must be of the form `{{Y,M,D},{H,I,S}}`.

For example:


```django
{{ value|sub_month }}
```

When the value is `{{2008,12,10},{15,30,0}}` then the output is `{{2008,11,10},{15,30,0}}`.

The filter has an optional argument which defines the number of months to subtract:

For example:


```django
{{ value|sub_month:3 }}
```

When the value is `{{2008,12,10},{15,30,0}}` then the output is `{{2008,12,7},{15,30,0}}`.
").
-export([sub_month/2, sub_month/3]).

sub_month(undefined, _Context) ->
	undefined;
sub_month(Date, Context) ->
	filter_add_month:add_month(Date, -1, Context).

sub_month(undefined, _N, _Context) ->
	undefined;
sub_month(Date, N, Context) ->
	filter_add_month:add_month(Date, -N, Context).

