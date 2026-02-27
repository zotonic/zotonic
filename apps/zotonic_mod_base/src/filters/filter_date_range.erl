%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2024 Marc Worrell
%% @doc 'date_range' filter, display two dates
%% @end

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

-module(filter_date_range).
-moduledoc("
Show a date range.

Filter to simplify displaying datetime ranges. When displaying a datetime range, the display of the dates and times
often depends if the date parts of the datetimes are equal or not.

Take the following code:


```django
{{ [fromdate, todate]|date_range:[format_ne, sep, format_eq] }}
```

If the `dates` of fromdate and todate are equal then the output will be as if the following were written:


```django
{{ fromdate|date:format_ne }}{{ sep }}{{ todate|date:format_eq }}
```

However, if the dates are not equal then the output will be as if the following were written:


```django
{{ fromdate|date:format_ne }}{{ sep }}{{ todate|date:format_ne }}
```



Timezones
---------

Dates in Zotonic are stored in UTC. If a date is displayed then it is converted to the timezone of the current request
context. This timezone can be one of the following, in order of preference:

*   Preferred timezone set by the user
*   Timezone of the user-agent
*   Default timezone of the site
*   Default timezone of the Zotonic server
*   UTC

A specific timezone can be enforced by adding a third parameter to the date_range-filter. For example, to display a
date range in UTC:


```django
{{ [fromdate, todate]|date_range:[format_ne, sep, format_eq]:\"UTC\" }}
```

Instead of the timezone, the following arguments are also accepted:

*   `true` set the timezone to UTC
*   `false` leave the timezone as is
*   `undefined` leave the timezone as is
*   a resource id (integer), set the timezone according to the tz property of the resource

See also

[date](/id/doc_template_filter_filter_date)").
-export([
	date_range/3,
	date_range/4
]).

date_range(Dates, Format, Id, Context) when is_integer(Id) ->
    Tz = m_rsc:p(Id, <<"tz">>, Context),
    date_range(Dates, Format, z_context:set_tz(Tz, Context));
date_range(Dates, Format, Tz, Context) ->
    date_range(Dates, Format, z_context:set_tz(Tz, Context)).

date_range([A, B], [WithDate, Sep, EqDate], Context) ->
	ALocal = z_datetime:to_local(A, Context),
	BLocal = z_datetime:to_local(B, Context),
    case filter_eq_day:eq_day(ALocal, BLocal, Context) of
        true -> [ filter_date:date(A, WithDate, Context), Sep, filter_date:date(B, EqDate, Context) ];
        false -> [ filter_date:date(A, WithDate, Context), Sep, filter_date:date(B, WithDate, Context) ]
    end.

