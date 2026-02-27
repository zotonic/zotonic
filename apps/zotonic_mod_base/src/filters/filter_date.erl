%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2024 Marc Worrell
%% @doc 'date' filter, display a date
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

-module(filter_date).
-moduledoc("
Formats a date or datetime according to the format specified in the argument.

The date should be a tuple `{Y,M,D}` and the datetime should be a tuple `{{Y,M,D},{H,I,S}}`. Dates and datetimes are
always assumed to be in local time.

An example:


```django
{{ mydate|date:\"Y-m-d\" }}
```

If mydate is `{2009,6,1}` this returns `2009-06-01` as output.

To show the year of the current date:


```django
{{ now|date:\"Y\" }}
```

See also the [timesince](/id/doc_template_filter_filter_timesince) filter to display a human readable relative time like
10 hours ago.



Timezones
---------

Dates in Zotonic are stored in UTC. If a date is displayed then it is converted to the timezone of the current request
context. This timezone can be one of the following, in order of preference:

*   Preferred timezone set by the user
*   Timezone of the user-agent
*   Default timezone of the site
*   Default timezone of the Zotonic server
*   UTC

A specific timezone can be enforced by adding a second parameter to the date-filter. For example, to display a date in
Amsterdam time:


```django
{{ mydate|date:\"Y-m-d H:i T\":\"Europe/Amsterdam\" }}
```

Instead of the timezone, the following arguments are also accepted:

*   `true` set the timezone to UTC (no conversion, display as-is)
*   `false` use the timezone of the current request context
*   `undefined` use the timezone of the current request context
*   a resource id (integer), set the timezone according to the tz property of the resource, unless the
    the resource’s `date_is_all_day` flag is set, then the timezone is set to UTC (see below for details).

For example, to display the date as entered in the admin:

```django
{{ id.publication_start|date:\"Y-m-d H:i T\":id }}
```

Timezone and *all day* date ranges
----------------------------------

If a resource’s date range is set with the `date_is_all_day` flag then the dates are not converted to or from
UTC but stored as they are entered. This needs to be taken into account when displaying those dates, otherwise a
conversion from (assumed) UTC to the current request timezone is performed and the wrong date might be displayed.

The timezone conversion can be prevented by adding the `date_is_all_day` flag to the date-filter as the timezone.
Example, for displaying the start date of a resource:


```django
{{ id.date_start|date:\"Y-m-d\":id.date_is_all_day }}  (In request tz if not all day)
{{ id.date_start|date:\"Y-m-d\":id }}  (In resource tz, if not all day)
```


Date formatting characters
--------------------------

Date uses the same format as PHP’s date function with some extensions and some omissions.

All supported formatting characters are listed below:

| Character | Description                                                                      | Example output                              |
| --------- | -------------------------------------------------------------------------------- | ------------------------------------------- |
| a         | “a.m.” or “p.m.” (note that this follows Associated Press style and adds periods). | “a.m.”                                      |
| A         | Uppercase “AM” or “PM”.                                                          | “AM”                                        |
| b         | Month, textual, in three lowercase characters.                                   | “jan”                                       |
| c         | ISO-8601 date format.                                                            | “2004-02-12T15:19:21+00:00”                 |
| d         | Day of the month in two digits with leading zero, i.e. “01” to “31”.             | “01”                                        |
| D         | Day of the week, textual, three letters of which the first one uppercase.        | “Mon”, “Fri”                                |
| e         | Show era when date is BCE (Before Common Era, so before the year 1).             | “BCE”                                       |
| E         | Always show era.                                                                 | “BCE”, “CE”                                 |
| f         | If minutes is zero then show only the hour, otherwise the hour and the minutes. Hours are shown using the “g” format character. | “2”, “3:01”                                 |
| F         | Month, textual, full english name with first character in uppercase.             | “January”                                   |
| g         | 12 Hour format without leading zero, i.e. “1” to “12”.                           | “1”                                         |
| G         | 24 Hour format without leading zero, i.e. “0” to “23”.                           | “0”, “15”                                   |
| h         | 12 Hour format with leading zero, i.e. “01” to “12”.                             | “01”                                        |
| H         | 24 Hour format with leading zero, i.e. “00” to “23”.                             | “00”, “15”                                  |
| i         | Minutes with leading zero, i.e. “00” to “59”.                                    | “00”, “46”                                  |
| i         | Daylight saving time flag. “1” if DST is in effect, “0” if no DST.               | “0”                                         |
| j         | Day of the month without leading zero, i.e. “1” to “31”.                         | “1”, “28”                                   |
| l         | (lowercase L) Day of the week, textual, full english name with first character in uppercase. | “Monday”, “Friday”                          |
| L         | Boolean for whether the year is a leap year. Returns the string “True” or “False”. | “True”, “False”                             |
| m         | Month with leading zero, i.e. “01” to “12”.                                      | “01”, “12”                                  |
| M         | Month, textual, in three characters, first character in uppercase.               | “Jan”                                       |
| n         | Month without leading zero, i.e. “1” to “12”.                                    | “1”, “12”                                   |
| N         | Month abbreviation in Associated Press style. March, April, June and July are shown in full. September as “Sept.” and all other months as three letter abbreviations with a full stop appended. | “Jan.”, “June”, “Sept.”, “Dec.”             |
| O         | Difference to Greenwich Mean Time (GMT).                                         | “+0200”                                     |
| P         | Time in 12 hour format with minutes and “a.m.” or “p.m.” appended. Minutes are left off if they are zero, and the strings “midnight” or “noon” if appropriate. | “1 a.m.”, “noon”, “1:30 a.m.”, “12:30 p.m.” |
| r         | RFC 2822 formatted date.                                                         | “Thu, 21 Dec 2000 16:01:07”                 |
| s         | Seconds with leading zero.                                                       | “09”                                        |
| S         | English ordinal suffix for the day of the month, 2 characters; i.e. “st”, “nd”, “rd” or “th”. | “st”, “nd”                                  |
| t         | Number of days in the given month, i.e. “28” to “31”.                            | “30”                                        |
| T         | Timezone used for displaying the date.                                           | “CEST”                                      |
| U         | Seconds since the Unix epoch of January 1, 00:00:00 GMT.                         | 1254911050                                  |
| w         | Day of the week, numeric. 0 For sunday to 6 for saturday.                        | “0”, “6”                                    |
| W         | ISO-8601 week number of the year, starting on mondays.                           | “22”                                        |
| x         | Year in (at least) four digits, in accordance with ISO 8601.                     | “0313”, “-0500”, “2010”                     |
| y         | Year in two digits.                                                              | “01”, “99”                                  |
| Y         | Full year. BCE years are shown as a positive number. Use `e` or `E` to add the era. | “1999”, “2010”, “313”                       |
| z         | Day of the year, i.e. 1 to 366.                                                  | “361”                                       |

To construct a date in a template, the filter also accepts Erlang lists as input, so the following will work:


```django
{{ [1990,10,10]|date:\"j F Y\" }}
```

Will output 10 October 1990. This also works with datetimes:


```django
{{ [[1990,10,10],[10,11,12]]|date:\"j F Y - H:i:s\" }}
```

Will output 10 October 1990 - 10:11:12.

See also

[date_range](/id/doc_template_filter_filter_date_range), [datediff](/id/doc_template_filter_filter_datediff), [timesince](/id/doc_template_filter_filter_timesince), [now](/id/doc_template_tag_tag_now)").

-export([
  date/3,
  date/4
]).

-include_lib("kernel/include/logger.hrl").

date(Date, Format, 0, Context) ->
    % In Zotonic 0.x the timezone argument 0 was used for the current request timezone.
    date(Date, Format, false, Context);
date(Date, Format, 1, Context) ->
    % In Zotonic 0.x the timezone argument 1 was used to use UTC (no conversion)
    ?LOG_DEBUG(#{
        in => zotonic_mod_base,
        text => <<"Filter 'date' now accepts a resource id as the timezone. Use 'UTC' or true to not convert timezones.">>,
        id => 1,
        format => Format
    }),
    date_for_id_tz(Date, Format, 1, Context);
date(Date, Format, Id, Context) when is_integer(Id) ->
    date_for_id_tz(Date, Format, Id, Context);
date(Date, Format, true, Context) ->
    date(Date, Format, z_context:set_tz(<<"UTC">>, Context));
date(Date, Format, false, Context) ->
    date(Date, Format, Context);
date(Date, Format, undefined, Context) ->
    date(Date, Format, false, Context);
date(Date, Format, Tz, Context) ->
    date(Date, Format, z_context:set_tz(Tz, Context)).

date_for_id_tz(Date, Format, Id, Context) ->
    case z_convert:to_bool(m_rsc:p(Id, <<"date_is_all_day">>, Context)) of
        true ->
            date(Date, Format, true, Context);
        false ->
            Tz = m_rsc:p(Id, <<"tz">>, Context),
            date(Date, Format, z_context:set_tz(Tz, Context))
    end.

date(undefined, _FormatStr, _Context) ->
    undefined;
date(<<>>, _FormatStr, _Context) ->
    <<>>;
date([Y,M,D], FormatStr, Context) when is_integer(Y), M >= 1, M =< 12, D >= 1, D =< 31 ->
    date({{z_convert:to_integer(Y),
           z_convert:to_integer(M),
           z_convert:to_integer(D)}, {0,0,0}}, FormatStr, Context);
date([[Y,M,D],[H,I,S]], FormatStr, Context)
    when is_integer(Y), M >= 1, M =< 12, D >= 1, D =< 31,
         H >= 0, H =< 23, I >= 0, I =< 59, S >= 0, S =< 60 ->
    date({{z_convert:to_integer(Y),
           z_convert:to_integer(M),
           z_convert:to_integer(D)},
          {z_convert:to_integer(H),
           z_convert:to_integer(I),
           z_convert:to_integer(S)}},
         FormatStr, Context);
date(Input, FormatStr, Context) when is_binary(Input) ->
    date(z_datetime:to_datetime(Input), FormatStr, Context);
date({{_,_,_} = Date,{_,_,_} = Time} = DT, FormatStr, Context) ->
    try
        z_convert:to_binary( z_datetime:format({Date, Time}, z_convert:to_list(FormatStr), Context) )
    catch
        error:Error ->
            ?LOG_WARNING(#{
                text => <<"Date format on illegal date">>,
                in => zotonic_mod_base,
                format => FormatStr,
                date => DT,
                result => error,
                reason => Error
            }),
            undefined
    end;
date({_,_,_} = Date, FormatStr, Context) ->
    try
        z_convert:to_binary( z_datetime:format({Date, {0,0,0}}, z_convert:to_list(FormatStr), Context) )
    catch
        error:Error ->
            ?LOG_WARNING(#{
                text => <<"Date format on illegal date">>,
                in => zotonic_mod_base,
                format => FormatStr,
                date => Date,
                result => error,
                reason => Error
            }),
            undefined
    end;
date(_Input, _FormatStr, _Context) ->
    undefined.
