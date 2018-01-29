.. highlight:: django
.. include:: meta-date.rst

Formats a date or datetime according to the format specified in the argument.

The date should be a tuple ``{Y,M,D}`` and the datetime should be a
tuple ``{{Y,M,D},{H,I,S}}``. Dates and datetimes are always assumed to
be in `local time`.

An example::

  {{ mydate|date:"Y-m-d" }}

If mydate is ``{2009,6,1}`` this returns ``2009-06-01`` as output.

To show the year of the current date::

  {{ now|date:"Y" }}

See also the :ref:`filter-timesince` filter to display a human
readable `relative` time like `10 hours ago`.


Timezones
---------

Dates in Zotonic are stored in UTC. If a date is displayed then it is converted to the timezone of the current request context.
This timezone can be one of the following, in order of preference:

  * Preferred timezone set by the user
  * Timezone of the user-agent
  * Default timezone of the site
  * Default timezone of the Zotonic server
  * UTC

A specific timezone can be enforced by adding a second parameter to the date-filter.
For example, to display a date in UTC::

    {{ mydate|date:"Y-m-d H:i T":"UTC" }}


Timezone and *all day* date ranges
----------------------------------

If a resource’s date range is set with the *date_is_all_day* flag then the dates are not converted to or from UTC but stored as-is.
This needs to be taken into account when displaying those dates, otherwise a conversion from (assumed) UTC to the current timezone is
performed and the wrong date might be displayed.

The timezone conversion can be prevented by adding the *date_is_all_day* flag to the date-filter as the timezone.
Example, for displaying the start date of a resource::

    {{ id.date_start|date:"Y-m-d":id.date_is_all_day }}


Date formatting characters
--------------------------

Date uses the same format as PHP’s date function with some extensions
and some omissions.

All supported formatting characters are listed below:

+----------+---------------------------------------+---------------------------+
|Character |Description                            |Example output             |
+==========+=======================================+===========================+
|a         |“a.m.” or “p.m.”  (note that this      |“a.m.”                     |
|          |follows Associated Press style and adds|                           |
|          |periods).                              |                           |
+----------+---------------------------------------+---------------------------+
|A         |Uppercase “AM” or “PM”.                |“AM”                       |
+----------+---------------------------------------+---------------------------+
|b         |Month, textual, in three lowercase     |“jan”                      |
|          |characters.                            |                           |
+----------+---------------------------------------+---------------------------+
|c         |ISO-8601 date format.                  |“2004-02-12T15:19:21+00:00”|
+----------+---------------------------------------+---------------------------+
|d         |Day of the month in two digits with    |“01”                       |
|          |leading zero, i.e. “01” to “31”.       |                           |
+----------+---------------------------------------+---------------------------+
|D         |Day of the week, textual, three letters|“Mon”, “Fri”               |
|          |of which the first one uppercase.      |                           |
+----------+---------------------------------------+---------------------------+
|e         |Show era when date is BCE (Before      |“BCE”                      |
|          |Common Era, so before the year 1).     |                           |
+----------+---------------------------------------+---------------------------+
|E         |Always show era.                       |“BCE”, “CE”                |
+----------+---------------------------------------+---------------------------+
|f         |If minutes is zero then show only the  |“2”, “3:01”                |
|          |hour, otherwise the hour and the       |                           |
|          |minutes. Hours are shown using the “g” |                           |
|          |format character.                      |                           |
+----------+---------------------------------------+---------------------------+
|F         |Month, textual, full english name with |“January”                  |
|          |first character in uppercase.          |                           |
+----------+---------------------------------------+---------------------------+
|g         |12 Hour format without leading zero,   |“1”                        |
|          |i.e. “1” to “12”.                      |                           |
+----------+---------------------------------------+---------------------------+
|G         |24 Hour format without leading zero,   |“0”, “15”                  |
|          |i.e. “0” to “23”.                      |                           |
+----------+---------------------------------------+---------------------------+
|h         |12 Hour format with leading zero,      |“01”                       |
|          |i.e. “01” to “12”.                     |                           |
+----------+---------------------------------------+---------------------------+
|H         |24 Hour format with leading zero,      |“00”, “15”                 |
|          |i.e. “00” to “23”.                     |                           |
+----------+---------------------------------------+---------------------------+
|i         |Minutes with leading zero, i.e. “00” to|“00”, “46”                 |
|          |“59”.                                  |                           |
+----------+---------------------------------------+---------------------------+
|i         |Daylight saving time flag. “1” if DST  |“0”                        |
|          |is in effect, “0” if no DST.           |                           |
+----------+---------------------------------------+---------------------------+
|j         |Day of the month without leading zero, |“1”, “28”                  |
|          |i.e. “1” to “31”.                      |                           |
+----------+---------------------------------------+---------------------------+
|l         |(lowercase L) Day of the week, textual,|“Monday”, “Friday”         |
|          |full english name with first character |                           |
|          |in uppercase.                          |                           |
+----------+---------------------------------------+---------------------------+
|L         |Boolean for whether the year is a leap |"True", "False"            |
|          |year.  Returns the string “True” or    |                           |
|          |“False”.                               |                           |
+----------+---------------------------------------+---------------------------+
|m         |Month with leading zero, i.e. “01” to  |“01”, “12”                 |
|          |“12”.                                  |                           |
+----------+---------------------------------------+---------------------------+
|M         |Month, textual, in three characters,   |“Jan”                      |
|          |first character in uppercase.          |                           |
+----------+---------------------------------------+---------------------------+
|n         |Month without leading zero, i.e. “1”   |“1”, “12”                  |
|          |to “12”.                               |                           |
+----------+---------------------------------------+---------------------------+
|N         |Month abbreviation in Associated Press |“Jan.”, “June”, “Sept.”,   |
|          |style. March, April, June and July are |“Dec.”                     |
|          |shown in full. September as “Sept.” and|                           |
|          |all other months as three letter       |                           |
|          |abbreviations with a full stop         |                           |
|          |appended.                              |                           |
+----------+---------------------------------------+---------------------------+
|O         |Difference to Greenwich Mean Time      |“+0200”                    |
|          |(GMT).                                 |                           |
+----------+---------------------------------------+---------------------------+
|P         |Time in 12 hour format with minutes and|“1 a.m.”, “noon”, “1:30    |
|          |“a.m.” or “p.m.”  appended. Minutes are|a.m.”, “12:30 p.m.”        |
|          |left off if they are zero, and the     |                           |
|          |strings “midnight” or “noon” if        |                           |
|          |appropriate.                           |                           |
+----------+---------------------------------------+---------------------------+
|r         |RFC 2822 formatted date.               |“Thu, 21 Dec 2000 16:01:07”|
+----------+---------------------------------------+---------------------------+
|s         |Seconds with leading zero.             |“09”                       |
+----------+---------------------------------------+---------------------------+
|S         |English ordinal suffix for the day of  |“st”, “nd”                 |
|          |the month, 2 characters; i.e. “st”,    |                           |
|          |“nd”, “rd” or “th”.                    |                           |
+----------+---------------------------------------+---------------------------+
|t         |Number of days in the given month,     |“30”                       |
|          |i.e. “28” to “31”.                     |                           |
+----------+---------------------------------------+---------------------------+
|T         |Timezone used for displaying the date. |“CEST”                     |
+----------+---------------------------------------+---------------------------+
|U         |Seconds since the Unix epoch of January|1254911050                 |
|          |1, 00:00:00 GMT.                       |                           |
+----------+---------------------------------------+---------------------------+
|w         |Day of the week, numeric.  0 For sunday|“0”, “6”                   |
|          |to 6 for saturday.                     |                           |
+----------+---------------------------------------+---------------------------+
|W         |ISO-8601 week number of the year,      |“22”                       |
|          |starting on mondays.                   |                           |
+----------+---------------------------------------+---------------------------+
|x         |Year in (at least) four digits, in     |“0313”, “-0500”, “2010”    |
|          |accordance with ISO 8601.              |                           |
+----------+---------------------------------------+---------------------------+
|y         |Year in two digits.                    |“01”, “99”                 |
+----------+---------------------------------------+---------------------------+
|Y         |Full year. BCE years are shown as a    |“1999”, “2010”, “313”      |
|          |positive number. Use ``e`` or ``E`` to |                           |
|          |add the era.                           |                           |
+----------+---------------------------------------+---------------------------+
|z         |Day of the year, i.e. 1 to 366.        |“361”                      |
+----------+---------------------------------------+---------------------------+


To construct a date in a template, the filter also accepts Erlang
lists as input, so the following will work::

  {{ [1990,10,10]|date:"j F Y" }}

Will output `10 October 1990`. This also works with datetimes::

  {{ [[1990,10,10],[10,11,12]]|date:"j F Y - H:i:s" }}

Will output `10 October 1990 - 10:11:12`.




.. seealso:: :ref:`filter-date_range`, :ref:`filter-datediff`, :ref:`filter-timesince`, :ref:`tag-now`
