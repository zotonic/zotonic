.. highlight:: django
.. include:: meta-date.rst

Formats a date or datetime according to the format specified in the argument.

The date should be a tuple ``{Y,M,D}`` and the datetime should be a
tuple ``{{Y,M,D},{H,I,S}}``. Dates and datetimes are always assumed to
be in `local time`.

An example::

  {{ mydate|date:"Y-m-d" }}

When mydate is ``{2009,6,1}`` this returns ``2009-06-01`` as output.

See also the :ref:`filter-timesince` filter to display a human
readable `relative` time like `10 hours ago`.

Date uses the same format as PHP's date function with some extensions
and some omissions.

All supported formatting characters are listed below:

+----------+---------------------------------------+---------------------------+
|Character |Description                            |Example output             |
+==========+=======================================+===========================+
|a         |“a.m.” or “p.m.”  (note that this      |“a.m.”                     |
|          |follows Associated Press style and adds|                           |
|          |periods)                               |                           |
+----------+---------------------------------------+---------------------------+
|A         |Uppercase “AM” or “PM”                 |“AM”                       |
+----------+---------------------------------------+---------------------------+
|b         |Month, textual, in three lowercase     |“jan”                      |
|          |characters.                            |                           |
+----------+---------------------------------------+---------------------------+
|c         |ISO-8601 date format                   |“2004-02-12T15:19:21+00:00”|
+----------+---------------------------------------+---------------------------+
|d         |Day of the month in two digits with    |“01”                       |
|          |leading zeros, i.e. “01” to “31”       |                           |
+----------+---------------------------------------+---------------------------+
|D         |Day of the week, textual, three letters|“Mon”, “Fri”               |
|          |of which the first one uppercase.      |                           |
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
|          |i.e. “0” to “23”                       |                           |
+----------+---------------------------------------+---------------------------+
|h         |12 Hour format with leading zero,      |“01”                       |
|          |i.e. “01” to “12”                      |                           |
+----------+---------------------------------------+---------------------------+
|H         |24 Hour format with leading zero,      |“00”, “15”                 |
|          |i.e. “00” to “23”                      |                           |
+----------+---------------------------------------+---------------------------+
|i         |Minutes with leading zero, i.e. “00” to|“00”, “46”                 |
|          |“59”                                   |                           |
+----------+---------------------------------------+---------------------------+
|j         |Day of the month without leading zeros,|“1”, “28”                  |
|          |i.e. “1” to “31”                       |                           |
+----------+---------------------------------------+---------------------------+
|l         |(lowercase L) Day of the week, textual,|“Monday”, “Friday”         |
|          |full english name with first character |                           |
|          |in uppercase.                          |                           |
+----------+---------------------------------------+---------------------------+
|L         |Boolean for whether the year is a leap |"True", "False"            |
|          |year.  Returns the string “True” or    |                           |
|          |“False”.                               |                           |
+----------+---------------------------------------+---------------------------+
|m         |Month with leading zeros, i.e. “01” to |“01”, “12”                 |
|          |“12”                                   |                           |
+----------+---------------------------------------+---------------------------+
|M         |Month, textual, in three characters,   |“Jan”                      |
|          |first character in uppercase.          |                           |
+----------+---------------------------------------+---------------------------+
|n         |Month without leading zeros, i.e. “1”  |“1”, “12”                  |
|          |to “12”                                |                           |
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
|r         |RFC 2822 formatted date.               |“Thu, 21 Dec 2000 16:01:07 |
+----------+---------------------------------------+---------------------------+
|S         |English ordinal suffix for the day of  |“st”, “nd”                 |
|          |the month, 2 characters; i.e. “st”,    |                           |
|          |“nd”, “rd” or “th”                     |                           |
+----------+---------------------------------------+---------------------------+
|t         |Number of days in the given month,     |“30”                       |
|          |i.e. “28” to “31”                      |                           |
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
|y         |Year in two digits.                    |“01”, “99”                 |
+----------+---------------------------------------+---------------------------+
|Y         |Year in four digits.                   |“1999”, “2010”             |
+----------+---------------------------------------+---------------------------+
|z         |Day of the year, i.e. 1 to 366.        |“361”                      |
+----------+---------------------------------------+---------------------------+

.. seealso:: :ref:`filter-date_range`, :ref:`filter-datediff`, :ref:`filter-timesince`
