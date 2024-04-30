.. highlight:: django
.. include:: meta-date_range.rst

.. seealso:: :ref:`filter-date`

Show a date range.

Filter to simplify displaying datetime ranges. When displaying a
datetime range, the display of the dates and times often depends if
the date parts of the datetimes are equal or not.

Take the following code::

  {{ [fromdate, todate]|date_range:[format_ne, sep, format_eq] }}

If the ``dates`` of fromdate and todate are equal then the output will
be as if the following were written::

  {{ fromdate|date:format_ne }}{{ sep }}{{ todate|date:format_eq }}

However, if the dates are not equal then the output will be as if the
following were written::

  {{ fromdate|date:format_ne }}{{ sep }}{{ todate|date:format_ne }}


Timezones
---------

Dates in Zotonic are stored in UTC. If a date is displayed then it is converted to the timezone of the current request context.
This timezone can be one of the following, in order of preference:

  * Preferred timezone set by the user
  * Timezone of the user-agent
  * Default timezone of the site
  * Default timezone of the Zotonic server
  * UTC

A specific timezone can be enforced by adding a third parameter to the date_range-filter.
For example, to display a date range in UTC::

    {{ [fromdate, todate]|date_range:[format_ne, sep, format_eq]:"UTC" }}

Instead of the timezone, the following arguments are also accepted:

  * ``true`` set the timezone to UTC
  * ``false`` leave the timezone as is
  * ``undefined`` leave the timezone as is
  * a resource id (integer), set the timezone according to the `tz` property of the resource

