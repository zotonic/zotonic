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

