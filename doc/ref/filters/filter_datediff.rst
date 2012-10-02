.. highlight:: django
.. include:: meta-datediff.rst

Calculate the difference between two dates, returning a single part of
that difference.

The filter takes a list with 2 parts `[start, end]` as date range
argument.

The filter argument the "part" that will be extracted, and is one of
`Y`, `M`, `D`, `H`, `I`, `S`.

Example, where start = 2012-02-02, end = 2012-03-01::

  {{ [end, start]|datediff:"M" }}

Returns `1`, since the difference in months between those 2 dates is 1.

.. seealso:: :ref:`filter-date`
