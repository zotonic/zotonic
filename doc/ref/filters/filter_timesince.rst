.. highlight:: django
.. include:: meta-timesince.rst

Show a readable version of a date/time difference.

Translates the difference between two dates into a simple readable string like "2 minutes, 10 seconds ago".

Optionally takes an argument with the date to compare against, which is by default the current local date/time.

Example::

  {{ my_date|timesince }}

When "my_date" is ``{{2008,12,10},{15,30,0}}`` and the current
date/time is ``{{2009,11,4},{13,50,0}}`` then this outputs "10 months,
24 days ago".  When the time value is in the future then it outputs
a string like "in X minutes".

This function does not take daylight saving changes into account.

.. seealso:: :ref:`filter-date`
