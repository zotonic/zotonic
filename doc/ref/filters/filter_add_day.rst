.. highlight:: django
.. include:: meta-add_day.rst

Adds a day to a date. The value must be of the form ``{{Y,M,D},{H,I,S}}``.

For example::

  {{ value|add_day }}

When the value is ``{{2008,12,10},{15,30,0}}``, the output is ``{{2008,12,11},{15,30,0}}``.

The filter has an optional argument which defines the number of days to add::

  {{ value|add_day:3 }}

When the value is ``{{2008,12,10},{15,30,0}}``, the output is ``{{2008,12,13},{15,30,0}}``.

