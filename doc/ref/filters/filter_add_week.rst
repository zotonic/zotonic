.. highlight:: django
.. include:: meta-add_week.rst

Adds a week to a date. The value must be of the form ``{{Y,M,D},{H,I,S}}``.

For example::

  {{ value|add_week }}

When the value is ``{{2008,12,10},{15,30,0}}``, the output is ``{{2008,12,17},{15,30,0}}``.

The filter has an optional argument which defines the number of weeks to add::

  {{ value|add_week:4 }}

When the value is ``{{2008,12,10},{15,30,0}}``, the output is ``{{2009,1,7},{15,30,0}}``.

