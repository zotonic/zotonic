.. highlight:: django
.. include:: meta-add_year.rst

Adds a year to a date. The value must be of the form ``{{Y,M,D},{H,I,S}}``.

For example::

  {{ value|add_year }}

When the value is ``{{2008,12,10},{15,30,0}}``, the output is ``{{2009,12,10},{15,30,0}}``.

The filter has an optional argument which defines the number of years to add::

  {{ value|add_year:3 }}

When the value is ``{{2008,12,10},{15,30,0}}``, the output is ``{{2011,12,10},{15,30,0}}``.

