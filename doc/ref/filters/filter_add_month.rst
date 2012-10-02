.. highlight:: django
.. include:: meta-add_month.rst

Adds a month to a date. The value must be of the form ``{{Y,M,D},{H,I,S}}``.

For example::

  {{ value|add_month }}

When the value is ``{{2008,12,10},{15,30,0}}``, the output is ``{{2009,1,10},{15,30,0}}``.

The filter has an optional argument which defines the number of months to add::

  {{ value|add_month:3 }}

When the value is ``{{2008,12,10},{15,30,0}}``, the output is ``{{2009,3,10},{15,30,0}}``.

