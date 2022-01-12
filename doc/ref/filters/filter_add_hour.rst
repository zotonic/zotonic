.. highlight:: django
.. include:: meta-add_hour.rst

.. seealso:: :ref:`filter-sub_hour`, :ref:`filter-add_day`, :ref:`filter-add_week`, :ref:`filter-add_month`, :ref:`filter-add_year`

Adds an hour to a date. The value must be of the form ``{{Y,M,D},{H,I,S}}``.

For example::

  {{ value|add_hour }}

When the value is ``{{2008,12,10},{15,30,0}}``, the output is ``{{2008,12,10},{16,30,0}}``.

The filter has an optional argument which defines the number of hours to add::

  {{ value|add_hour:3 }}

When the value is ``{{2008,12,10},{15,30,0}}``, the output is ``{{2008,12,10},{18,30,0}}``.
