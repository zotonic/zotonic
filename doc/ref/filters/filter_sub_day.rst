.. highlight:: django
.. include:: meta-sub_day.rst

Subtracts a day from a date. The value must be of the form ``{{Y,M,D},{H,I,S}}``.

For example::

  {{ value|sub_day }}

When the value is ``{{2008,12,10},{15,30,0}}`` then the output is ``{{2008,12,9},{15,30,0}}``.

The filter has an optional argument which defines the number of days to subtract:

For example::

  {{ value|sub_day:3 }}

When the value is ``{{2008,12,10},{15,30,0}}`` then the output is ``{{2008,12,7},{15,30,0}}``.


.. seealso:: :ref:`filter-add_day`, :ref:`filter-sub_week`, :ref:`filter-sub_month`, :ref:`filter-sub_year`
