.. highlight:: django
.. include:: meta-sub_month.rst

Subtracts a month from a date. The value must be of the form ``{{Y,M,D},{H,I,S}}``.

For example::

  {{ value|sub_month }}

When the value is ``{{2008,12,10},{15,30,0}}`` then the output is ``{{2008,11,10},{15,30,0}}``.

The filter has an optional argument which defines the number of months to subtract:

For example::

  {{ value|sub_month:3 }}

When the value is ``{{2008,12,10},{15,30,0}}`` then the output is ``{{2008,12,7},{15,30,0}}``.


.. seealso:: :ref:`filter-sub_day`, :ref:`filter-sub_week`, :ref:`filter-add_month`, :ref:`filter-sub_year`
