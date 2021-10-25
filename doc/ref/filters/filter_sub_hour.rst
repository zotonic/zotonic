.. highlight:: django
.. include:: meta-sub_hour.rst

Subtracts an hour from a date. The value must be of the form ``{{Y,M,D},{H,I,S}}``.

For example::

  {{ value|sub_hour }}

When the value is ``{{2008,12,10},{15,30,0}}`` then the output is ``{{2008,12,10},{14,30,0}}``.

The filter has an optional argument which defines the number of hours to subtract:

For example::

  {{ value|sub_hour:3 }}

When the value is ``{{2008,12,10},{15,30,0}}`` then the output is ``{{2008,12,10},{12,30,0}}``.


.. seealso:: :ref:`filter-add_hour`, :ref:`filter-sub_day`,  :ref:`filter-sub_week`, :ref:`filter-sub_month`, :ref:`filter-sub_year`
