.. highlight:: django
.. include:: meta-sub_year.rst

Subtracts a year from a date. The value must be of the form ``{{Y,M,D},{H,I,S}}``.

For example::

  {{ value|sub_year }}

When the value is ``{{2008,12,10},{15,30,0}}`` then the output is ``{{2007,12,10},{15,30,0}}``.

The filter has an optional argument which defines the number of years to subtract:

For example::

  {{ value|sub_year:3 }}

When the value is ``{{2008,12,10},{15,30,0}}`` then the output is ``{{2005,12,10},{15,30,0}}``.


.. seealso:: :ref:`filter-sub_day`, :ref:`filter-sub_week`, :ref:`filter-sub_month`, :ref:`filter-add_year`
