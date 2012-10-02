.. highlight:: django
.. include:: meta-sub_week.rst

Subtracts a week from a date. The value must be of the form ``{{Y,M,D},{H,I,S}}``.

For example::

  {{ value|sub_week }}

When the value is ``{{2008,12,10},{15,30,0}}`` then the output is ``{{2008,12,3},{15,30,0}}``.

The filter has an optional argument which defines the number of weeks to subtract:

For example::

  {{ value|sub_week:3 }}

When the value is ``{{2008,12,10},{15,30,0}}`` then the output is ``{{2008,11,19},{15,30,0}}``.


.. seealso:: :ref:`filter-sub_day`, :ref:`filter-add_week`, :ref:`filter-sub_month`, :ref:`filter-sub_year`
