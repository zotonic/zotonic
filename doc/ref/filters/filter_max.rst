.. highlight:: django
.. include:: meta-max.rst

Take the maximum of the filter value and its first argument.

The following::

  {% print 102|to_integer|max:103 %}

Prints ``103``.

.. seealso:: :ref:`filter-min`, :ref:`filter-minmax`
