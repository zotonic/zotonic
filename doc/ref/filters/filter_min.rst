.. highlight:: django
.. include:: meta-min.rst

Take the minimum of the filter value and its first argument.

The following::

  {% print 102|min:103 %}

Prints ``102``.

.. seealso:: :ref:`filter-max`
