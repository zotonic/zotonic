.. highlight:: django
.. include:: meta-minmax.rst
.. seealso:: :ref:`filter-max`, :ref:`filter-min`

Force the given value in the given range.

This clamps the filter value between the two filter arguments.

Example::

  {% print 3|to_integer|minmax:10:20 %}

This will print ``10``, since that is the minimum value allowed.

Passing in ``undefined`` will not clamp the value but return ``undefined``.
