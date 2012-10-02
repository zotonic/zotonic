.. highlight:: django
.. include:: meta-element.rst

Select an element from a tuple or list of tuples.

For example::

  {{ value|element:1 }}

When value is a list of tuples ``[{312,0.34}, {200,0.81}]`` then the
output is the list ``[312,200]``.

When value is just a tuple, ``{123, 22, 11}``, the output of
``|element:1`` is ``123``.

.. seealso:: :ref:`filter-before`, :ref:`filter-after`
