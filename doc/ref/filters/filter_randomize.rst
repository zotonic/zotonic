.. highlight:: django
.. include:: meta-randomize.rst

Shuffle a list of values.

For example::

  {{ ["a","b","c"]|randomize }}

The output of this is the same list, but the order of the elements randomized. So for instance: ["c", "a", "b"].

.. seealso:: :ref:`filter-rand`, :ref:`filter-random`

