.. highlight:: django
.. include:: meta-random.rst

Returns a random value from a list of values.  When the input is an
empty list or not a list then the result is undefined.

For example::

  {{ ["a","b","c"]|random }}

The output of this is one of "a", "b" or "c".

.. seealso:: :ref:`filter-randomize`, :ref:`filter-rand`
