.. highlight:: django
.. include:: meta-rjust.rst

Justifies the value in a field of a certain width to the right, using spaces.

For example::

  {{ value|rjust:7 }}

When value is ``hello`` then the output is ``__hello`` (with spaces).

Justifying only works for single byte character values. At this moment
there is no support for multi-byte unicode characters.

.. seealso:: :ref:`filter-ljust`, :ref:`filter-center`
