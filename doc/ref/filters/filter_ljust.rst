.. highlight:: django
.. include:: meta-ljust.rst

Justifies the value in a field of a certain width to the left, with spaces.

For example::

  {{ value|ljust:7 }}

When value is ``hello`` then the output is ``hello__`` (with spaces).

Justifying only works for single byte character values. At this moment
there is no support for multi-byte unicode characters.

.. seealso:: :ref:`filter-rjust`, :ref:`filter-center`
