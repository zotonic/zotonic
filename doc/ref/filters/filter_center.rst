.. highlight:: django
.. include:: meta-center.rst

Centers the value in a field of a certain width using spaces.

For example::

  {{ value|center:7 }}

When value is ``hello`` then the output is ``_hello_`` (with spaces).

Centering only works for single byte character values. At this moment
there is no support for multi-byte unicode characters.

.. seealso:: :ref:`filter-rjust`, :ref:`filter-ljust`
