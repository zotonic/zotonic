.. highlight:: django
.. include:: meta-index_of.rst

.. seealso:: :ref:`filter-element`

Returns the index of the first occurrence of the item in the given
list.

For example::

  {{ [44,11,2,443,2]|index_of:11 }}

Returns ``2``.

**Note:** Erlang list indices are always 1-based.
