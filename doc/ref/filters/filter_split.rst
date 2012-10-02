.. highlight:: django
.. include:: meta-split.rst

Splits the filter value into a list of values.

The input value is split by the filter argument, for example::

  {{ "foo bar baz"|split:" " }}

Will create the list ``["foo", "bar", "baz"]``.

.. seealso:: :ref:`filter-join`
