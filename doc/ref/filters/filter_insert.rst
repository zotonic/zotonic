.. highlight:: django
.. include:: meta-insert.rst

.. seealso:: :ref:`filter-append`

Prepends the argument in front of the value.

For example::

  {{ value|insert:"world " }}

When value is “hello” then the output will be “world hello”.
