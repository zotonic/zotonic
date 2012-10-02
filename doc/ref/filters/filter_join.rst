.. highlight:: django
.. include:: meta-join.rst

Joins the elements of a list. Joins the elements of the input list
together, separated by the argument.

For example::

  {{ value|join:", " }}

When value is the list ``["hello", "world"]`` then the output will be
``"hello, world"``.

.. seealso:: :ref:`filter-element`, :ref:`filter-tail`, :ref:`filter-split`
