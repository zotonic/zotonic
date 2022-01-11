.. highlight:: django
.. include:: meta-join.rst

.. seealso:: :ref:`filter-element`, :ref:`filter-tail`, :ref:`filter-split`

Joins the elements of a list. Joins the elements of the input list
together, separated by the argument.

For example::

  {{ value|join:", " }}

If the value is the list ``["hello", "world"]`` then the output will be
``"hello, world"``.

It is possible to use a special separator between the last two elements of the list,
for the list ``[ "Jan", "Piet", "Klaas" ]`` the following example::

  {{ list|join:", ":_"or" }} }}

Gives as result::

  Jan, Piet or Klaas

The spaces around the last separator are added by the filter.
