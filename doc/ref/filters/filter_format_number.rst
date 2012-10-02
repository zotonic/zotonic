.. highlight:: django
.. include:: meta-format_number.rst

Show an integer or float.

Formats integer and float values as a list, assuming a radix of ten.

For example::

  {{ value|formaat_number }}

When the value is the float ``12.0`` then the output is the list ``12.0``.

.. seealso:: :ref:`filter-format_integer`, :ref:`filter-format_price`
