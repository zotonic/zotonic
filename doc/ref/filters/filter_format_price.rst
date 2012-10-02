.. highlight:: django
.. include:: meta-format_price.rst

Show a price with decimals.

Formats integer and float values as a number with two decimals.

For example::

  {{ value|format_price }}

When the value is the float ``12.1`` then the output is the list ``12.10``.

An undefined price will have the output “-”.

**Note:** the decimal separator is currently always a dot, independent of
the user's language.

.. seealso:: :ref:`filter-format_number`, :ref:`filter-format_integer`
