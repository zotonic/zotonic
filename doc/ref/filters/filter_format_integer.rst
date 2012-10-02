.. highlight:: django
.. include:: meta-format_integer.rst

Show an integer value.

Formats an integer value as a list, assuming a radix of ten.

For example::

  {{ value|format_integer }}

When the value is the integer 123 then the output is the list ``123``.

The format_integer filter has an optional argument to pad the
returned string with zeros to a fixed length. For example::

  {{ 123|format_integer:5 }}

Will output ``00123``.  And when the number does not fit::

  {{ 123|format_integer:2 }}

Will output "**".

**Note:** This option only works for positive integers.

.. seealso:: :ref:`filter-format_number`, :ref:`filter-format_price`
