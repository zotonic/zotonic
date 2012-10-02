.. highlight:: django
.. include:: meta-rand.rst

Generates a random number.  The number is from, and including, 1 up
to, and including, the input value.

Example::

  {{ 100|rand|format_integer }}

Might output “42”.

The rand filter can also generate a floating point value by given it a
floating point number or a string representing a floating point number
as input.  It will generate a number from, but not including, 0 to,
and including, the input value::

  {{ "4.0"|rand|format_number }}

Might output “3.1415926536”

.. seealso:: :ref:`filter-randomize`, :ref:`filter-random`

