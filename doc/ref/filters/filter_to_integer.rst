.. highlight:: django
.. include:: meta-to_integer.rst

Convert the input to an integer value.

Example::

  {{ "123"|to_integer }}

Results in the integer value ``123``.

This filter uses the ``z_convert:to_integer/1`` function.

.. seealso:: :ref:`filter-to_binary`, :ref:`filter-format_number`,
             :ref:`filter-format_integer`
