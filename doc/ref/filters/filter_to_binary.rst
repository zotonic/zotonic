.. highlight:: django
.. include:: meta-to_binary.rst
.. seealso:: :ref:`filter-stringify`

Convert the input to a binary value.

Example::

  {{ 42|to_binary }}

Results in the binary value ``<<"42">>``.

This filter uses the ``z_convert:to_binary/1`` function.
