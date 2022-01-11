.. highlight:: django
.. include:: meta-to_binary.rst
.. seealso:: :ref:`filter-stringify`

Convert the input to a binary value.

Example::

  {{ "Hello"|to_binary }}

Results in the binary value ``<<"Hello">>``.

This filter uses the ``z_convert:to_binary/1`` function.
