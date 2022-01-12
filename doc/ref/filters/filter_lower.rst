.. highlight:: django
.. include:: meta-lower.rst

.. seealso:: :ref:`filter-upper`

Translates the value to lower case.

For example::

  {{ value|lower }}

When value is “Hello World” then the output is “hello world”.

**Note:** There is partial support for multi-byte unicode characters.
