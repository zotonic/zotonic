.. highlight:: django
.. include:: meta-upper.rst
.. seealso:: :ref:`filter-capfirst`, :ref:`filter-lower`

Translates the value to upper case.

For example::

  {{ value|upper }}

When value is “Hello World” then the output is “HELLO WORLD”.

**Note:** There is partial support for multi-byte unicode characters.
