.. highlight:: django
.. include:: meta-capfirst.rst

Converts the first character of the value to uppercase.

For example::

  {{ value|capfirst }}

When value is ``hello world`` then the output is ``Hello world``.

At the moment this only works for the characters a through z. Accented
characters (like ü) are not yet supported.

.. seealso:: :ref:`filter-upper`
