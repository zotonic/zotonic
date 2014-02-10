.. highlight:: django
.. include:: meta-trim.rst

Removes whitespace at the start and end of a string.

For example::

  {{ value|trim }}

When the value is ``"   hello   "`` then the output is ``"hello"``.

Internally, this calls ``z_string:trim/1`` to perform the
trimming.
