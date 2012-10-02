.. highlight:: django
.. include:: meta-first.rst

Returns the first character or element.

Returns the first byte of a binary or the first element of a list.  An
empty binary is returned when the input is empty.

For example::

  {{ value|first }}

When the value is ``hello`` then the output is ``h``.

**Note:** This function is not safe to use with multibyte character
values, use with care.

For a regular list::

  {{ [1,2,3]|first }}

The filtered value is ``1``.

.. seealso:: :ref:`filter-tail`, :ref:`filter-last`

