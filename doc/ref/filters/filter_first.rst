.. highlight:: django
.. include:: meta-first.rst

.. seealso:: :ref:`filter-tail`, :ref:`filter-last`

Returns the first character or element.

Returns the first byte of a binary or the first element of a list.  An
empty binary is returned when the input is empty.

For example::

  {{ value|first }}

If the value is ``hello`` then the output is ``h``.

**Note:** This function is safe to use with multibyte character
values, if the input is a binary.

For a regular list::

  {{ [1,2,3]|first }}

The filtered value is ``1``.

It is also possible to fetch the first N elements or characters::

  {{ [1,2,3]|first:2 }}

The filtered value is ``[1,2]``.

Or, with a string::

  {{ "hello"|first:2 }}

The filtered value is ``"he"``.
