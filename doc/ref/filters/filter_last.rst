.. highlight:: django
.. include:: meta-last.rst

Returns the last character or element.

Returns the last element of the value.  When the value is a list then
the last element of the list is returned, when the value is a binary
then the last `byte` of the binary is returned.

For example::

  {{ value|last }}

When value is the list ``hello`` then the output will be ``o``.

**Note:** This function is not safe to use with multibyte character
values, use with care.

.. seealso:: :ref:`filter-first`
