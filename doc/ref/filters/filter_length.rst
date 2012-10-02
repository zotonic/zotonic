.. highlight:: django
.. include:: meta-length.rst

Returns the length of the value.

The length of a list is the number of elements in the list, the length
of a binary is the number of bytes in the binary.

For example::

  {{ value|length }}

When value is the list "hello" then the output will be “5”.

**Note:** With multi-byte values this function does not return the
 number of characters, it returns the number of bytes.  This may
 change in a future release.

