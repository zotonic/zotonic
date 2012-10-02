.. highlight:: django
.. include:: meta-stringify.rst

Translates atoms, integers and floats to strings. The undefined value
is translated to the empty string.  Does not translate tuples.

For example::

  {{ value|stringify }}

When `value` is undefined then the output will be “”.

.. seealso:: :ref:`filter-slugify`, :ref:`filter-to_binary`

