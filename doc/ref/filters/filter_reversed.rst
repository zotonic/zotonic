.. highlight:: django
.. include:: meta-reversed.rst

Reverse a list.

For example::

  {{ value|reversed }}

When value is ["hello", "world"] then the output is “worldhello”.

The main use for this filter is to reverse lists of values or search
results. There is no support for multi-byte unicode characters, this
is only a problem when applying the filter directly to a string value.

.. versionadded:: 0.6

