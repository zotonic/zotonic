.. highlight:: django
.. include:: meta-make_list.rst

Forces the value to a list.

This is especially useful for loops using the ``{% for %}`` tag.

For example::

  {{ value|make_list }}

When value is the tuple ``{"a","b"}`` then the output is the list ``["a","b"]``.

