.. highlight:: django
.. include:: meta-tail.rst

Fetch the tail of a list.

Returns the tail of a list.  Useful when you want to skip the first
element of a list when looping.

For example::

  {% for a in value|tail %}{{ a|format_number }}{% endfor %}

When value is the list ``[1,2,3]`` then the output is ``23``.

.. seealso:: :ref:`filter-first`, :ref:`filter-nthtail`
