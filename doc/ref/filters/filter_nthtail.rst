.. highlight:: django
.. include:: meta-nthtail.rst

Fetch the nth tail of a list.

Useful when you want to skip the first N elements of a list when looping.

For example::

  {% for a in value|nthtail:2 %}{{ a|format_number }}{% endfor %}

When value is the list ``[1,2,3]`` then the output is ``3``.

.. seealso:: :ref:`filter-first`, :ref:`filter-tail`
