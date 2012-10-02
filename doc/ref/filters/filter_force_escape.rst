.. highlight:: django
.. include:: meta-force_escape.rst

HTML escapes a text.

Applies HTML escaping to a string (see the :ref:`filter-escape` filter
for details). In contrary to the `escape` filter, the `force_escape`
filter is applied `immediately` and returns a new, escaped
string. This is useful in the rare cases where you need multiple
escaping or want to apply other filters to the escaped
results. Normally, you want to use the :ref:`filter-escape` filter.

For example::

  {{ value|force_escape }}

When the value is ``hel&lo`` then the output is ``hel&amp;lo``.

.. seealso:: :ref:`filter-escape`
