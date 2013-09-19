
.. include:: meta-sort.rst
.. highlight:: django

The `sort` filter takes a list of items to sort. Items can be a ordinary list of terms, or a list of resources to be filtered based on their properties. Sort order and properties to sort on are given as arguments to the filter.

By default it sorts the list in `ascending` order, and resource lists are sorted on their `id` if no property is specified.

Example::

   {{ [4, 6, 2, 3, 5]|sort }}

Sorts the list of numbers in `ascending` order.

Example::

   {{ [4, 6, 2, 3, 5]|sort:'desc' }}

Sorts the list of numbers in `descending` order.

Example::

   {% for r in id.o.author|sort:['title', 'desc', 'modified'] %}
      do something with `r`...
   {% endfor %}

This will sort on `title` in `ascending` order first, then on `modified` in `descending` order.
Any number of properties may be added, each one can have it’s own sort order, or use the current one.

See :ref:`model-rsc` for a list of properties available to sort on. 

Sort order may be either `ascending` or `descending` (may be abbreviated as `asc`, `+`, `desc`, `-` or as string version of those).
