
.. include:: meta-without.rst

Remove the items given in the argument from the filter value.

For example::

  {% print [1,2,3]|without:[2] %}

prints::

  [1,3]

This filter also works on list-like values like resource edges::

  {% for id in id.o.tags|without:some_id.o.tags %}

Iterates of all `tags` edges of the given id, for each id that is not
also an edge of `some_id`.
