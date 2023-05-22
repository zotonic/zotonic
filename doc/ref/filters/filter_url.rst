.. highlight:: django
.. include:: meta-url.rst
.. seealso:: :ref:`filter-url_abs`, :ref:`tag-url`, :ref:`filter-urlencode`

Generates the relative URL for the given dispatch information.

An *relative URL* is an URL that excludes the protcol and hostname.
For example ``/foo/bar``.

For example, generate a url for the dispatch rule ``home`` with an
extra argument ``hello``::

    {{ {home hello="world"}|url }}

This is similar to::

    {% url home hello="world" %}

Difference between the tag and the filter is that the filter can
be used in expressions or with passed values.

