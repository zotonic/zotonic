.. highlight:: django
.. include:: meta-url_abs.rst
.. seealso:: :ref:`filter-url`, :ref:`tag-url`, :ref:`filter-urlencode`

Generates an absolute URL for the given dispatch information.

An *absolute URL* is an URL that includes the protcol and hostname.
For example ``https://example.com/foo/bar``.

For example, generate a url for the dispatch rule ``home`` with an
extra argument ``hello``::

    {{ {home hello="world"}|url_abs }}

This is similar to::

    {% url_abs home hello="world" %}

Difference between the tag and the filter is that the filter can
be used in expressions or with passed values.
