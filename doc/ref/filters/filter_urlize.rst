.. highlight:: django
.. include:: meta-urlize.rst

Find urls in the given input and make them clickable.

Example::

  {{ "http://foo.bar/"|escape_link }}

Outputs::

  <a href="http://foo.bar/">http://foo.bar/</a>

This filter is very similar to the :ref:`filter-escape_link` filter.

.. seealso:: :ref:`filter-escape_link`
