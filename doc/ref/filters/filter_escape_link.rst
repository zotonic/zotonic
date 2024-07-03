.. highlight:: django
.. include:: meta-escape_link.rst

.. seealso:: :ref:`filter-urlize`

Convert any URLs in a plaintext into HTML links, with adding the
``rel="nofollow"`` attribute, and replaces all newlines with ``<br>`` tags.

Example::

  {{ "Hello http://foo.bar/\n\nAnd bye."|escape_link }}

Outputs::

  "Hello <a href="http://foo.bar/" rel="noopener nofollow noreferrer">http://foo.bar/</a><br /><br />And bye."

This filter is very useful when displaying user-generated plaintexts, like comments.
