.. highlight:: django
.. include:: meta-escape_link.rst

Convert any URLs in a plaintext into HTML links, with adding the
``rel="nofollow"`` attribute.

Example::

  {{ "http://foo.bar/"|escape_link }}

Outputs::

  <a href="http://foo.bar/" rel="nofollow">http://foo.bar/</a>

This filter is very useful when displaying user-generated plaintexts,
like comments.
