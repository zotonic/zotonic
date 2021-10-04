.. highlight:: django
.. include:: meta-twitter.rst

Format a plain text Tweet into HTML.

This filter creates hyperlinks out of the embedded URLs, ``@username``-references
and ``#tag``-hashtags in a Tweet, like Twitter does.

For example::

  {{ "@acscherp"|twitter }}

Converts into::

  <a href="https://twitter.com/acscherp">@acscherp</a>

