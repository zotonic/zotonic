.. highlight:: django
.. include:: meta-twitter.rst

Format a plain text Tweet into HTML.

This filter creates hyperlinks out of the embedded URLs, @-references
and hashtags in a Tweet, like Twitter does.

For example::

  {{ "@acscherp"|twitter }}

Converts into::

  <a href="http://twitter.com/acscherp">@acscherp</a>

