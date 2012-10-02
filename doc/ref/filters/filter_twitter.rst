.. highlight:: django
.. include:: meta-twitter.rst

Format a plain text tweet into HTML.

This filter creates hyperlinks out of the embedded URLs, @-referencfes
and hashtags in a tweet, like Twitter does.

For example::

  {{ "@acscherp"|twitter }}

Converts into::

  <a href="http://twitter.com/acscherp">@acscherp</a>

