.. highlight:: django
.. include:: meta-urlencode.rst
.. seealso:: :ref:`filter-sanitize_url`, :ref:`filter-is_site_url`, :ref:`filter-url_abs`, :ref:`filter-url`, :ref:`filter-urlencode`

Make a text safe for URLs.

Translates all url unsafe characters in the value to their percent encoding.

For example::

  {{ value|urlencode }}

When value is “msg=Hello&World” then the output is “msg%3DHello%26World”.

