.. highlight:: django
.. include:: meta-urldecode.rst
.. seealso:: :ref:`filter-urlencode`

Decode a text where characters are encoded as URL-safe characters.

Translates all percent encoded characters back to their original encoding.

For example::

  {{ value|urldecode }}

When value is “msg%3DHello%26World” then the output is “msg=Hello&World”.

