.. highlight:: django
.. include:: meta-escapexml.rst

Escape the value for insertion in xml output.

For example::

  {{ value|escapexml }}

When the value is ``<hel'lo>`` then the output is ``&#60;hel&#39;lo&#62;``.

