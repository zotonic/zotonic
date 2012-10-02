.. highlight:: django
.. include:: meta-filesizeformat.rst

This filter formats a numeric value as KB, MB etc. This filter can be
used to display a number of bytes in a human readable format of kilo-
or megabytes.

For example::

  {{ 532671|filesizeformat }}

Will output ``520.2 KB``.

