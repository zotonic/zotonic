.. highlight:: django
.. include:: meta-escape_ical.rst

Escape the value according to the RFC2445 rules.

A double quote becomes ``\"``; a comma becomes ``\,``; a colon
becomes ``":"``; a semicolon becomes ``\;``; a backslash becomes
``\\`` and a newline becomes ``\n``.

It is also ensures that any single line is maximum 70 characters long
by splitting the lines with newline/space combinations.

For example::

  {{ value|escape_ical }}

When the value is ``abc:d;e`` then the output is ``abc":"d\;e``.

.. seealso:: :ref:`filter-escape`
