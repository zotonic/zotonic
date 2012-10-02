.. highlight:: django
.. include:: meta-escapejson.rst

Escapes the value for insertion in JSON output.

For example::

  {{ value|escapejson }}

When the value is ``he'llo`` then the output is ``he\x27llo``.

Internally, this calls ``z_utils:json_escape/1`` to perform the
escaping.

.. seealso:: :ref:`filter-escape`, :ref:`filter-escapejs`
