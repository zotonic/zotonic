.. highlight:: django
.. include:: meta-escapejson.rst

.. seealso:: :ref:`filter-escape`, :ref:`filter-escapejs`

Escapes the value for insertion in JSON output.

For example::

  {{ value|escapejson }}

When the value is ``he"llo`` then the output is ``he\"llo``.

Internally, this calls ``z_utils:json_escape/1`` to perform the
escaping.
