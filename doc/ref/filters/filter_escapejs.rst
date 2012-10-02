.. highlight:: django
.. include:: meta-escapejs.rst

Escapes the value for insertion in Javascript output.

For example::

  {{ value|escapejs }}

When the value is ``he'llo`` then the output is ``he\x27llo``.

Internally, this calls ``z_utils:js_escape/1`` to perform the
escaping.

Note: when generating JSON output, be sure to use
:ref:`filter-escapejson`, as JSON escaping is subtly different from JS
escaping.

.. seealso:: :ref:`filter-escape`, :ref:`filter-escapejson`
