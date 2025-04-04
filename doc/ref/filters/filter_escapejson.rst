.. highlight:: django
.. include:: meta-escapejson.rst

.. seealso:: :ref:`filter-escape`, :ref:`filter-escapejs`

Escapes the value for safe insertion into JSON strings.

For example::

  { "value": "{{ value | escapejson }}" }

When the value is ``he"llo`` then the output is ``he\"llo``.

Or::

  { "title": "{{ id.title | unescape  | escapejson }}" }

In Zotonic text properties of resources are automatically html escaped.
In order to transform them to correct JSON these values have to be unescaped
first.

Internally, this calls ``z_utils:json_escape/1`` to perform the
string escaping.
