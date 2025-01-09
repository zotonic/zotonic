.. highlight:: django
.. include:: meta-from_json.rst

Parse a string as a JSON (JavaScript Object Notation) value.
The returned value can be processed futher.

For example::

  {{ "[1,2,3]"|from_json|first }}

Converts this JSON string to a list of integers and displays the first result::

  1

Another example with a string that parses to a structured value::

  {% with '{ "a": "Hello", "b": 2 }'|from_json as obj %}
      {% print obj.a %}
  {% endwith %}

This will display::

  Hello

If the given value can not be parsed as JSON then it is silently mapped to ``undefined``.
