.. highlight:: django
.. include:: meta-to_json.rst

Display any value as in JSON (JavaScript Object Notation).

For example::

  {{ [1,2,3]|to_json }}

Converts this list to a valid JSON UTF-8 encoded string which can be
directly used in JavaScript calls.

Another example for a JSON object::

  {{ %{ a: 1, b: 2 }|to_json }}
