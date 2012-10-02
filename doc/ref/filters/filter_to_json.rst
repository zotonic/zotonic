.. highlight:: django
.. include:: meta-to_json.rst

Display any value as in JSON (JavaScript Object Notation).

For example::

  {{ [1,2,3]|to_json }}

Converts this list to a valid JSON UTF-8 encoded string which can be
directly used in JavaScript calls.

The input value may be in a different encoding, given as argument to
the `to_json` filter::

  {{ value|to_json:"latin-1" }}

Currently, there are two encodings supported:

- UTF-8 (``utf-8``) i.e. no encoding conversion, which is the default when no argument is given.
- ISO 8859-1 (``latin-1``).


