.. highlight:: django
.. include:: meta-replace.rst

Regular expression replacement of a pattern with a string.

Replaces the sub strings matching a regular expression with a new
string value.

For example::

  {{ "abcba"|replace:["b","x"] }}

The output will be the string "axcxa".

When you don not specify the replacement value, the replacement value
is assumed to be the empty string::

  {{ "abcba"|replace:"b" }}

The output will be the string "aca".

**Note:** This filter is very inefficient, as it will compile and
match the regular expression while serving the template.  Try to do
your replacements when you `save` your content, and not when you
serve the content.

