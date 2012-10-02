.. highlight:: django
.. include:: meta-match.rst

Match a value with a regular expression.

Returns true if the value matches the regular expression. This is
handy for checking if a string starts or ends with a particular value.

Usage::

  {% if value|match:".*foo$" %}

Checks if the value ends with "foo".
