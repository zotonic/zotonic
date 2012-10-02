.. highlight:: django
.. include:: meta-member.rst

Finds a value in a list.

Checks if the value is part of the argument. The argument must be a
list. Returns a boolean value.

For example::

  {% if value|member:[1,2,3] %}
      One of the first three
  {% endif %}

When value is the integer 2 then the output is “One of the first three”.
