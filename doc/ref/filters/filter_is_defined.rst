.. highlight:: django
.. include:: meta-is_defined.rst

Tests if a value is defined.

Checks if the value is not empty and outputs a boolean true or false.
This is useful in combination with the :ref:`tag-if` tag.

For example::

  {% if value|is_defined %}The value was defined{% endif %}

When the value is “foo” then the output “The value was defined”.

.. seealso:: :ref:`filter-is_undefined`
