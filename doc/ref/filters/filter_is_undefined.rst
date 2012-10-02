.. highlight:: django
.. include:: meta-is_undefined.rst

Tests if a value is undefined.

Checks if the value is empty and outputs a boolean true or false.
This is useful in combination with the :ref:`tag-if` tag.

For example::

  {% if value|is_undefined %}The value was undefined{% endif %}

When the value is “” then the output “The value was undefined”.

.. seealso:: :ref:`filter-is_defined`
