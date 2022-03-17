.. highlight:: django
.. include:: meta-is_undefined.rst

.. seealso:: :ref:`filter-is_defined`, :ref:`filter-if_undefined`, :ref:`filter-if`

Tests if a value is undefined.

Checks if the value is empty and outputs a boolean true or false.
This is useful in combination with the :ref:`tag-if` tag.

For example::

  {% if value[1]|is_undefined %}The first elemeent of value was undefined{% endif %}

If the value is ``[]`` then the output is ``The first elemeent of value was undefined``.
