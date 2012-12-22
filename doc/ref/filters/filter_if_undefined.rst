.. highlight:: django

.. include:: meta-if_undefined.rst

Tests if a value is undefined, returning the given argument.

Whereas the `|default` filter also falls back to the default value
when a value is an empty string or `false`, this filter only falls to
its value when it is ``undefined``.

This can be used for setting values which default to true if they are never set.

For example::

  {% if value|if_undefined:`true` %}The value is true or undefined{% endif %}

When the value is ``undefined`` then the output “The value was
undefined”.

.. seealso:: :ref:`filter-is_defined`, :ref:`filter-is_undefined`
