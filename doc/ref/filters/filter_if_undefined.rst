.. highlight:: django

.. include:: meta-if_undefined.rst

Tests whether a value is undefined, returning the given argument.

Whereas the `|default` filter also falls back to the default value
when a value is an empty string or ``false``, this filter `only` falls
back to its value when the input value is the Erlang ``undefined``
atom.

This can be used for setting values which default to true if they are never set.

For example::

  {% if value|if_undefined:`true` %}The value is true or undefined{% endif %}

If the value is ``undefined``, the output will be “The value is true or undefined”.

.. seealso:: :ref:`filter-is_defined`, :ref:`filter-is_undefined`, :ref:`filter-if`
