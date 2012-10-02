.. highlight:: django
.. include:: meta-in_future.rst

Tests if a date is in the future.

Tests if the value is a date and in the future.  The value must be a
tuple of the format ``{Y,M,D}`` or ``{{Y,M,D},{H,I,S}}``. When the
value is not a date, or datetime, the result will be ``undefined``.

For example::

  {% if value|in_future %}That day has yet to come.{% endif %}

This outputs “That day has yet to come.” if the value is a date and in the future.

.. seealso:: :ref:`filter-in_past`

