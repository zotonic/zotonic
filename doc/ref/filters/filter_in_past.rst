.. highlight:: django
.. include:: meta-in_past.rst

Tests if a date is in the past.

Tests if the value is a date and in the past.  The value must be a
tuple of the format ``{Y,M,D}`` or ``{{Y,M,D},{H,I,S}}``. When the value is
not a date or datetime, the result is ``undefined``.

For example::

  {% if value|in_past %}Those days have gone.{% endif %}

This outputs “Those days have gone.” if the value is a date and in the past.

.. seealso:: :ref:`filter-in_future`
