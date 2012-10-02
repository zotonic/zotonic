.. highlight:: django
.. include:: meta-ne_day.rst

Tests if two dates are not equal.

Tests if the value is a date and not equal to the argument.  The value
and the argument must be a tuple of the format ``{Y,M,D}`` or
``{{Y,M,D},{H,I,S}}``.

For example::

  {% if value|ne_day:othervalue %}different days{% endif %}

This outputs “different days” if value and othervalue are dates and different.

This is useful in combination with for example the if tag.

.. seealso:: :ref:`filter-eq_day`
