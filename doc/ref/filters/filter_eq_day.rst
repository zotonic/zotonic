.. highlight:: django
.. include:: meta-eq_day.rst

Tests if the value is a date and equal to the argument.  The value and
the argument must be a tuple of the format ``{Y,M,D}`` or
``{{Y,M,D},{H,I,S}}``.

For example::

  {% if value|eq_day:othervalue %}same day{% endif %}

This outputs “same day” if value and othervalue are dates and on the same day.

This is useful for conditions, in combination with for example the `if` tag.

.. seealso:: :ref:`filter-ne_day`

