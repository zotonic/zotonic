.. highlight:: django
.. include:: meta-format_duration.rst

.. seealso:: :ref:`filter-format_number`, :ref:`filter-format_integer`, :ref:`filter-format_price`

Show a duration in hours, minutes and seconds.

Takes as input a number representing a number of seconds.
Outputs a human readable form.

For example::

  {{ 123|format_duration }}

Will ouput ``2m3s``.

And::

  {{ 3601|format_duration }}

Will ouput ``1h0m1s``.

