.. highlight:: django
.. include:: meta-range.rst

Generate a list of integers, with an optional step.

For example::

  {{ 1|range:4 }}

Generates the list ``[1,2,3,4]``.

The second filter argument is the step size::

  {{ 1|range:10:2 }}

Generates the list ``[1,3,5,7,9]``.



