
.. index:: tag; now
.. _tag-now:

now
===

Show the current date and time.

Displays the current local date and time, formatted according to the given date format string.

Example::

   {% now "Y-m-d" %}

Did output “2008-12-10” on december 10, 2008.

There is also a variable called ``now``, which holds the current date::

  {{ now|date:"Y-m-d" }}

Is equivalent to using the ``{% now %}`` tag.
  
.. seealso:: the :ref:`filter-date` filter for the possible format characters.

