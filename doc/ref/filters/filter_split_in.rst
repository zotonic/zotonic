.. highlight:: django
.. include:: meta-split_in.rst

This filter split a list in shorter lists. It splits an array in N
sub-arrays of more or less equal length. This is useful when
displaying a list of items in columns.

For example::

  {% for a,b,c in [1,2,3,4,5,6]|split_in:3 %}
      {% for n in a %}{{ n|format_number }} {% endfor %}
  {% endfor %}

This displays ``1 4``.  The variable b will be ``[2,5]`` and the variable c will be ``[3,6]``.

.. seealso:: :ref:`filter-chunk`, :ref:`filter-vsplit_in`
