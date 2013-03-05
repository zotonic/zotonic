.. highlight:: django
.. include:: meta-vsplit_in.rst

This filter splits a list in shorter lists. It splits an array in N
sub-arrays of more or less equal length. This is useful when
displaying a list of items in columns. 

Note that it splits the array in a different way than
:ref:`filter-split_in` does: The filter `split_in` takes alternating
elements from the array, where `vsplit_in` takes complete runs at a
time. See the example below.

For example::

  {% with [1,2,3,4,5,6]|vsplit_in:3 as a,b,c %}
      {% for n in a %}{{ n|format_number }} {% endfor %}
  {% endwith %}

This displays ``1 2``.  The variable b will be ``[3,4]`` and the variable c will be ``[5,6]``.

.. seealso:: :ref:`filter-chunk`, :ref:`filter-split_in`
