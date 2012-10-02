.. highlight:: django
.. include:: meta-chunk.rst

This filter splits a list in shorter lists. It splits an array in
sub-arrays of at most a given length. This is useful when displaying a
list of items in columns or rows.

For example::

  {% for s in [1,2,3,4,5]|chunk:2 %}
      {% for n in s %}{{ n|format_number }} {% endfor %} * 
  {% endfor %}

This displays ``1 2 * 3 4 * 5 *``, as the array is split in three
chunks.  The last chunk is not filled to the maximum length.  Then
number of chunks depends on the length of the input list, this in
contrary to the split_in filters where the number of splits is fixed
and the length per split is variable.


.. seealso:: :ref:`filter-split_in`, :ref:`filter-vsplit_in`
