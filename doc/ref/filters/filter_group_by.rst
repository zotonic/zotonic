.. highlight:: django
.. include:: meta-group_by.rst

Groups items of a list by a property.

When the item is an integer then it is assumed to be the id of a
resource.  This is especially useful for grouping items in for-loops.

For example::

  {% for grp in value|group_by:"a" %} ... loop over grp ... {% endfor %}

When value is the three element list::

  [
   [{a,1}, {b,1}],
   [{a,1},{b,2}], 
   [{a,2},{b,3}]
  ] 

then the output of group_by "a" will be the two element list::

  [ 
   [[{a,1},{b,1}],[{a,1},{b,2}]], 
   [[{a,2},{b,3}]]
  ].

