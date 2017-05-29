.. highlight:: django
.. include:: meta-slice.rst

Perform array-slice operations on a list or string.

If the argument is a binary then it is handles as an UTF-8 encoded string.

Given a list ``[1,2,3,4,5,6,7,8,9,0]``

Get all elements from element M to element N::

  {{ list|slice:[3,7] }} -> [3,4,5,6,7]
  {{ list|slice:[3,-3] }} -> [3,4,5,6,7]
  {{ list|slice:[-7,-3] }} -> [4,5,6,7]
  {{ list|slice:[-7,7] }} -> [4,5,6,7]

Get all elements except the first N::

  {{ list|slice:[3,] }} -> [3,4,5,6,7,8,9,0]
  {{ list|slice:[-7,] }} -> [4,5,6,7,8,9,0]

Get all elements up to element N::

  {{ list|slice:[,3] }} -> [1,2,3]
  {{ list|slice:[3] }} -> [1,2,3]

Get all elements except the last N::

  {{ list|slice:[,-3] }} -> [1,2,3,4,5,6,7]
  {{ list|slice:[-3] }} -> [1,2,3,4,5,6,7]

If the slice start is after the slice end then ``[]`` is returned::

  {{ list|slice:[2,1] }} -> []

An empty slice returns the whole input::

  {{ list|slice:[,] }} -> [1,2,3,4,5,6,7,8,9,0]

