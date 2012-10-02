.. highlight:: django
.. include:: meta-after.rst

Return the first element after another element in a list. For example::

  {{ [1,2,3]|after:2 }}

Evaluates to the value ``2``.

When the element is not part of the list, or is the last element in
the list, the returned value is ``undefined``.
