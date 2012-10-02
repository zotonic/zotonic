.. highlight:: django
.. include:: meta-before.rst

Return the first element before another element in a list. For example::

  {{ [1,2,3]|before:2 }}

Evaluates to the value ``1``.

When the element is not part of the list, or is the first element in
the list, the returned value is ``undefined``.
