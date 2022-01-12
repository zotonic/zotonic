.. highlight:: django
.. include:: meta-fix_ampersands.rst

.. seealso:: :ref:`filter-escape`

Replaces ampersands in the value with “&amp;” entities.

For example::

  {{ value|fix_ampersands }}

If the value is ``hel&lo`` then the output is ``hel&amp;lo``.
