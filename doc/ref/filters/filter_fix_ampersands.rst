.. highlight:: django
.. include:: meta-fix_ampersands.rst

Replaces ampersands in the value with “&amp;” entities.

For example::

  {{ value|fix_ampersands }}

When the value is ``hel&lo`` then the output is ``hel&amp;lo``.

.. seealso:: :ref:`filter-escape`
