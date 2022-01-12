.. highlight:: django
.. include:: meta-default.rst

.. seealso:: :ref:`filter-if`, :ref:`filter-is_defined`, :ref:`filter-is_undefined`, :ref:`filter-if_undefined`

Provide an alternative value in case a value has a falsy value (0, ``false``, ``undefined`` or empty string).

For example::

  {{ value|default:1 }}
