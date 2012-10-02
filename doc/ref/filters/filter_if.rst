.. highlight:: django
.. include:: meta-if.rst

Selects an argument depending on a condition.

For example::

  {{ value|if:"yes":"no" }}

This is a shortcut for using the :ref:`tag-if` tag. The same can be
expressed as follows::

  {% if value %}yes{% else %}no{% endif %}

.. seealso:: :ref:`tag-if`
