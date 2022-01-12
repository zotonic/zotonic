.. highlight:: django
.. include:: meta-menu_flat.rst

.. seealso:: :ref:`filter-menu_subtree`, :ref:`filter-menu_trail`

Flattens the rsc menu structure for use in a template loop.

Example::

  {% for item in m.rsc[id].menu|menu_flat %}
  ...
  {% endif %}
