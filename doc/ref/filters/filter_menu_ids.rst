.. highlight:: django
.. include:: meta-menu_ids.rst

.. seealso:: :ref:`filter-menu_flat`, :ref:`filter-menu_is_visible`

Returns all resource ids in a menu. Could return invisible and non
existing resource ids. The returned ids are a flat list, the hierarchy
of the menu is lost.

Example::

  {% for mid in id|menu_ids %}
      {{ mid.title }}
  {% endif %}
