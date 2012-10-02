.. highlight:: django
.. include:: meta-menu_trail.rst

Return a breadcrumb navigation trail for the given id.

This filter locates the filter value which represents the current page
in the main menu or in the menu saved in the resource of the given id.

For example::

  {% print id|menu_trail:55 %}

Could print the list ``[13, 33]`` if ids 13 and 33 are the parents of
the `id` argument in the menu resource 55.

If no argument is given, it takes menu from the resource with the name
``main_menu``.

.. seealso:: :ref:`filter-menu_subtree`, :ref:`filter-menu_flat`
