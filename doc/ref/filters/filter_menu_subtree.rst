.. highlight:: django
.. include:: meta-menu_subtree.rst

Get the subtree of an id in a menu (if any).

Returns the subtree of the filter value. Useful for showing a part of
a menu when browsing sub-pages.

If the given id is not found inside the menu, it returns
``undefined``.

If no argument is given, it takes menu from the resource with the name
``main_menu``.

.. seealso:: :ref:`filter-menu_trail`, :ref:`filter-menu_flat`
