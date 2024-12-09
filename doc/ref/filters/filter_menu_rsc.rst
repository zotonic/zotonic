.. highlight:: django
.. include:: meta-menu_rsc.rst

Return the menu to be displayed with a resource.

Checks the results of the :ref:`menu_rsc` notification. If that returns `undefined` then
the predicate ``hasmenu`` is used to find menus attached to the resource. The first
in the list is returned by this filter.

If no menu is found then the menu resource id with name ``main_menu`` is returned.

Example::

    {% if id|menu_rsc as menu_id %}
        .. display the menu with id menu_id ..
    {% endif %}
