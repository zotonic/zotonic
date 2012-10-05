
.. include:: meta-mod_menu.rst

Create nested navigation menus for your site.
             
Activating the module in the admin enables a "menu" item in the admin
navigation under "content", which lets you define a simple menu. Every
item in the menu references a Zotonic page and can be looked up using
the autocompletion widget.

This menu can be rendered in the frontend with the :ref:`scomp-menu`
custom tag.

It will use the ``_menu.tpl`` template which is by default able to
render a Twitter Bootstrap compatible menu structure using nested
``<ul>``s.

To implement a different navigation menu, override the ``_menu.tpl``
in your project and create new markup.


Data model
----------

The module creates a new category named `menu`. This allows one to
create multiple menus in a single site. Its edit page in the admin
contains the hierarchical menu editor.

The menu resource that is accessible from the admin page (`Content` >
`Menu`) is the resource with the unique name ``main_menu``.



.. seealso:: The filters :ref:`filter-menu_flat`, :ref:`filter-menu_subtree` and :ref:`filter-menu_trail`.
