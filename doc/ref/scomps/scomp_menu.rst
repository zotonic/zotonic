.. include:: meta-menu.rst

Show a page menu.

This tag is part of the module mod_menu. The ``{% menu %}`` tag is used to generate the HTML for the menu defined in the admin.

You can define multiple menus in your site. By default there is one menu, called “main_menu”. If you want another one, create a page of type “page menu” (under “Categorization”) and start editing your menu. You can use the “menu_id” argument to select which menu you want to display.

Example::

   {% menu id=id %}

Generates something like::

   <ul id="navigation" class="nav">
     <li>
       <a href="/" class="welcome">Home</a>
     </li>
     <li>
       <a href="/features" class="page_features">Features</a>
     </li>
     <li>
       <a href="/documentation" class="documentation active">Documentation</a>
     </li>
     <li>
       <a href="/documentation/628/installation" class="page_install">Install</a>
     </li>
   </ul>

The menu has the following features:

* The menu is a unordered list.
* The id of the menu is ``navigation``.
* The class of the menu is always ``nav``.
* Menu items are a <li> with a single <a>
* The link of the menu item referring to the current page has the class ``active``.
* Every link also gets the unique name of the target as a class.
* Every menu item can have single level submenus.  A submenu has the same properties as the menu.

+---------------+----------------------------------------------+---------------+
|Argument       |Description                                   |Example        |
+===============+==============================================+===============+
|id             |Set this to the id of the current shown page  |               |
|               |and it wil highlight its page path.           |               |
+---------------+----------------------------------------------+---------------+
|menu_id        |The id of the menu that you want to           |               |
|               |display. If left empty, the main menu is      |               |
|               |shown.                                        |               |
+---------------+----------------------------------------------+---------------+
