
.. include:: meta-menu.rst

Show a page menu.

This tag is part of the module mod_menu. The ``{% menu %}`` tag is used to generate the HTML for the menu defined in the admin.

You can define multiple menu's in your site. By default there is one menu, called “main_menu”. If you want another one, create a page of type “page menu” (under “Categorization”) and start editing your menu. You can use the “menu_id” argument to select which menu you want to display.

Example::

   {% menu id=id %}

Generates something like::

   <ul id="navigation" class="clearfix at-menu do_superfish">
     <li id="nav-item-1" class=" first ">
       <a href="/" class="welcome">Home</a>
     </li>
     <li id="nav-item-2" class="">
       <a href="/features" class="page_features">Features</a>
     </li>
     <li id="nav-item-3" class="">
       <a href="/documentation" class=" current documentation">Documentation</a>
     </li>
     <li id="nav-item-4" class=" last ">
       <a href="/documentation/628/installation" class="page_install">Install</a>
     </li>
   </ul>

The menu has the following features:

* The menu is a unordered list.
* The id of the menu is "navigation".
* The class of the menu is always "clearfix at-menu do_superfish"
* Menu items are a <li> with a single <a>
* The first and last <li> are given the respective classes "first" and "last".
* The link of the menu item referring to the current page has the class "current"
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
