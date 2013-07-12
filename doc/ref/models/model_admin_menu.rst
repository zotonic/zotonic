
.. include:: meta-admin_menu.rst


This model holds the admin menu, which is built up by calling each
module to add items to the menu.


Extending the admin menu
------------------------

By implementing the ``admin_menu`` :ref:`notification <manual-notification>` you can add your own
menu items to the admin menu. The ``admin_menu`` notification is a
fold which build up the menu, allowing each callback to add and remove
menu items as they wish.

.. highlight:: erlang
            
For example, this add a menu separator and an "edit homepage" button
to the "content" submenu::

  -include_lib("modules/mod_admin/include/admin_menu.hrl").

  observe_admin_menu(admin_menu, Acc, _Context) ->
    [
     #menu_separator{
       parent=admin_content},
     #menu_item{
        id=admin_edit_homepage,
        parent=admin_content,
        label="Edit homepage",
        url={admin_edit_rsc, [{id, page_home}]}}
     |Acc].

The default submenu names are `admin_content`, `admin_structure`,
`admin_modules`, `admin_auth` and `admin_system`, but you are free to
add your own submenus.

