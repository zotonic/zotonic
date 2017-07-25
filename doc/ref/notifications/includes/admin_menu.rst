.. include:: includes/meta-admin_menu.rst

Example
"""""""

By observing this notification, you can add your own menu items to the admin
menu. The ``admin_menu`` notification is a fold which build up the menu,
allowing each callback to add and remove menu items as they wish.

For example, this add a menu separator and an "edit homepage" button
to the "content" submenu::

    -include_lib("zotonic_core/include/zotonic.hrl").
    -include_lib("zotonic_mod_admin/include/admin_menu.hrl").

    observe_admin_menu(#admin_menu{}, Acc, _Context) ->
    [
        #menu_separator{parent=admin_content},
        #menu_item{
            id=admin_edit_homepage,
            parent=admin_content,
            label="Edit homepage",
            url={admin_edit_rsc, [{id, page_home}]}
        } |Acc
    ].

The default submenu names are `admin_content`, `admin_structure`,
`admin_modules`, `admin_auth` and `admin_system`, but you are free to
add your own submenus.
