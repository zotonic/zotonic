
.. include:: meta-mod_admin.rst

.. todo:: Finish documentation

Extending the admin menu
------------------------

See :ref:`model-admin_menu` on how to extend the admin menu.


Extending the admin edit page
-----------------------------

There are several special templates names that will be automatically
included into the `/admin/edit/xxx` page from when you create these
specially named templates.
        
``_admin_edit_basics.tpl``
  Will be automatically included into main (left) div (at top).

``_admin_edit_content.tpl``
  Will be automatically included into main (left) div (at bottom).

``_admin_edit_sidebar.tpl``
  Will be automatically included into right sidebar (near middle/bottom).

These templates are included using the :ref:`tag-all-catinclude` tag; so
if you need something in the sidebar just for persons, create a
``_admin_edit_sidebar.person.tpl`` file in your project.
  

Writing admin widget templates
------------------------------

This section contains examples of templates to create widgets for the
/admin. Each of these examples extends basic several widget templates
from mod_admin. To write your own you need to drop example content and
fill holes in these example widgets.

You can use them as basis for your's site admin-related tasks.

``_admin_dashboard_example.tpl``
  Very simple example widget for admin dashboard. Contains blocks for title and body.
  Look at /admin to see several dashboard widgets (latest events, pages, media, etc).

``_admin_widget_std.tpl``
  Sligthly more complex widget example. Same templates are used into /admin/edit/N for
  main content and sidebar widgets. These widgets do not provide any localization
  abilities. Also note that there are several special widget names:
        
``_admin_widget_i18n.tpl``
  Complex widget example. Is used to edit localized rsc properties. It will be rendered
  as tabs. See /admin/edit/N top left to see the tabs. If mod_translation disabled, then
  i18n-widgets are displayed same as _admin_widget_std.tpl.
      

Making an admin widget conditionally visible
--------------------------------------------

.. highlight:: django
               
To make an entire admin widget visible or not, depending on some
condition that you want to calculate inside the widget's code, you can
use the `widget_wrapper` block (which sits around the entire widget)
in combination with the :ref:`tag-inherit` tag, wrapping that with a
condition.

For instance, :ref:`mod_backup` uses this technique to display the
import/export sidebar widget. Excerpt from mod_backup's `_admin_edit_sidebar.tpl`::

    {# Make the widget conditional, based on the config value mod_backup.admin_panel #}
    {% block widget_wrapper %}
        {% if m.config.mod_backup.admin_panel.value %}
            {% inherit %}
        {% endif %}
    {% endblock %}


.. seealso:: :ref:`template-admin_edit_widget_i18n`
