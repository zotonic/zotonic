
.. include:: meta-menu_is_visible.rst

Filters a list of menu items on visibility and existance. Only top-level menu items that are both visible and exist are kept in the list. Note that sub-menus are not filtered, they need
to be filtered separately.

The :ref:`filter-is_visible` filter can’t be used due to the structure of a menu item list.

Example:

.. code-block:: django

	{% with m.rsc.main_menu.menu|menu_is_visible as menu %}
		{% if menu %}
		<ul>
			{% for item in menu %}
				<li>{{ item.id.title }}</li>
			{% endfor %}
		</ul>
		{% endif %}
	{% endwith %}
