
.. include:: meta-menu_is_visible.rst

Filters a list of menu items on visibility. The :ref:`filter-is_visible` filter can’t be used due to the 
structure of a menu item list.

Example:

.. code-block:: django

	{% with m.rsc.main_menu.menu|menu_is_visible as menu 
		{% if menu %}
		<ul>
			{% for id,subids in menu %}
				<li>{{ id.title }}</li>
			{% endfor %}
		</ul>
		{% endif %}
	{% endwith %}
