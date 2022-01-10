
.. include:: meta-menu_expand.rst

Takes a menu, or a menu resource id, and add alls ``haspart`` edges eminating from the menu ids.

With this filter it is possible to add a collection to the menu and have the pages in the collection
automatically added to the menu.

Example:

.. code-block:: django

    {% with m.rsc.main_menu.menu|menu_expand as menu %}
        {% if menu %}
            {% with id|menu_trail:menu as trail %}
            <ul>
                {% for item in menu %}
                    <li {% if item.id|member:trail %}class="active"{% endif %}>
                        <a href="{{ item.id.page_url }}">{{ item.id.title }}</a>
                    </li>
                {% endfor %}
            </ul>
            {% endwith %}
        {% endif %}
    {% endwith %}
