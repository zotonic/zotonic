<h3 class="above-list">{_ Current menu _}</h3>
<ul class="short-list navigation-manager">

	{% for mid, path, action in m.rsc[id].menu|menu_flat %}
        {% with forloop.counter as c %}
        {% if mid %}
			<li id="{{ #before.c }}" class="line"> &nbsp; </li>
			{% droppable id=#before.c tag=[id, "before", path] delegate="resource_menu_admin_menu" %}

            <li class="header" id="{{ #menu.c }}">
				<a href="#" class="clearfix">
					<span class="grippy"><img src="/lib/images/grippy.png" alt="{_ Drag me _}" /></span>
					<span>{{ m.rsc[mid].title }}</span>
					{% button text="x" style="float:right" action={postback postback={delete path=path menu_id=id} delegate="resource_menu_admin_menu"} %}
				</a>
            {% if action == "down" %}
				<ul>
            {% else %}
                </li>
  			    {% droppable id=#menu.c tag=[id, "on", path] delegate="resource_menu_admin_menu" %}
            {% endif %}
			{% draggable id=#menu.c tag=path  %}
        {% else %}
        </ul></li>
        {% endif %}
        {% endwith %}
    {% empty %}
    <li id="menu-first">
		Drop a page here to start the menu.
	</li>
    {% droppable id="menu-first" tag=[id, "first"] delegate="resource_menu_admin_menu" %}

    {% endfor %}
</ul>
