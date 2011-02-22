<h3 class="above-list">Current menu</h3>
<ul class="short-list navigation-manager">

	{% for mid, path, action in m.rsc[id].menu|menu_flat %}
        {% with forloop.counter as c %}
        {% if mid %}
			<li id="{{ #before.c }}" class="line"> &nbsp; </li>
			{% droppable id=#before.c tag=[id, "before", path] delegate="resource_menu_admin_menu" %}

            <li class="header" id="{{ #menu.c }}">
				<a href="#" class="clearfix">
					<span class="grippy"><img src="/lib/images/grippy.png" alt="Drag me" /></span>
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


{#

					{% for s_id in sub %}
						{% with forloop.counter as s_nr %}
							{% with m_nr|append:"-"|append:s_nr as m_s_nr %}

								<li id="{{ #sub.m_s_nr }}">
									<a id="{{ #menu.m_s_nr }}" href="#" class="clearfix">
										<span class="grippy"><img src="/lib/images/grippy.png" alt="Drag me" /></span>
										<span>{{ m.rsc[s_id].short_title|default:m.rsc[s_id].title }}</span>
										{% button text="x" style="float:right" action={postback postback={delete item=[m_nr, s_nr] id=id} delegate="resource_menu_admin_menu"} %}
									</a>
								</li>

								{% droppable id=#sub.m_s_nr tag=[id, m_nr, s_nr] delegate="resource_menu_admin_menu" %}
								{% draggable id=#sub.m_s_nr tag=[m_nr, s_nr] delegate="resource_menu_admin_menu" %}

							{% endwith %}
						{% endwith %}
					{% endfor %}
				</ul>
			</li>
			<li id="{{ #after.m_nr }}" class="line">&nbsp;</li>

			{% droppable id=#menu.m_nr tag=[id, m_nr] delegate="resource_menu_admin_menu" %}
			{% draggable id=#menu.m_nr tag=[m_nr] clone axis="y" delegate="resource_menu_admin_menu" %}
			{% droppable id=#after.m_nr tag=[id, "after", m_nr] delegate="resource_menu_admin_menu" %}

		{% endwith %}
	{% endfor %}
#}
