<h3 class="above-list">Current menu</h3>
<ul class="short-list navigation-manager">
	<li id="top" class="header">
		Drop pages here or drop them on one of the menu items.
	</li>

	{% for m_id, sub in m.rsc[id].menu %}
		{% with forloop.counter as m_nr %}
			<li class="header">
				
				<a id="{{ #menu.m_nr }}" href="#" class="clearfix">
					<span class="grippy"><img src="/lib/images/grippy.png" alt="Drag me" /></span>
					<span>{{ m.rsc[m_id].title }}</span>
					{% button text="x" style="float:right" action={postback postback={delete item=[m_nr] id=id} delegate="resource_menu_admin_menu"} %}
				</a>
				<ul>
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
			<li id="{{ #after.m_nr }}" class="line">
				&nbsp;
			</li>

			{% droppable id=#menu.m_nr tag=[id, m_nr] delegate="resource_menu_admin_menu" %}
			{% draggable id=#menu.m_nr tag=[m_nr] clone axis="y" delegate="resource_menu_admin_menu" %}
			{% droppable id=#after.m_nr tag=[id, "after", m_nr] delegate="resource_menu_admin_menu" %}

		{% endwith %}
	{% endfor %}
</ul>

{% droppable id="top" tag=[id, "top"] delegate="resource_menu_admin_menu" %}
