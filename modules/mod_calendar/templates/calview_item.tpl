<li style="z-index: {{ev.z_index|format_integer}}; top: {{ev.top_em|format_number}}em; height: {{ev.height_em|format_number}}em; width: 100%" class="cal-lev-{{ev.level|format_number}}of{{ev.max_level|format_number}}{% block calview_item_li_class %}{% endblock %}">
	<div class="programme-event-item{% block calview_item_div_class %}{% endblock %}">
		{% block calview_item_content %}
		<p>
			<a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a>
			<span class="time">{{ m.rsc[id].date_start|date:"H:i" }}</span>
		</p>
		{% endblock %}
	</div>
</li>
