{% if allday or period|eq:"month" %}
<li style="width: 100%" class="cal-lev-{{ev.level|format_number}}of{{ev.max_level|format_number}}{% block calview_item_li_class %}{% endblock %}">
{% else %}
<li style="z-index: {{ev.z_index|format_integer}}; top: {{ev.top_em|format_number}}em; height: {{ev.height_em|format_number}}em; width: 100%" class="cal-lev-{{ev.level|format_number}}of{{ev.max_level|format_number}}{% block calview_item_li_class %}{% endblock %}">
{% endif %}
	<div class="programme-event-item{% block calview_item_div_class %}{% endblock %}">
		{% block calview_item_content %}
		<p>{{ m.rsc[id].title }}</p>
		<p class="time">{{ m.rsc[id].date_start|date:"H:i" }}&mdash;{{ m.rsc[id].date_end|date:"H:i" }}</span></p>
		{% endblock %}
	</div>
</li>
