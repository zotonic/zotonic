<div class="calendar clearfix">

	<ol class="programme-day-titles clearfix">
		<li class="programme-empty-first-cell">&nbsp;</li>
		{% for day in week_dates %}
			<li><h3>{{ day|date:"l" }}</h3></li>
		{% endfor %}
	</ol>

	{% for week in month_dates|chunk:7 %}
		<ol class="programme-week clearfix">
			<li class="programme-empty-first-cell">&nbsp;</li>

			{% for day,time in week %}
			<li class="programme-day">
				{% with event_divs[day]|length|default:0 as ev_len %}
				{% with whole_day[day]|length|default:0 as wd_len %}
				<ol class="{% if ev_len|add:wd_len|gt:5 %}long-list{% endif %}">
				<li class="programme-day-date">
					{% if day[3]|eq:1 or month_dates[1][1]|eq:day %}
						{{ day|date:"F d" }}
					{% else %}
						{{ day|date:"d" }}
					{% endif %}
				</li>
				{% for ev in whole_day[day] %}
					{% catinclude "_calview_item.tpl" ev.id ev=ev %}
				{% endfor %}
				{% for ev in event_divs[day] %}
					{% catinclude "_calview_item.tpl" ev.id ev=ev %}
				{% endfor %}
				</ol>
				{% endwith %}
				{% endwith %}
			</li>
			{% endfor %}
		</ol>
	{% endfor %}
</div>
