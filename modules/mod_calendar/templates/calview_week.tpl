<div class="calender clearfix">

	<ol class="programme-day-titles clearfix">
		<li class="programme-empty-first-cell">&nbsp;</li>
		{% for day in week_dates %}
			<li><h3>{{ day|date:"l" }}</h3></li>
		{% endfor %}
	</ol>
	
	<ol class="programme-days">
		<li class="programme-hours-item">
			<ol class="programme-hours">
				{% for h in day_hours %}
					<li class="hour">{{ h|format_integer:2 }}:00</li>
					<li class="quarter quarter-30">:30</li>
				{% endfor %}
			</ol>

		</li>
		{% for day,time in week_dates %}
		<li>
			<ol class="programme-day">
				{% for ev in event_divs[day] %}
					{% catinclude "calview_item.tpl" ev.id ev=ev %}
				{% endfor %}
			</ol>
		</li>
		{% endfor %}
	</ol>
		
</div>
