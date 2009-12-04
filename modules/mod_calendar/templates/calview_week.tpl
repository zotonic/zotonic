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
					<li style="z-index: {{ev.z_index|format_integer}}; top: {{ev.top_em|format_number}}em; height: {{ev.height_em|format_number}}em; width: 100%" class="cal-lev-{{ev.level|format_number}}of{{ev.max_level|format_number}}">
						<div class="programme-event-item">
							{{ m.rsc[ev.id].title }}
						</div>
					</li>
				{% endfor %}
			</ol>
		</li>
		{% endfor %}
	</ol>
		
</div>
