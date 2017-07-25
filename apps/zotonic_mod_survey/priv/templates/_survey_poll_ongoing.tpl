{% for id in m.search[{query cat="poll" ongoing}]%}
	<div class="survey-poll" id="{{ #poll.id }}">
		{% if m.survey.did_survey[id] %}
			{% include "_survey_results.tpl" id=id inline %}
		{% else %}
			{% poll id=id element_id=#poll.id %}
		{% endif %}
	</div>
{% endfor %}
