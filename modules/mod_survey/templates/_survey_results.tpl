{% if inline %}
	<h2>{_ Results _}</h2>
{% endif %}

{% if id.is_editable or id.survey_show_results %}
	{% for result, chart, question in m.survey.results[id] %}
	<div class="survey_result">
		{% if not result %}
			{% if not inline %}
				{% if question.type == 'subhead' %}{% include "_survey_question_subhead.tpl" %}{% endif %}
			{% endif %}
			{% if question.type == 'prompt' %}{% include "_survey_question_prompt.tpl" %}{% endif %}
			{% if question.type == 'textblock' %}{% include "_survey_question_textblock.tpl" %}{% endif %}
			{% if question.type == 'pagebreak' %}<hr/>{% endif %}
		{% else %}
			{% if chart.question %}
				<h4>{{ chart.question }}</h4>
			{% endif %}
			{% if chart.charts %}
				{% for c in chart.charts %}
					{% if c.question %}
						<h5>{{ c.question }}</h5>
					{% endif %}
					{% include "_survey_result_chart.tpl" chart=c %}
				{% endfor %}
			{% else %}
				{% include "_survey_result_chart.tpl" %}
			{% endif %}
		{% endif %}
	</div>
	{% endfor %}
{% endif %}
