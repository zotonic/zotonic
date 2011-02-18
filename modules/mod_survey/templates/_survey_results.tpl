{% for result, chart, question in m.survey.results[id] %}
<div class="survey_result">
	{% if not result %}
		{% if question.type == 'subhead' %}{% include "_survey_question_subhead.tpl" %}{% endif %}
		{% if question.type == 'prompt' %}{% include "_survey_question_prompt.tpl" %}{% endif %}
		{% if question.type == 'texblock' %}{% include "_survey_question_textblock.tpl" %}{% endif %}
		{% if question.type == 'pagebreak' %}<hr/>{% endif %}
	{% else %}
		<div class="graph">
		{% if chart.type == "pie" %}
			<h4>{{ question.question }}</h4>
			{% chart_pie3d height=100 width=400 data=chart.data %}
			
			<div class="values">
				<table>
				{% for label,value in chart.values %}
					<tr><th>{{ label }}</th><td>{{ value }}</td></tr>
				{% endfor %}
				</table>
			</div>
		{% endif %}
		</div>
	{% endif %}
</div>
{% endfor %}
