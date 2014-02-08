{% if inline %}
	<h2>{_ Results _}</h2>
{% endif %}

{% if m.survey.is_allowed_results_download[id] %}
	<p><a id="{{ #download }}" href="{% url survey_results_download id=id %}">{_ Click here to download the results as a CSV file. _}</a></p>
	{% wire id=#download propagate 
			action={alert text=_"Download will start in the background. Please check your download window."}
	%}
{% endif %}

{% if id.survey_show_results or m.survey.is_allowed_results_download[id] %}
	{% for result, chart, question in m.survey.results[id] %}
	<div class="survey_result">
		{% if not result %}
{#
    			{% include ["blocks/_block_view_",question.type,".tpl"]|join id=id blk=question answers=answers %}
#}
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
{% else %}
	<p>{_ You are not allowed to see the results of this survey. _}</p>
{% endif %}
