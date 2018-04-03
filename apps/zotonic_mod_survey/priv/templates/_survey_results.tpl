{% if inline %}
	{% include "_survey_end.tpl" %}

	<h2>{_ Results _}</h2>
{% endif %}

{% if id.survey_show_results == 2 and not is_aggregated %}
    {% with m.survey.did_survey_results[id] as result %}
        {% if id.survey_test_percentage %}
            {% with id|survey_test_max_points as max_points %}
                {% if max_points %}
                    <h2>
                        {{ (result.points / max_points * 100)|round }}% &ndash;
                        {% if result.points >= max_points * (id.survey_test_percentage / 100) %}
                            {_ Passed _}
                        {% else %}
                            {_ Failed _}
                        {% endif %}
                    </h2>

                    <table class="table" style="width: auto">
                        <tr>
                            <td>{_ Points _}</td>
                            <th style="text-align: right">{{ result.points }} / {{ max_points }}</th>
                        </tr>
                        <tr>
                            <td>{_ Needed for pass _}</td>
                            <th style="text-align: right">{{ id.survey_test_percentage }}%</th>
                        </tr>
                        <tr>
                            <td>{_ Your result _}</td>
                            <th style="text-align: right">{{ (result.points / max_points * 100)|round }}%</th>
                        </tr>
                    </table>
                {% endif %}
            {% endwith %}
        {% endif %}

        {% for blk in id.blocks %}
            {% if blk.is_hide_result %}
                {# nothing #}
            {% elseif blk.name != 'survey_feedback' %}
                {% optional include "blocks/_block_view_"++blk.type++".tpl" blk=blk is_survey_answer_view result=result %}
            {% endif %}
        {% endfor %}
    {% endwith %}
{% elseif id.survey_show_results or m.survey.is_allowed_results_download[id] %}
	{% for result, chart, question in m.survey.results[id] %}
	<div class="survey_result">
        {% if question.is_hide_result %}
            {# nothing #}
        {% elseif not result %}
            {#
    			{% optional include ["blocks/_block_view_",question.type,".tpl"]|join id=id blk=question answers=answers %}
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
