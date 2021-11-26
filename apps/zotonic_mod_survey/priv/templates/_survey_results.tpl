{# Shown after survey - shows thank you and optionally the results #}

{% if inline %}
	{% include "_survey_end.tpl" %}

	<h2>{_ Results _}</h2>
{% endif %}

{% if id.survey_show_results == 2 and not is_aggregated %}

    {# Personal results #}
    {% include "_survey_results_user.tpl" %}

{% elseif id.survey_show_results or m.survey.is_allowed_results_download[id] %}

    {# Aggregated results #}

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

{% if viewer == 'overlay' %}
    {% javascript %}
        if ($(".overlay-content-wrapper").length > 0) {
            $(".overlay-content-wrapper").get(0).scrollTo(0, 0);
        }
    {% endjavascript %}
    <button id="{{ #survey_close }}" class="btn btn-lg btn-default">{_ Close _}</button>
    {% wire id=#survey_close
            action={overlay_close}
    %}
{% elseif viewer == 'dialog' %}
    <button id="{{ #survey_close }}" class="btn btn-lg btn-default">{_ Close _}</button>
    {% wire id=#survey_close
            action={dialog_close}
    %}
{% else %}
    {% javascript %}
        $(window).scrollTop(0);
    {% endjavascript %}
{% endif %}
