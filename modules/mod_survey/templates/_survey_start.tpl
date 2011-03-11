{% media m.rsc[id].media[1] width=400 height=400 class="main-image" %}

<p class="summary">
	{{ m.rsc[id].summary }}
</p>

{{ m.rsc[id].body|show_media }}

<div class="buttons">
	<button id="{{ #survey_next }}" class="submit next">{_ Start _} &gt;</button>
	{% wire id=#survey_next
		postback={survey_start id=id}
		delegate="mod_survey"
	%}
</div>

{% if id.survey_show_results or m.survey.is_allowed_results_download[id] %}
<div class="buttons">
	<button id="{{ #survey_result }}" class="submit next">{_ Results _} &gt;</button>
	{% wire id=#survey_result
		action={redirect dispatch="survey_results" id=id}
	%}
</div>
{% endif %}
