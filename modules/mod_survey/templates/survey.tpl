{% extends "page.tpl" %}

{% block content %}


<style type="text/css">
	.survey-question {
		border: 1px solid white;
		background-color: #f8f8f8;
		padding: 4px;
		margin-bottom: 4px;
	}

	.survey-missing {
		background-color: #fee;
		border: 1px solid #fcc;
	}
</style>

{% wire id=#survey type="submit" postback={survey_submit id=id} delegate="mod_survey" %}
<form id="{{ #survey }}" class="survey" method="POST" action="postback">
	<p>
		{{ m.rsc[id].summary }}
	</p>

	{% for q_id, q in m.survey.questions[id] %}
	<div id="{{ q_id }}" class="survey-question clear-fix">
		{{ q.html }}
	</div>
	{% empty %}
		<p>{_ Sorry, this survey doesn't have any questions. _}</p>
	{% endfor %}

	<button class="survey-submit">{_ Ready _}</button>
</form>

<div id="{{ #survey }}-success" style="display: none">
	{{ m.rsc[id].body|show_media }}
</div>

{% endblock %}
