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

<div id="survey-question">
	{% include "_survey_start.tpl" %}
</div>

{% endblock %}
