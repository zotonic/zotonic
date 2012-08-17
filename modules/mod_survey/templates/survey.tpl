{% extends "page.tpl" %}

{% block html_head_extra %}
	{% inherit %}
	{% lib "css/survey.css" %}
{% endblock %}

{% block main %}
<div id="survey-question">
    {% media m.rsc[id].media[1] width=400 height=400 class="main-image" %}

    <p class="summary">{{ m.rsc[id].summary }}</p>
    {{ m.rsc[id].body|show_media }}

	{% include "_survey_start.tpl" %}
</div>
{% endblock %}
