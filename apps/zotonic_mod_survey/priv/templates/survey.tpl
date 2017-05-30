{% extends "page.tpl" %}

{% block html_head_extra %}
	{% inherit %}
	{% lib "css/survey.css" %}
{% endblock %}

{% block content_attributes %}{% include "_language_attrs.tpl" id=id class="wrapper" %} id="survey-question"{% endblock %}

{% block body %}
	{% if not id.survey_is_autostart %}
		{{ id.body|show_media }}
	{% endif %}
{% endblock %}

{% block below_body %}
	{% include "_survey_start.tpl" %}
	{% inherit %}
{% endblock %}
