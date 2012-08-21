{% extends "page.tpl" %}

{% block html_head_extra %}
	{% inherit %}
	{% lib "css/survey.css" %}
{% endblock %}

{% block content_attributes %}{% include "_language_attrs.tpl" id=id %} id="survey-question"{% endblock %}

{% block body %}
	{{ id.body|show_media }}
{% endblock %}

{% block below_body %}
	{% include "_survey_start.tpl" %}
	{% inherit %}
{% endblock %}
