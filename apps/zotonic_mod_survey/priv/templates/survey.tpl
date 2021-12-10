{% extends "page.tpl" %}

{% block html_head_extra %}
	{% inherit %}
	{% lib "css/survey.css" %}
{% endblock %}

{% block content_attributes %}{% include "_language_attrs.tpl" id=id class="wrapper" %} id="survey-question"{% endblock %}

{% block body %}
	{% if not id.survey_is_autostart or id.survey_is_disabled %}
		{{ id.body|show_media }}
	{% endif %}
{% endblock %}

{% block below_body %}
    {% if id.is_a.survey and not id.survey_is_disabled %}
    	{% lazy template="_survey_start.tpl" id=id %}
    {% endif %}

    {% inherit %}

    <div>
        {{ id.body_extra|show_media }}
    </div>
{% endblock %}
