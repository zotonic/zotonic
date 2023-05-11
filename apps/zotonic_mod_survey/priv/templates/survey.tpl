{% extends "page.tpl" %}

{% block html_head_extra %}
	{% inherit %}
	{% lib "css/survey.css" %}
{% endblock %}

{% block content_area %}
    <div id="survey-question">
        {% inherit %}
    </div>
{% endblock %}

{% block body %}
	{% if not id.survey_is_autostart or id.survey_is_disabled %}
		{{ id.body|show_media }}
	{% endif %}
{% endblock %}

{% block below_body %}

    {% if id.survey_is_disabled %}
        <p><em>{_ Closed _}</em></p>
        {% if id.is_editable %}
            <p><b>{_ Because you can edit, you can proceed for testing. _}</b></p>
        {% endif %}
    {% endif %}

    {% if id.is_a.survey and (not id.survey_is_disabled or id.is_editable) %}
    	{% lazy template="_survey_start.tpl"
                id=id
                answers=answers
                element_id=element_id|default:"survey-question"
        %}
    {% endif %}

    {% inherit %}

    <div>
        {{ id.body_extra|show_media }}
    </div>
{% endblock %}
