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
        {% if    id.survey_multiple
              or m.acl.user
              or m.req.session_id
        %}
            {% include "_survey_start.tpl"
                    id=id
                    answers=answers
                    is_autostart=(q.autostart or id.survey_is_autostart)
                    element_id=element_id|default:"survey-question"
            %}
        {% else %}
            {# Lazy load to ensure that the unique cotonic-sid for the browser is set. #}
            {# On first page loads for new visitors the cotonic-sid is not yet set.    #}
            {% lazy template="_survey_start.tpl"
                    id=id
                    answers=answers
                    is_autostart=(q.autostart or id.survey_is_autostart)
                    element_id=element_id|default:"survey-question"
            %}
        {% endif %}
    {% endif %}

    {% inherit %}

    <div>
        {{ id.body_extra|show_media }}
    </div>
{% endblock %}
