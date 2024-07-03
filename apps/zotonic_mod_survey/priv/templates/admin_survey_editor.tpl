{% extends "admin_base.tpl" %}

{% block head_extra %}
    {% lib "css/admin_survey.css"
           "css/survey.css"
    %}
{% endblock %}

{% block title %}{_ Survey editor _} &mdash; {{ m.rsc[q.id].title }}{% endblock %}

{% block content %}
{% with m.rsc[q.id].id as id %}
    <ul class="breadcrumb">
        <li><a href="{% url admin_edit_rsc id=id %}">{{ id.title }}</a></li>
        <li class="active">{_ Results editor _}</li>
    </ul>

    <div id="survey-results">
        {% if id.is_editable %}
            {% include "_admin_survey_editor_results.tpl" %}
        {% else %}
            <p class="alert alert-danger">
                {_ You must be allowed to edit the form, to edit its results. _}
            </p>
        {% endif %}
    </div>
{% endwith %}
{% endblock %}

{% block js_extra %}
    {% lib "js/modules/z.survey_test_feedback.js" %}
{% endblock %}
