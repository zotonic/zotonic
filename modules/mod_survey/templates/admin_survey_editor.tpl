{% extends "admin_base.tpl" %}

{% block head_extra %}
{% lib "css/admin_survey.css" %}
{% endblock %}

{% block title %}{_ Survey editor _} &mdash; {{ m.rsc[q.id].title }}{% endblock %}

{% block content %}
{% with m.rsc[q.id].id as id %}
    <div class="edit-header">

        <p class="admin-chapeau">{_ survey results editor _}:</p>
        <h2>
            {{ id.title }}
            <a class="btn btn-mini" href="{% url admin_edit_rsc id=id %}">{_ edit _}</a>
        </h2>

        <hr />
    </div>

    <div id="survey-results">
        {% if id.is_editable %}
            {% include "_admin_survey_editor_results.tpl" %}
        {% else %}
            <p class="alert alert-error">
                {_ You must have edit permission on the survey to edit its results. _}
            </p>
        {% endif %}
    </div>
{% endwith %}
{% endblock %}
