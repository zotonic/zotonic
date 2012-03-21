{% extends "admin_base.tpl" %}

{% block head_extra %}
{% lib "css/admin_survey.css" %}
{% endblock %}

{% block title %}{_ Survey editor _} &mdash; {{ m.rsc[q.id].title }}{% endblock %}

{% block content %}
<div class="edit-header">

    <p class="admin-chapeau">{_ survey results editor _}:</p>
    <h2>
        {{ m.rsc[q.id].title }}
        <a class="btn btn-mini" href="{% url admin_edit_rsc id=q.id %}">{_ edit _}</a>
    </h2>

    <hr />
</div>

<div id="survey-results">
    {% include "_admin_survey_editor_results.tpl" %}
</div>
{% endblock %}
