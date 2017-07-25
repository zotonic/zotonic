{% extends "admin_base.tpl" %}

{% block head_extra %}
{% lib "css/admin_survey.css" %}
{% endblock %}

{% block title %}{_ Survey editor _} &mdash; {{ m.rsc[q.id].title }}{% endblock %}

{% block content %}
<ul class="breadcrumb">
    <li><a href="{% url admin_edit_rsc id=q.id %}">{{ m.rsc[q.id].title }}</a></li>
    <li class="active">{_ Survey Results Editor _}</li>
</ul>

<div class="admin-header">
    <h2>
        {_ Survey Results Editor _}
    </h2>
</div>

<div id="survey-results">
    {% include "_admin_survey_editor_results.tpl" %}
</div>
{% endblock %}
