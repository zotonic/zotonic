{% extends "admin_base.tpl" %}

{% block title %}{_ Log _}{% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ Logging _}</h2>
</div>

<ul class="nav nav-tabs">
    <li class="{% block active_log %}{% endblock %}"><a href="{% url admin_log %}">{_ Message log _}</a></li>
    <li class="{% block active_email %}{% endblock %}"><a href="{% url admin_log_email %}">{_ Email log _}</a></li>
    <li class="{% block active_csp %}{% endblock %}"><a href="{% url admin_log_csp %}">{_ Content-Security log _}</a></li>
    <li class="{% block active_ui %}{% endblock %}"><a href="{% url admin_log_ui %}">{_ User interface log _}</a></li>
</ul>
<div class="well">
{% block content_log %}
{% endblock %}
</div>

{% endblock %}
