{% extends "admin_base.tpl" %}

{% block bodyclass %}dashboard{% endblock %}
{% block title %}{_ Dashboard _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Dashboard _}</h2>
    </div>

    {% include "_admin_status_alert.tpl" %}
    {% include "_admin_dashboard_buttons.tpl" %}
    {% include "_admin_dashboard.tpl" %}
{% endblock %}
