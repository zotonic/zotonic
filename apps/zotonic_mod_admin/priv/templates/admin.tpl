{% extends "admin_base.tpl" %}

{% block bodyclass %}dashboard{% endblock %}
{% block title %}{_ Dashboard _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Dashboard _}</h2>
    </div>

    {% if m.admin_status.disks.alert %}
        <div class="alert alert-danger" role="alert">
            <span class="fa fa-warning"></span>
            <b>{_ Warning! _}</b>
            {_ Some disks are almost full. _}

            &nbsp; <a class="btn btn-danger btn-xs" href="{% url admin_status %}">{_ View status _}</a>
        </div>
    {% endif %}

    {% include "_admin_dashboard_buttons.tpl" %}
    {% include "_admin_dashboard.tpl" %}
{% endblock %}
