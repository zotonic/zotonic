{% extends "admin_base.tpl" %}

{% block title %}{_ Dashboard _}{% endblock %}

{% block content %}
    <div>
        <div class="pull-right">
            <p class="admin-chapeau">
                {_ Logged in as _}
                <a href="{% url admin_edit_rsc id=m.acl.user %}">{{ m.acl.user.title }}</a>
            </p>
        </div>
        <div class="admin-header">
            <h2>{_ Dashboard _}</h2>
        </div>
        {% include "_admin_dashboard_buttons.tpl" %}
    </div>
    {% include "_admin_dashboard.tpl" %}
{% endblock %}