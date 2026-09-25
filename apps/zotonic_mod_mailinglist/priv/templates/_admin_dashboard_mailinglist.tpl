{% extends "admin_widget_dashboard.tpl" %}

{% block widget_headline %}
    {_ Recent mailings _}
    <a class="btn btn-default btn-xs pull-right" href="{% url admin_mailings %}">{_ All mailings _}</a>
{% endblock %}

{% block widget_content %}
    {% live topic="bridge/origin/model/mailinglist/event/+/runs"
            throttle=3000
            template="_admin_dashboard_mailinglist_runs.tpl"
    %}
{% endblock %}
