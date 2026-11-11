{% extends "admin_base.tpl" %}
{% block title %}{_ Site Analytics _}{% endblock %}
{% block head_extra %}{% lib "css/analytics.css" %}{% endblock %}
{% block content %}
{% if m.acl.use.mod_analytics %}
{% with analytics|default:m.analytics.overview as a %}
<div class="admin-header analytics-heading">
    <div><h2>{_ Site Analytics _}</h2><p>{_ Your site at a glance. _}</p></div>
    {% if m.acl.is_admin %}<a class="btn btn-default" href="{% url admin_analytics_config %}">{_ Configure analytics measurements _}</a>{% endif %}
</div>
<form method="get" action="{% url admin_analytics %}" class="analytics-period">
    <span>{_ Period _}</span>
    {% for value, label in [["hour", _"Last hour"], ["day", _"Last six hours"], ["24hours", _"Last 24 hours"], ["week", _"Past 7 days"], ["month", _"Past 30 days"], ["year", _"Past 365 days"], ["years", _"Last five years"]] %}
        <button name="period" value="{{ value }}" class="btn {% if value == a.period %}btn-primary{% else %}btn-default{% endif %}" {% if value == a.period %}aria-current="true"{% endif %}>{{ label|escape }}</button>
    {% endfor %}
    <span class="text-muted">{{ m.req.timezone|escape }}</span>
</form>
<div class="analytics-grid">
    {% for panel in a.panels %}
        {% include "_admin_analytics_panel.tpl" panel=panel lead=forloop.first %}
    {% empty %}
        <p>{_ No analytics measurements to display. Configure or resume an analytics measurement to get started. _}</p>
    {% endfor %}
</div>
<p class="text-muted analytics-note">{_ Gaps are unrecorded intervals. Partial intervals may undercount activity. Page-request totals, device breakdowns and popular content exclude detected crawlers and agents unless explicitly selected. API and traffic-classification measurements include them unless filtered out. Counts exclude editor activity and do not measure unique visitors. _}</p>
{% endwith %}
{% endif %}
{% endblock %}
