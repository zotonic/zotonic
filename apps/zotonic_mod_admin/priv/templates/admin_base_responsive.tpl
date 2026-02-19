{% extends "admin_base.tpl" %}

{% block bodyclass %}admin-responsive{% endblock %}

{% block navigation %}
    {% include "_admin_menu.tpl" %}
{% endblock %}

{% block container %}
<div class="admin-container admin-responsive-container" id="admin-container">
    <div class="content-title">{% block content_title %}{% endblock %}</div>
    <div class="content">{% block content %}{% endblock %}</div>
</div>
{% endblock %}

{% block footer %}
{% endblock %}
