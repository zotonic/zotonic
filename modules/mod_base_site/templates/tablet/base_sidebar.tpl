{% extends "base.tpl" %}

{% block content %}
{% block above %}{% endblock %}
<div class="row-fluid">
    <div class="span9">
        {% block main %}{% endblock %}
    </div>
    
    <div class="span3">
        {% block sidebar %}{% endblock %}
    </div>
</div>
{% block below %}{% endblock %}
{% endblock %}
