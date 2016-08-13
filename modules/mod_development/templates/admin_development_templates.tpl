{% extends "admin_base.tpl" %}

{% block title %} Development {% endblock %}

{% block content %}
<ul class="breadcrumb">
    <li><a href="{% url admin_development %}">{_ Site Development _}</a></li>
    <li class="active">{_ Included templates _}</li>
</ul>

<div class="admin-header">
    <h2>{_ Included templates _}</h2>
    <p>{_ Below you see in real time which templates are compiled and included. _}</p>
</div>

<div class="well">
    {% button class="btn btn-primary" text=_"Recompile Templates" action={admin_tasks task="templates_reset"} %}
    {% button class="btn btn-default" text=_"Empty log" action={update target="dev_templates" text=""} %}
</div>

<div>
    <pre id="dev_templates"></pre>
    {% wire action={development_templates_stream target="dev_templates"} %}
</div>
{% endblock %}
