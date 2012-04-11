{% extends "admin_base.tpl" %}

{% block title %} Development {% endblock %}

{% block content %}
<div class="edit-header">
    <h2>{_ Site Development &mdash; Included templates _}</h2>
        
    <p>{_ Below you see in real time which templates are compiled and included. _}</p>
        
    <div class="well">
        {% button class="btn btn-primary" text=_"Recompile Templates" action={admin_tasks task="templates_reset"} %}
        {% button class="btn" text=_"Empty log" action={update target="dev_templates" text=""} %}
    </div>
    
    <pre id="dev_templates">
    </pre>
    {% wire action={development_templates_stream target="dev_templates"} %}
    
</div>
{% endblock %}
