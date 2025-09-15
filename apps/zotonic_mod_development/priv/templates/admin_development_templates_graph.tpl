{% extends "admin_base.tpl" %}

{% block title %} Development {% endblock %}

{% block content %}
<ul class="breadcrumb">
    <li><a href="{% url admin_development %}">{_ Site Development _}</a></li>
    <li class="active">{_ Dependency graph of all templates _}</li>
</ul>

<div class="admin-header">
    <h2>{_ Dependency graph of all templates _}</h2>
    <p>{_ This calculates and visualizes the dependency graph of all templates. _}</p>
</div>

{% if m.acl.is_allowed.use.mod_development %}
    <div class="well">
        {% button class="btn btn-primary" text=_"Rerun graph"
                  postback={template_graph element_id="graph-results"}
                  delegate=`mod_development`
        %}
    </div>

    {% wire postback={template_graph element_id="graph-results"}
            delegate=`mod_development`
    %}

    <div class="widget">
        <div class="widget-header">
            {_ Dependency graph _}
        </div>
        <div class="widget-content" id="graph-results">
            <p class="text-muted">
                {# Some minimal height for the loading mask to show #}
                <br>
                <br>
            </p>
        </div>
    </div>
{% else %}
    <div class="alert alert-danger">
        {_ You do not have permission to access development tools. _}
    </div>
{% endif %}

{% endblock %}

{% block js_extra %}
    {% lib
        "js/viz.js"
    %}
{% endblock %}
