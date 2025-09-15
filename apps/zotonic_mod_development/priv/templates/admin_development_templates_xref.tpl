{% extends "admin_base.tpl" %}

{% block title %} Development {% endblock %}

{% block content %}
<ul class="breadcrumb">
    <li><a href="{% url admin_development %}">{_ Site Development _}</a></li>
    <li class="active">{_ Cross-reference check of all templates _}</li>
</ul>

<div class="admin-header">
    <h2>{_ Cross-reference check of all templates _}</h2>
    <p>{_ This checks all templates to see if included or extended templates are available. _}</p>
</div>

{% if m.acl.is_allowed.use.mod_development %}
    <div class="well">
        {% button class="btn btn-primary" text=_"Refresh"
                  postback={template_xref_check element_id="xref-results"}
                  delegate=`mod_development`
        %}
    </div>

    {% wire postback={template_xref_check element_id="xref-results"}
            delegate=`mod_development`
    %}

    <div class="widget">
        <div class="widget-header">
            {_ Cross-reference check results _}
        </div>
        <div class="widget-content" id="xref-results">
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
