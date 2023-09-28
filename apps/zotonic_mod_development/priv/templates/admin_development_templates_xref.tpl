{% extends "admin_base.tpl" %}

{% block title %} Development {% endblock %}

{% block content %}
<ul class="breadcrumb">
    <li><a href="{% url admin_development %}">{_ Site Development _}</a></li>
    <li class="active">{_ Templates XRef check _}</li>
</ul>

<div class="admin-header">
    <h2>{_ Cross reference check of all templates _}</h2>
    <p>{_ This checks all templates to see if included of extended templates are available. _}</p>
</div>

<div class="well">
    {% button class="btn btn-primary" text=_"Rerun XRef"
              postback={template_xref_check element_id="xref-results"}
              delegate=`mod_development`
    %}
</div>

{% wire postback={template_xref_check element_id="xref-results"}
        delegate=`mod_development`
%}

<div class="widget">
    <div class="widget-header">
        {_ Template check results _}
    </div>
    <div class="widget-content" id="xref-results">
        <p class="text-muted">
            <img src="/lib/images/spinner.gif" height="16" width="16">
            {_ Checking all templates... _}
        </p>
    </div>
</div>

{% endblock %}
