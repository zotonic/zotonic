{% extends "admin_base.tpl" %}
{% block title %}{_ Deployment _}{% endblock %}
{% block content %}
<div class="admin-header">
    <h2>{_ Deployment _}</h2>
    <p>{_ Review incoming changes and deploy this site's code. Compilation and activation are coordinated across all sites on this node. _}</p>
</div>
{% include "_admin_status_update.tpl" %}
{% compose "_admin_maintenance_panel.tpl" title=_"Site deployment" %}
    {% block tools %}
        <p>{_ The deployment preview shows the current revision and incoming commits. No checked-out files change until you confirm Deploy. _}</p>
        {% include "_admin_deployment_tools.tpl" %}
        {% if m.acl.use.mod_admin_modules %}
            <p>{_ To deploy updates for individual modules, open the _} <a href="{% url admin_modules %}">{_ Modules list _}</a>.</p>
        {% endif %}
        <p><a href="{% url admin_status %}">{_ View system status _}</a></p>
    {% endblock %}
{% endcompose %}
{% endblock %}
