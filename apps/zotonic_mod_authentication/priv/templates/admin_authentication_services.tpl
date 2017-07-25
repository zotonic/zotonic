{% extends "admin_base.tpl" %}

{% block title %}{_ External Services _}{% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ External Services _}</h2>

    {% if not m.acl.use.mod_admin_config %}
        <p class="alert alert-danger">
            {_ You need to be allowed to edit the system configuration to view or change the configured App Keys. _}
        </p>
    {% else %}
        <p>{_ Here you can set all the application keys, secrets and other configurations to integrate with external services like Facebook and Twitter. _}</p>
    {% endif %}
</div>

<div>
    {% if m.acl.use.mod_admin_config %}
        {% if m.modules.active.mod_signup %}
            <p class="text-muted"><span class="icon-info-sign"></span> {_ The signup module is enabled, users authenticating via the services below are automatically signed up. _}</p>
        {% else %}
            <p class="text-muted"><span class="icon-info-sign"></span> {_ Enable the signup module to automatically sign up users that are authenticating via the services below. _} <a href="{% url admin_modules %}">{_ Go to Modules _}</a></p>
        {% endif %}

        {% all include "_admin_authentication_service.tpl" %}

        {% if not m.modules.active.mod_facebook %}
            <p class="text-muted"><span class="icon-info-sign"></span> {_ Enable <em>mod_facebook</em> to see Facebook settings. _} <a href="{% url admin_modules %}">{_ Go to Modules _}</a></p>
        {% endif %}
        {% if not m.modules.active.mod_twitter %}
            <p class="text-muted"><span class="icon-info-sign"></span> {_ Enable <em>mod_twitter</em> to see Twitter settings. _} <a href="{% url admin_modules %}">{_ Go to Modules _}</a></p>
        {% endif %}
    {% endif %}
</div>
{% endblock %}
