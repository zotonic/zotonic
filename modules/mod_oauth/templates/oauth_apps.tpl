{% extends "admin_base.tpl" %}

{% block title %}{_ OAuth applications _}{% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ Registered OAuth applications _}</h2>
    <p>
        {_ This page allows you to register API keys with which 3rd parties can gain access to specific parts of the API and database. _}
    </p>
</div>

<div class="well">
    {% button class="btn btn-primary" text=_"Add new application" postback="start_add_app" %}
</div>

<div>
    <div id="oauth-apps">
        {% include "_oauth_apps_list.tpl" %}
    </div>
</div>

{% endblock %}

