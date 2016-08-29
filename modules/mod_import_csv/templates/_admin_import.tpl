{% extends "admin_base.tpl" %}

{% block title %}{_ Import content _}{% endblock %}

{% block content %}

    <div class="admin-header">
        <h2>{_ Import content _}</h2>
    </div>

    <div class="well">
        {% all include "_admin_import_button.tpl" %}
    </div>

{% endblock %}
