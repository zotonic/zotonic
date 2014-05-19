{% extends "admin_base.tpl" %}

{% block title %}{_ L10N Configuration _}{% endblock %}

{% block content %}

<div class="admin-header">

    <h2>{_ Localization _}</h2>

    <p>{_ Here you can set the default localization configurations. _}</p>

    <div class="row-fluid">
        <div class="widget span6">
            <h3 class="widget-header">{_ Localization _}</h3>
            <div class="widget-content">
                {% include "_admin_l10n_config.tpl" %}
            </div>
        </div>
    </div>
</div>


{% endblock %}
