{% extends "admin_base.tpl" %}
{% block title %}{_ System status _}{% endblock %}
{% block content %}
<div class="admin-header">
    <h2>{_ System status _}</h2>
    <p>{_ Health, background work and services for _} <strong>{{ m.site.site|escape }}</strong>.</p>
</div>
{% include "_admin_system_navigation.tpl" %}
{% include "_admin_status_alert.tpl" %}
{% include "_admin_status_update.tpl" %}

{% if m.acl.use.mod_admin_config %}
    <section class="widget">
        <h3 class="widget-header">{_ Media maintenance _}</h3>
        <div class="widget-content" id="admin-status-media-maintenance" aria-live="polite">
            {% include "_admin_status_media_maintenance.tpl" %}
        </div>
    </section>
{% endif %}
<h3>{_ Background work _} <small>{_ This site _}</small></h3>
<div class="row">
    <div class="col-md-4">
        <section class="widget">
            <h3 class="widget-header">{_ Search indexing _}</h3>
            <div class="widget-content" id="admin-status-indexing" aria-live="polite">
                {% include "_admin_status_indexing.tpl" %}
            </div>
        </section>
    </div>
    <div class="col-md-8">
        <section class="widget">
            <h3 class="widget-header">{_ Scheduled tasks _}</h3>
            <div class="widget-content">
                {% if m.acl.is_admin %}
                    {% live topic="bridge/origin/$SYS/site/"++m.site.site++"/task-queue" template="_admin_status_task_queue.tpl" %}
                    <p class="help-block">{_ Tasks with errors retry up to 10 times before being dropped. _}</p>
                {% else %}
                    <p>{_ Task details are available to system administrators. _}</p>
                {% endif %}
            </div>
        </section>
    </div>
</div>
{% if m.acl.is_admin %}
    <h3>{_ Services _} <small>{_ All sites on this node _}</small></h3>
    {% include "_admin_status_media.tpl" %}
    <details class="widget">
        <summary class="widget-header">{_ Network listeners and connections _}</summary>
        <div class="widget-content">
            <div id="admin-status-listeners" aria-live="polite">{% include "_admin_status_listeners.tpl" %}</div>
            {% include "_admin_status_connections.tpl" %}
        </div>
    </details>
    <h3>{_ Resources _}</h3>
    {% include "_admin_status_resources.tpl" %}
{% endif %}
{% all include "_admin_status_panel.tpl" %}
{# Compatibility: external modules can still contribute complete status widgets. #}
{% all include "_admin_system_status.tpl" %}
<section class="widget">
    <h3 class="widget-header">{_ Additional module tools _}</h3>
    <div class="widget-content">
        {# Compatibility: keep the existing HTML/wire context for external buttons. #}
        {% all include "_admin_status.tpl" %}
        {% if m.acl.use.mod_admin_config %}<p class="help-block"><a href="{% url admin_maintenance %}">{_ Open maintenance tools _}</a></p>{% endif %}
    </div>
</section>
{% endblock %}
