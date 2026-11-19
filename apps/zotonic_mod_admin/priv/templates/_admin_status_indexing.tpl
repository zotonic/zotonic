{% with m.admin.pivot_queue_count as queue %}
    <p>{% trans "<strong>{n}</strong> page waiting to be indexed" "<strong>{n}</strong> pages waiting to be indexed" n=queue.backlog|default:0 %}</p>
    <p class="help-block">{_ Search indexing runs separately from the scheduled task queue. _}</p>
    {% if m.acl.use.mod_admin_config and zotonic_dispatch != `admin_maintenance` %}
    <p><a href="{% url admin_maintenance %}#search">{_ Search maintenance _}</a></p>
    {% endif %}
    {% button class="btn btn-default btn-sm" text=_"Refresh" action={update target="admin-status-indexing" template="_admin_status_indexing.tpl"} %}
    <small class="text-muted">{_ Updated _}: {{ now|date:"H:i:s" }}</small>
{% endwith %}
