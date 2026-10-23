    <tr id="feed-log-row-{{ entry.id }}" data-log-id="{{ entry.id }}" {% if entry.status == 'error' %}class="danger"{% endif %}>
        <td class="text-nowrap">{{ entry.created|date:"Y-m-d H:i:s" }}</td>
        <td><a href="{% url admin_import_feed_log feed=entry.feed_id %}">{{ entry.feed_title|escape }} ({{ entry.feed_id }})</a></td>
        <td>{{ entry.event|escape }}<br>{{ entry.status|escape }}</td>
        <td>
            {% if entry.rsc_id %}
                <a href="{% url admin_edit_rsc id=entry.rsc_id %}">{{ entry.rsc_id.title|default:entry.rsc_id }} ({{ entry.rsc_id }})</a>
            {% endif %}
            {% if entry.uri %}<div class="text-muted">{{ entry.uri|escape }}</div>{% endif %}
        </td>
        <td>{% include "_admin_import_feed_log_resource.tpl" id=entry.user_id %}</td>
        <td>{% include "_admin_import_feed_log_resource.tpl" id=entry.category_id %}</td>
        <td>{% include "_admin_import_feed_log_resource.tpl" id=entry.content_group_id %}</td>
        <td>{{ entry.message|escape|linebreaksbr }}</td>
    </tr>
