{% extends "admin_edit_widget_std.tpl" %}

{% block widget_id %}sidebar-websub{% endblock %}
{% block widget_title %}{_ WebSub subscriptions _}<div class="widget-header-tools"></div>{% endblock %}

{% block widget_content %}
{% if id.is_editable %}
    {% with m.websub.subscriber_count[id] as count %}
        <p>
            {% if count %}
                {% trans "Active subscriptions to this page: {count}" count=count %}
            {% else %}
                {_ No active subscriptions to this page. _}
            {% endif %}
        </p>
        {% if m.acl.use.mod_admin_config %}
            <p><a href="{% url admin_websub type="export" rsc_id=id %}">{_ View subscribers to this page _}</a></p>
            {% if not id.is_authoritative %}
                <p><a href="{% url admin_websub type="import" rsc_id=id %}">{_ View subscriptions to the external page _}</a></p>
            {% endif %}
        {% endif %}
    {% endwith %}
{% endif %}
{% endblock %}
