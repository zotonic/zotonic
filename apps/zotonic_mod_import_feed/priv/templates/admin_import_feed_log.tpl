{% extends "admin_base.tpl" %}

{% block title %}{_ Feed activity log _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Feed activity log _}</h2>
        <p><a href="{% url admin_import_feed %}">{_ All feeds _}</a></p>
    </div>
    <form method="get" action="{% url admin_import_feed_log %}" class="form-inline">
        <label for="log-feed">{_ Feed _}</label>
        <select id="log-feed" name="feed" class="form-control">
            <option value="all">{_ All feeds _}</option>
            {% for feed in m.import_feed.list %}
                <option value="{{ feed.id }}" {% if q.feed|to_integer == feed.id %}selected{% endif %}>{{ feed.title|escape }}</option>
            {% endfor %}
        </select>
        <button class="btn btn-primary" type="submit">{_ Filter _}</button>
    </form>
    <p class="help-block">{_ New events appear live at the top. Scroll down to load older events. Logs are kept for 30 days. _}</p>
    <div id="feed-log-entries">
        {% include "_admin_import_feed_log.tpl" qfeed=q.feed|default:"all" %}
    </div>
        {% lib "js/admin_import_feed_log.js" %}
        {% wire type={mqtt topic="bridge/origin/model/import_feed/event/log"}
            action={insert_top target="feed-log-rows" template="_admin_import_feed_log_row.tpl" is_live qfeed=q.feed|default:"all"}
            action={script script="z_import_feed_log_rows();"}
            action={update target="feed-log-controls" template="_admin_import_feed_log_controls.tpl" qfeed=q.feed|default:"all"}
        %}
{% endblock %}
