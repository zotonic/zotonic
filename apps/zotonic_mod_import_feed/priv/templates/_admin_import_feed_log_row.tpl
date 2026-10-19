{% if is_live %}
    {% with m.import_feed_log.get[q.payload.log_id] as entry %}
        {% if entry and (qfeed == 'all' or qfeed|to_integer == entry.feed_id) %}
            {% include "_admin_import_feed_log_row_cells.tpl" %}
        {% endif %}
    {% endwith %}
{% else %}
    {% include "_admin_import_feed_log_row_cells.tpl" %}
{% endif %}
