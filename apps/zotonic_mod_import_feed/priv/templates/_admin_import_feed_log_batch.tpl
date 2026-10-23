{% for entry in m.import_feed_log.list[qfeed][qbefore].items %}
    {% include "_admin_import_feed_log_row.tpl" %}
{% endfor %}
