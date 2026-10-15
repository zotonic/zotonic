{% with m.import_feed_log.list[qfeed][qbefore] as page %}
    {% if page.has_next %}
        {% lazy
            action={insert_bottom target="feed-log-rows" template="_admin_import_feed_log_batch.tpl" qfeed=qfeed qbefore=page.next}
            action={script script="z_import_feed_log_rows();"}
            action={update target="feed-log-more" template="_admin_import_feed_log_loader.tpl" qfeed=qfeed qbefore=page.next}
        %}
    {% endif %}
{% endwith %}
