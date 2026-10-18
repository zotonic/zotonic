<div id="feed-log-controls">{% include "_admin_import_feed_log_controls.tpl" %}</div>
{% with m.import_feed_log.list[qfeed][0] as history %}
    <div class="table-responsive">
        <table class="table table-striped table-condensed">
            <thead>
                <tr>
                    <th>{_ Time _}</th>
                    <th>{_ Feed _}</th>
                    <th>{_ Event / result _}</th>
                    <th>{_ Resource _}</th>
                    <th>{_ User _}</th>
                    <th>{_ Category _}</th>
                    <th>{_ Content group _}</th>
                    <th>{_ Details _}</th>
                </tr>
            </thead>
            <tbody id="feed-log-rows">
                {% for entry in history.items %}
                    {% include "_admin_import_feed_log_row.tpl" %}
                {% empty %}
                    <tr id="feed-log-empty"><td colspan="8">{_ No activity for this feed, or access denied. _}</td></tr>
                {% endfor %}
            </tbody>
        </table>
    </div>
    <div id="feed-log-more">
        {% include "_admin_import_feed_log_loader.tpl" qbefore=0 %}
    </div>
{% endwith %}
