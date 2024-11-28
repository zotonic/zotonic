{% extends "_stats_panel.tpl" %}

{% block panel_title %}{_ Files Cache _}{% endblock %}

{% block panel_body %}
<table class="table table-condensed">
    <thead></thead>
    <tbody>
        {% for title, id, format in [
                ["Bytes", "bytes", "filesize"],
                ["Max Bytes", "max_bytes", "filesize"],
                ["Files", "entries", ""],
                ["Inserts", "insert_count", ""],
                ["Deletes", "delete_count", ""],
                ["Hits", "hit_count", ""],
                ["Misses", "miss_count", ""],
                ["Evictions", "evict_count", ""]
            ]
        %}
            {% include "_stat_row.tpl" id=["filezcache-", id] %}
        {% endfor %}
    </tbody>
</table>
{% endblock %}
