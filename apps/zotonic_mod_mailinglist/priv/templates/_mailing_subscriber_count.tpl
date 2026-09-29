{% with m.mailinglist.stats[list_id] as stats %}
    <strong>{{ stats.total }}</strong>
    {% if m.rsc[list_id].query %}
        <span class="text-muted">({% trans "{count} from the list query" count=stats.query_text %})</span>
    {% endif %}
{% endwith %}
