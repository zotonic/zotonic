{% with m.mailinglist.stats[list_id] as stats %}
    <strong>{{ stats.total|format_number }}</strong> {_ enabled _}
    <small class="text-muted">({{ stats.disabled|format_number }} {_ disabled _})</small>
    {% if m.rsc[list_id].query %}
        <span class="text-muted">({% trans "{count} from the list query" count=stats.query_text %})</span>
    {% endif %}
{% endwith %}
