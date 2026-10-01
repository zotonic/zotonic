{% with m.mailinglist.stats[id] as stats %}
    <p>{_ Number of enabled recipients on this list: _} <b>{{ stats.total|format_number }}</b></p>
    <table class="table admin-table">
        <tr>
            <th>{_ Email subscriptions _}</th>
            <th>{_ Subscriber Edges _}</th>
            <th>{_ Matched via Query _}</th>
        </tr>
        <tr>
            <td>
                <strong>{{ stats.recipients|format_number }}</strong> {_ enabled _}
                <br><small class="text-muted">{{ stats.disabled|format_number }} {_ disabled _}</small>
            </td>
            <td>
                <a href="{% url admin_edges qhasobject=id qpredicate=`subscriberof` %}">
                    {{ stats.subscriberof|format_number }} {_ pages _}
                </a>
            </td>
            <td>
                <a href="{% url admin_overview_rsc qquery_id=id %}">
                    {{ stats.query_text|format_number }} {_ pages _}
                </a>
            </td>
        </tr>
    </table>
    <p class="help-block">{_ Disabled email subscriptions are automatically deleted after three months. _}</p>
{% endwith %}
