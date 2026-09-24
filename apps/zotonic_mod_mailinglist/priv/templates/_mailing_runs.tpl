<table class="table table-striped">
    <thead><tr><th>{_ Page _}</th><th>{_ Mailing list _}</th><th>{_ Language _}</th><th>{_ Status _}</th><th>{_ Results _}</th></tr></thead>
    <tbody>
    {% for run in runs %}
        <tr>
            <td><a href="{% url admin_mailing_run run_id=run.id %}">{{ m.rsc[run.page_id].title }}</a>
                {% if run.is_test %}<small>{_ Test _}</small>{% endif %}
                <br><small>{{ run.created|date:"Y-m-d H:i" }}</small></td>
            <td><a href="{% url admin_edit_rsc id=run.mailinglist_id %}">{{ m.rsc[run.mailinglist_id].title }}</a></td>
            <td>{% if run.language %}{{ m.translation.language_list_configured[run.language].name|default:run.language|escape }}{% else %}{_ Recipient preference _}{% endif %}</td>
            <td><a href="{% url admin_mailing_run run_id=run.id %}">{% include "_mailing_run_status.tpl" status=run.status type=run.type due=run.due %}</a>
                {% if run.status == "scheduled" and run.type == "date" %}<br>{{ run.due|date:"Y-m-d H:i" }}{% endif %}</td>
            <td>{% include "_mailing_run_counts.tpl" stats=run.stats %}</td>
        </tr>
    {% empty %}<tr><td colspan="5">{_ No tracked mailings yet. History before run tracking was enabled is not available here. _}</td></tr>
    {% endfor %}
    </tbody>
</table>
