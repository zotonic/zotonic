<table class="table">
    <thead><tr><th>{_ Mailing _}</th><th>{_ Status and progress _}</th></tr></thead>
    <tbody>
        {% for run in m.mailinglist_run.recent %}
            <tr>
                <td>
                    <a href="{% url admin_mailing_run run_id=run.id %}">{{ m.rsc[run.page_id].title|default:_"Untitled" }}</a>
                    {% if run.is_test %}<span class="label label-info">{_ Test _}</span>{% endif %}
                    <br>{{ m.rsc[run.mailinglist_id].title }}
                    <br><small>{{ run.created|date:"Y-m-d H:i" }} ·
                        {% if run.language %}{{ m.translation.language_list_configured[run.language].name|default:run.language|escape }}{% else %}{_ Recipient preference _}{% endif %}
                    </small>
                </td>
                <td>
                    <a href="{% url admin_mailing_run run_id=run.id %}">{% include "_mailing_run_status.tpl" status=run.status type=run.type due=run.due %}</a>
                    {% if run.status == "scheduled" and run.type == "date" %}<br>{{ run.due|date:"Y-m-d H:i" }}{% endif %}
                    {% include "_mailing_run_counts.tpl" stats=run.stats %}
                </td>
            </tr>
        {% empty %}
            <tr><td colspan="2">{_ No mailings yet. Send a mailing from a page’s edit screen to see its progress here. _}</td></tr>
        {% endfor %}
    </tbody>
</table>
