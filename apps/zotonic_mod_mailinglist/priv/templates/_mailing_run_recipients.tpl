{% if m.mailinglist_run.run[run_id] as run %}
    {% if run.details_expired %}
        <p class="alert alert-info">{_ Recipient details have expired after three months. Aggregate statistics are still available above. _}</p>
    {% else %}
        {% with m.mailinglist_run.recipients[run.id]::%{status:recipient_status, after:after} as recipients %}
            <table class="table table-striped">
                <thead>
                    <tr>
                        <th>{_ Email _}</th>
                        <th>{_ Language _}</th>
                        <th>{_ Status _}</th>
                        <th>{_ Details _}</th>
                    </tr>
                </thead>
                <tbody>
                    {% for recipient in recipients %}
                        <tr>
                            <td>{{ recipient.email|escape }}</td>
                            <td>{{ recipient.language|escape }}</td>
                            <td>{% include "_mailing_recipient_status.tpl" status=recipient.status %}</td>
                            <td>{% include "_mailing_skip_reason.tpl" reason=recipient.reason %}</td>
                        </tr>
                    {% empty %}
                        <tr>
                            <td colspan="4">
                                <span class="text-muted">
                                    {% if run.prepared %}{_ No recipients match this filter. _}
                                    {% else %}{_ Recipients will appear when preparation starts. _}
                                    {% endif %}
                                </span>
                            </td>
                        </tr>
                    {% endfor %}
                </tbody>
            </table>
            {% if recipients|length == 100 %}
                {% with recipients|last as last_recipient %}
                        <a href="{% url admin_mailing_run run_id=run.id after=last_recipient.id recipient_status=recipient_status %}">{_ Next 100 recipients _}</a>
                {% endwith %}
            {% endif %}
            {% if after %}
                <a href="{% url admin_mailing_run run_id=run.id recipient_status=recipient_status %}">{_ First page _}</a>
            {% endif %}
        {% endwith %}
    {% endif %}
{% endif %}
