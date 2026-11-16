{% if m.mailinglist_run.run[run_id] as run %}
    {% if run.details_expired %}
        <p class="alert alert-info">{_ Recipient details have expired after three months. Aggregate statistics are still available above. _}</p>
    {% else %}
        {% with m.mailinglist_run.recipients_page[run.id]::%{status:recipient_status, email:recipient_email, page:recipient_page} as recipients %}
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
                    {% for recipient in recipients.result %}
                        <tr>
                            <td><a href="{% url admin_log_email severity=4 to=recipient.email %}"
                                   target="_blank" rel="noopener noreferrer"
                                   title="{_ Open email log in a new window _}">{{ recipient.email|escape }}</a></td>
                            <td>{{ recipient.language|escape }}</td>
                            <td>{% include "_mailing_recipient_status.tpl" status=recipient.status %}</td>
                            <td>{% include "_mailing_skip_reason.tpl" reason=recipient.reason|mailinglist_delivery_detail %}</td>
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
            <nav aria-label="{_ Recipient pages _}">
                <p class="text-muted">
                    {% trans "Page {page} of {pages} ({total} recipients)" page=recipients.page pages=recipients.pages total=recipients.total %}
                </p>
                <ul class="pagination">
                    {% if recipients.page > 1 %}
                        <li><a href="{% url admin_mailing_run run_id=run.id recipient_status=recipient_status recipient_email=recipient_email recipient_page=1 %}">{_ First _}</a></li>
                        <li><a rel="prev" href="{% url admin_mailing_run run_id=run.id recipient_status=recipient_status recipient_email=recipient_email recipient_page=recipients.page-1 %}">{_ Previous _}</a></li>
                    {% else %}
                        <li class="disabled"><span aria-disabled="true">{_ First _}</span></li>
                        <li class="disabled"><span aria-disabled="true">{_ Previous _}</span></li>
                    {% endif %}
                    {% if recipients.page < recipients.pages %}
                        <li><a rel="next" href="{% url admin_mailing_run run_id=run.id recipient_status=recipient_status recipient_email=recipient_email recipient_page=recipients.page+1 %}">{_ Next _}</a></li>
                        <li><a href="{% url admin_mailing_run run_id=run.id recipient_status=recipient_status recipient_email=recipient_email recipient_page=recipients.pages %}">{_ Last _}</a></li>
                    {% else %}
                        <li class="disabled"><span aria-disabled="true">{_ Next _}</span></li>
                        <li class="disabled"><span aria-disabled="true">{_ Last _}</span></li>
                    {% endif %}
                </ul>
            </nav>
        {% endwith %}
    {% endif %}
{% endif %}
